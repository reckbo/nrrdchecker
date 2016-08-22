{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE ExistentialQuantification #-}

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Map               as M
import           Data.Maybe
import           System.Console.CmdArgs
import           Text.RawString.QQ
import           Text.Trifecta
import Text.Printf
import System.IO (hPutStrLn,stderr)

-------------------------------------------------------------
-- Data types

-- Basic field specifications
-- data DataType
--   = UInt1 | Int1 | UInt2 | Int2 | UInt4 | Int4 | UInt8 | Int8 | Float4 | Float8
--   deriving (Show, Eq)
type DataType
  = String

type Encoding
  = String

-- Space and orientation
data Space
  = LPS | RPS | RAS | LAS
  deriving (Show, Eq)

type Tuple3
  = (Double, Double, Double)

data SpaceDirections
  = StructuralSpace Tuple3 Tuple3 Tuple3
  | DWISpace Tuple3 Tuple3 Tuple3
  deriving (Eq, Show)

type SpaceOrigin
  = Tuple3

type Dimension
  = Int

-- Per axis specifications
type Sizes = [Integer]
type Kinds = [String]

-- Key Value types

type Key
  = String

data Value
  = VDataType DataType  -- String for now
  | VDimension Dimension -- Int
  | VSpace Space
  | VSizes Sizes
  | VSpaceDirections SpaceDirections
  | VKinds Kinds
  | VEndian String
  | VEncoding Encoding
  | VSpaceOrigin SpaceOrigin
  | VDefault String
  deriving (Show, Eq)

-------------------------------------------------------------
-- Parsers

parseEncoding :: Parser String
parseEncoding = try (string "gzip") <|> try (string "raw")

skipEOL :: Parser ()
skipEOL = (skipMany $ char ' ')  <* char '\n'

skipComments :: Parser [String]
skipComments = many (char '#' *> manyTill anyChar newline)

skipNrrdMagic :: Parser ()
skipNrrdMagic = void $ string "NRRD000" *> anyChar *> newline

parseKey :: Parser String
parseKey = try twoWords <|> try oneWord
  where
    oneWord = some letter
    twoWords = do
          w1 <- oneWord
          char ' '
          w2 <- oneWord
          return $ w1++" "++w2

eol :: Parser ()
eol = choice $ map (try . (spaces *>)) [void newline, eof]
  where
    spaces = many $ char ' '

parseKVP :: Parser (Key, Value)
parseKVP = do
  key <- parseKey
  token $ char ':'
  val <- manyTill anyChar eol
  return (key, readValue key val)

-- get header

type KVPs = M.Map String Value

-- maybeSuccess :: Result a -> Maybe a
-- maybeSuccess (Success a) = Just a
-- maybeSuccess _ = Nothing

fromResult :: a -> Result a -> a
fromResult _ (Success a) = a
fromResult x _ = x

-- note :: e -> Maybe a -> Either e a
-- note e Nothing  = Left e
-- note _ (Just a) = Right a

dbl :: Parser Double
dbl = try double <|> try intdouble
  where
    intdouble = fromInteger <$> integer

parseTuple3 :: Parser Tuple3
parseTuple3 = (,,) <$> (bo *> dbl <* sep) <*> (dbl <* sep) <*> (dbl <* bc<* spaces)
  where
    bo = token $ char '('
    bc = token $ char ')'
    sep = token $ char ','

parseSpaceDirections :: Parser SpaceDirections
parseSpaceDirections = do
  choice $ map try [
    DWISpace <$> parseTuple3 <*> parseTuple3 <*> (parseTuple3 <* string "none" <* eol),
    StructuralSpace <$> parseTuple3 <*> parseTuple3 <*> (parseTuple3 <* eol)
         ]

-------------------------------------------------------------
-- Read functions

runParseHeader :: String -> Result KVPs
runParseHeader = parseString parser mempty
  where
    parser = fmap M.fromList $ skipNrrdMagic *> some (skipComments *> parseKVP)

readArray :: String -> [Integer]
readArray s = fromResult [] $ parseString (some integer) mempty s

readSpace :: String -> Space
readSpace s = case (map toLower s) of
  "lps" -> LPS
  "left-posterior-superior" -> LPS
  "rps" -> RPS
  "right-anterior-superior" -> RAS
  "las" -> LAS
  "left-anterior-posterior" -> LAS
  _ -> error "Invalid space"

readSpaceDirections :: String -> SpaceDirections
readSpaceDirections s = fromResult (error errmsg) $ parseString parseSpaceDirections mempty s
  where errmsg = "Invalid value in nrrd after 'space:'"

readKinds :: String -> [String]
readKinds = words . unwords . words

keyReadMap :: M.Map Key (String -> Value)
keyReadMap = M.fromList [
  ("type", VDataType),
  ("dimension", VDimension . (read :: String -> Int)),
  ("space", VSpace . readSpace),
  ("sizes", VSizes . readArray),
  ("space directions", VSpaceDirections . readSpaceDirections),
  ("kinds", VKinds . readKinds),
  ("endian", VEndian),
  ("encoding", VEncoding),
  ("space origin", VSpaceOrigin . (read :: String -> Tuple3))
  ]

readValue :: Key -> String -> Value
readValue k s
  =  (M.findWithDefault VDefault k keyReadMap) s

-------------------------------------------------------------
-- Logic

nrrdCheck :: KVPs -> KVPs -> Bool
nrrdCheck kvps kvpsRef
  = and $ map valsAreEqual $ M.elems $ M.filterWithKey (\k _ -> k/="content") $ M.intersectionWith (,) kvps kvpsRef
    where
      valsAreEqual (v1, v2) = v1 == v2

----------------------------------------------------------------------------------
-- Arg parsing

data NrrdChecker = NrrdChecker {inNrrd :: String, refNrrd :: String}
  deriving (Show,Data,Typeable)

nrrdchecker = NrrdChecker{inNrrd = def &= typ "NRRD", refNrrd = def &= typ "NRRD"}

----------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  args <- cmdArgs nrrdchecker
  hPrintf stderr "Read in '%s'\n" (refNrrd args)
  hPrintf stderr "Read in '%s'\n" (inNrrd args)
  refnrrd <- fmap runParseHeader $ readFile $ refNrrd args
  innrrd <- fmap runParseHeader $ readFile $ inNrrd args
  case nrrdCheck <$> innrrd <*> refnrrd of
    Success result -> printf "%s,pass" (inNrrd args)
    Failure doc -> do
      printf "%s,fail" (inNrrd args)
      hPutStrLn stderr (show doc)
