{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Map            as M
import           Data.Maybe
import           Text.RawString.QQ
import           Text.Trifecta

-- Basic field specifications
data DataType = UInt1 | Int1 | UInt2 | Int2 | UInt4 | Int4 | UInt8 | Int8 | Float4 | Float8

-- Space and orientation
data Space = LPS | RPS | RAS | LAS
  deriving (Show, Eq)

type Tuple3 = (Double, Double, Double)
data SpaceDirections =
  StructuralSpace Tuple3 Tuple3 Tuple3 |
  DWISpace Tuple3 Tuple3 Tuple3
  deriving (Eq, Show)

-- Per axis specifications
type Sizes = [Int]
type Kinds = [String]

-- # Parsers

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

parseKVP :: Parser (String, String)
parseKVP = do
  key <- parseKey
  token $ char ':'
  value <- manyTill anyChar eol
  return (key, value)

-- get header

type KVPs = M.Map String String

parseHeader :: String -> Result KVPs
parseHeader = parseString parser mempty
  where
    parser = fmap M.fromList $ skipNrrdMagic *> some (skipComments *> parseKVP)

readHeader :: String -> KVPs
readHeader = (fromResult M.empty) . parseHeader


getHeader :: String -> String
getHeader s = case parseString parser mempty s of
                (Success x) -> x
                (Failure doc) -> error $ show doc
   where
     parser = manyTill anyChar end
     end = (void $ string "\n\n") <|> eof

-- Check for valid values

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

fromResult :: a -> Result a -> a
fromResult _ (Success a) = a
fromResult x _ = x

note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just a) = Right a

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

readSpaceDirections :: String -> SpaceDirections
readSpaceDirections s = fromResult (error errmsg) $ parseString parseSpaceDirections mempty s
  where errmsg = "Invalid value in nrrd after 'space:'"


type Key = String
type Predicate = String -> Bool

pred ::  (String -> a) -> (a -> a -> Bool) -> a -> Predicate
pred readFunc predicate val = (predicate val) . readFunc

assertKinds :: Kinds -> Predicate
assertKinds e = (e ==) . readKinds
  where readKinds = words . unwords . words

assertSpace :: Space -> Predicate
assertSpace s = (s ==) . readSpace

assertSizes :: [Integer] -> Predicate
assertSizes e = (e ==) . readArray

assertSpaceDirections :: SpaceDirections -> Predicate
assertSpaceDirections e = (e ==). readSpaceDirections

assertOrigin :: Tuple3 -> Predicate
assertOrigin e = (e ==) . rd
  where rd = read :: String -> Tuple3

validators :: [(Key, Predicate)]
validators = [
  ("type", (== "short")),
  ("dimension", (== "3")),
  ("space", assertSpace LPS),
  ("sizes", assertSizes [256,256,176]),
  ("space directions", assertSpaceDirections $ StructuralSpace (0,1,0) (0,0,-1) (-1,0,0)),
  ("kinds", assertKinds ["domain","domain","domain"]),
  ("encoding", ("gzip" ==) ),
  ("space origin", assertOrigin (87.5, -127.5, 127.5))
  ]

validateKey :: (Key, Predicate) -> KVPs -> Bool
validateKey (key, pred) kvps = fromMaybe False $ pred <$> M.lookup key kvps

validateKVPs :: [(Key, Predicate)] -> KVPs -> Bool
validateKVPs validators kvps = and $ validateKey <$> validators <*> [kvps]

main :: IO ()
main = do
  hdr <- getHeader <$> readFile "test.nhdr"
  print hdr
  putStrLn "Does good header pass?"
  print $ readHeader hdr
  print $ validateKVPs validators $ readHeader hdr
  putStrLn "Does bad header pass?"
  print $ validateKVPs validators $ readHeader badHeader

testHeader = [r|NRRD0004
# Complete NRRD file format specification at:
# http://teem.sourceforge.net/nrrd/format.html
type: short
dimension: 3
space: left-posterior-superior
sizes: 256 256 176
space directions: (0,1,0) (0,0,-1) (-1,0,0)
kinds: domain domain domain
endian: little
encoding: gzip
space origin: (87.5, -127.5, 127.5)

rawdatagoesheredonotparse|]

badHeader = [r|NRRD0004
type : short |]

