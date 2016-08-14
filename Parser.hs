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
type SpaceDirections = [Int]
type SpaceOrigin = [Int]

-- Per axis specifications
type Dimension = Int
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

parseKVP :: Parser (String, String)
parseKVP = do
  key <- parseKey
  token $ char ':'
  value <- manyTill anyChar (void newline <|> eof)
  return (key, trimEnd value)
    where trimEnd = reverse . dropWhile (' '==) . reverse

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

-- Test
p x = parseString x mempty


type Validator = (String, String -> Bool)

validators :: [Validator]
validators = [
  ("dimension", (=="3") ),
  ("encoding", (=="gzip") ),
  ("sizes", (==[256,256,176]) . readArray),
  ("space", (==LPS) . readSpace )
  ]

validateKey :: KVPs -> (String, (String -> Bool)) -> Bool
validateKey kvps (key, pred) = fromMaybe False $ pred <$> M.lookup key kvps

validate :: KVPs -> [(String, (String -> Bool))] -> Bool
validate kvps preds = and $ map (validateKey kvps) preds

main :: IO ()
main = do
  nrrd <- readFile "test.nhdr"
  let hdr = getHeader nrrd
  print hdr
  let kvps = readHeader hdr
  putStrLn "Does test header pass?"
  print $ validate kvps validators

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

