{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.Map            as M
import           Text.RawString.QQ
import           Text.Trifecta
import Data.Maybe

-- Basic field specifications
data DataType = UInt1 | Int1 | UInt2 | Int2 | UInt4 | Int4 | UInt8 | Int8 | Float4 | Float8

-- Space and orientation
data Space = LPS | RPS | RAS | LAS
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


getHeader :: String -> String
getHeader s = case parseString parser mempty s of
                (Success x) -> x
                (Failure doc) -> error $ show doc
   where
     parser = manyTill anyChar end
     end = (void $ string "\n\n") <|> eof

parseSizes :: Parser [Integer]
parseSizes = some integer

-- Check for valid values

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just a) = Right a

getSizes :: String -> Maybe [Integer]
getSizes = maybeSuccess . (parseString parseSizes mempty)

checkSizes :: KVPs -> [Integer] -> Bool
checkSizes kvps validSizes =  sizes == validSizes
  where
    sizes = fromMaybe [] $ (M.lookup "sizes" kvps) >>= getSizes

-- Test
p x = parseString x mempty

main :: IO ()
main = do
  nrrd <- readFile "test.nhdr"
  let hdr = getHeader nrrd
  print hdr
  print $ parseHeader hdr

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

