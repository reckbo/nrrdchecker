{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.Map                     as M
import           Text.RawString.QQ
import           Text.Trifecta

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
    oneWord = many letter
    twoWords = do
          w1 <- many letter
          char ' '
          w2 <- many letter
          return $ w1++" "++w2

parseKVP :: Parser (String, String)
parseKVP = do
  key <- parseKey
  token $ char ':'
  value <- manyTill anyChar newline
  return (key, trimEnd value)
    where trimEnd = reverse . dropWhile (' '==) . reverse

-- get header

type KVPs = M.Map String String

parseHeader :: Parser KVPs
parseHeader = fmap M.fromList $ skipNrrdMagic *> manyTill (skipComments *> parseKVP) newline

getKVPs :: String -> Result KVPs
getKVPs s = parseString parseHeader mempty s




-- Test
p x = parseString x mempty

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

