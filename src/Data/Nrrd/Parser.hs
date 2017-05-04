module Data.Nrrd.Parser
  (
    readNrrdHeader,
    Text.Trifecta.Result (..),
    printResult
  ) where

import           Control.Applicative
import           Control.Monad                (void)
import           Data.Char                    (toLower)
import           Data.List                    (isPrefixOf)
import qualified Data.Map                     as M
import           Data.Nrrd.Types
import           System.IO                    (stderr)
import           Text.PrettyPrint.ANSI.Leijen (hPutDoc, linebreak, pretty, (<>), Pretty (..))
import           Text.Trifecta

skipComments :: Parser [String]
skipComments = many (char '#' *> manyTill anyChar newline)

skipNrrdMagic :: Parser ()
skipNrrdMagic = void $ string "NRRD000" *> anyChar *> newline

eol :: Parser ()
eol = choice $ map (try . (spaces' *>)) [void newline, eof]
  where
    spaces' = many $ char ' '

dbl :: Parser Double
dbl = toDbl <$> integerOrDouble
      where
        toDbl (Left i)  = fromIntegral i
        toDbl (Right d) = d

parseTuple3 :: Parser Tuple3
parseTuple3 = (,,) <$> (bo *> dbl <* sep) <*> (dbl <* sep) <*> (dbl <* bc)
  where
    bo = token $ char '('
    bc = token $ char ')'
    sep = token $ char ','

parseSpaceDirections :: Parser SpaceDirections
parseSpaceDirections = do
  choice [ try parseDWI, try parseStrct ]
  where
     parseStrct = StructuralSpaceDirections <$> parseTuple3 <*> parseTuple3 <*> parseTuple3
     parseDWI = DWISpaceDirections <$> parseTuple3 <*> parseTuple3 <*>
      (parseTuple3 <* string "none" <* eol)

readSpace :: String -> Space
readSpace s = case (map toLower s) of
  "lps"                     -> LPS
  "left-posterior-superior" -> LPS
  "rps"                     -> RPS
  "right-anterior-superior" -> RAS
  "las"                     -> LAS
  "left-anterior-posterior" -> LAS
  _                         -> error "Invalid space"

parseKey :: Parser String
parseKey = try twoWords <|> try oneWord
  where
    oneWord = some (alphaNum <|> char '_' <|> char '-')
    twoWords = do
          w1 <- oneWord
          char ' '
          w2 <- oneWord
          return $ w1++" "++w2


parseSeparator :: Parser ()
parseSeparator = try s <|> try s'
  where
    s = void $ token (string ":=")
    s' = void $ token (char ':')

parseStringValue :: Parser String
parseStringValue = manyTill anyChar eol

parseKVP :: Parser (Key, Value)
parseKVP = do
  key <- parseKey
  parseSeparator
  val <- if ("DWMRI_gradient" `isPrefixOf` key)
    then fmap VGradientDir $ (,,) <$> dbl <*> dbl <*> dbl
    else case key of
           "type" -> VDataType <$> parseStringValue
           "dimension" -> VDimension <$> natural
           "space" -> VSpace . readSpace <$> parseStringValue
           "sizes" -> VSizes <$> some natural
           "space directions" -> VSpaceDirections <$> parseSpaceDirections
           "kinds" -> VKinds . words . unwords . words <$> parseStringValue
           "endian" -> VEndian <$> parseStringValue
           "encoding" -> VEncoding <$> parseStringValue
           "space origin" -> VSpaceOrigin <$> parseTuple3
           "measurement frame" -> VMeasurementFrame <$> parseTuple3 <*> parseTuple3
             <*> parseTuple3
           _ -> VDefault <$> parseStringValue
  return (key, val)

parseHeader :: Parser KVPs
parseHeader = fmap M.fromList $ skipNrrdMagic *> some (skipComments *> parseKVP)

readNrrdHeader :: FilePath -> IO (Result KVPs)
readNrrdHeader nrrd = parseFromFileEx parseHeader nrrd

printResult :: Pretty a => a -> IO ()
printResult x = hPutDoc stderr $ pretty x <> linebreak
