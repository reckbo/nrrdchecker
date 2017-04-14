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
import           Text.PrettyPrint.ANSI.Leijen (hPutDoc, linebreak, pretty, (<>))
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
     parseStrct = StructuralSpace <$> parseTuple3 <*> parseTuple3 <*> parseTuple3
     parseDWI = DWISpace <$> parseTuple3 <*> parseTuple3 <*>
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


parseKVP :: Parser (Key, Value)
parseKVP = do
  key <- parseKey
  _ <- token $ char ':'
  -- valstr <- manyTill anyChar eol
  let parseStr = manyTill anyChar eol
  val <- if ("DWMRI_gradient" `isPrefixOf` key)
    then fmap VGradientDir $ (,,) <$> ((token $ char '=') *> dbl) <*> dbl <*> dbl
    -- then VGradientDir <$> ((token $ char '=') *> double) <*> double <*> double
    else case key of
           "type" -> VDataType <$> parseStr
           "dimension" -> VDimension <$> natural
           "space" -> VSpace . readSpace <$> parseStr
           "sizes" -> VSizes <$> some natural
           "space directions" -> VSpaceDirections <$> parseSpaceDirections
           "kinds" -> VKinds . words . unwords . words <$> parseStr
           "endian" -> VEndian <$> parseStr
           "encoding" -> VEncoding <$> parseStr
           "space origin" -> VSpaceOrigin <$> parseTuple3
           "measurement frame" -> VMeasurementFrame <$> parseTuple3 <*> parseTuple3
             <*> parseTuple3
           _ -> VDefault <$> parseStr
  return (key, val)

parseHeader :: Parser KVPs
parseHeader = fmap M.fromList $ skipNrrdMagic *> some (skipComments *> parseKVP)

readNrrdHeader :: FilePath -> IO (Result KVPs)
readNrrdHeader nrrd = parseFromFileEx parseHeader nrrd

printResult :: Result (Maybe String) -> IO ()
printResult x = hPutDoc stderr $ pretty x <> linebreak
