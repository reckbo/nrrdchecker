{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Map            as M (filterWithKey, foldMapWithKey,
                                            intersectionWith, unionsWith, map,
                                            mapWithKey, toList
                                          )
import           Data.Nrrd
import           System.Exit         (exitFailure, exitSuccess)
-- import           System.IO           (hPutStr, stderr)
-- import           Text.Printf         (hPrintf, printf)
-- import           System.Console.CmdArgs
import           Data.Csv            (ToField (..), encode)
import           Data.Semigroup      ((<>))
import           Options.Applicative hiding (Success)
import qualified Data.ByteString.Char8 as B (pack, putStr, intercalate)
import qualified Data.ByteString.Lazy as BL (toStrict, writeFile, ByteString(..), append)


epsilon :: Double
epsilon = 0.05


data EpsilonConfig = EpsilonConfig
  {epsSpaceDirections, epsMeasurementFrame, epsSpaceOrigin, epsGradientDirection :: Double}


data NrrdCheckerArgs = NrrdCheckerArgs
  {inputNrrds :: (FilePath, FilePath, [FilePath]),
   outfile :: Maybe FilePath,
   epsilonConfig   :: EpsilonConfig
  }


eqTuple3 :: (Num a, Ord a) => (a, a, a) -> (a, a, a) -> a -> Bool
eqTuple3 (x0,x1,x2) (x0',x1',x2') eps =
  abs (abs x0 - abs x0') < eps &&
  abs (abs x1 - abs x1') < eps &&
  abs (abs x2 - abs x2') < eps


veq :: EpsilonConfig -> Value -> Value -> (Maybe Double, Bool)
veq EpsilonConfig{..}
  (VGradientDir x)
  (VGradientDir x') =
  (Just epsGradientDirection, eqTuple3 x x' epsGradientDirection)
veq EpsilonConfig{..}
  (VSpaceDirections (StructuralSpaceDirections t0 t1 t2))
  (VSpaceDirections (StructuralSpaceDirections t0' t1' t2')) =
  (Just epsSpaceDirections,
  eqTuple3 t0 t0' epsSpaceDirections &&
  eqTuple3 t1 t1' epsSpaceDirections &&
  eqTuple3 t2 t2' epsSpaceDirections)
veq EpsilonConfig{..}
  (VSpaceDirections (DWISpaceDirections t0 t1 t2))
  (VSpaceDirections (DWISpaceDirections t0' t1' t2')) =
  (Just epsSpaceDirections,
  eqTuple3 t0 t0' epsSpaceDirections &&
  eqTuple3 t1 t1' epsSpaceDirections &&
  eqTuple3 t2 t2' epsSpaceDirections)
veq EpsilonConfig{..}
  (VSpaceOrigin t)
  (VSpaceOrigin t') =
  (Just epsSpaceOrigin,
  eqTuple3 t t' epsSpaceOrigin)
veq EpsilonConfig{..}
  (VMeasurementFrame t0 t1 t2)
  (VMeasurementFrame t0' t1' t2') =
  (Just epsMeasurementFrame,
  eqTuple3 t0 t0' epsMeasurementFrame &&
  eqTuple3 t1 t1' epsMeasurementFrame &&
  eqTuple3 t2 t2' epsMeasurementFrame)
veq _ x x' = (Nothing, x == x')

instance ToField Value where
  toField = B.pack . unwords . tail . words . filter (/='\\') . filter (/='"') . show
instance ToField [Value] where
  toField xs = B.intercalate "|" $ map toField xs
instance ToField Bool where
  toField = B.pack . show


nrrdDiff :: EpsilonConfig -> KVPs -> KVPs -> [(Key, Value, Value, Maybe Double, Bool)]
nrrdDiff epsilonConfig kvps kvpsRef
  = M.foldMapWithKey diff $ M.filterWithKey (\k _ -> k/="content") $ kvpsZipped
    where
      diff k (v,v') = [(k, v, v', eps, ans)]
        where (eps, ans) = (veq epsilonConfig v v')
      kvpsZipped = M.intersectionWith (,) kvps kvpsRef


nrrdDiff2 :: EpsilonConfig -> [KVPs] -> [(Key, [Value], Maybe Double, Bool)]
nrrdDiff2 epsilonConfig ms
  = M.foldMapWithKey diff $ M.filterWithKey (\k _ -> k/="content") mapWithLists
    where
      diff k vs = [(k, vs, eps, ans)]
        where
          allEqual (x:x':xs') = (eps, ans)
            where
              ys = map (veq epsilonConfig $ x) (x':xs')
              ans = and $ map snd ys
              eps = fst . head $ ys
          allEqual _ = error "Shouldn't be here"
          (eps, ans) = allEqual vs
          -- (eps, ans) = (veq' epsilonConfig vs)
      mapWithLists = M.unionsWith (++) (map (M.map (:[])) ms)


nrrdDiff3 :: EpsilonConfig -> [FilePath] -> [KVPs] -> [(FilePath, Key, Value, Maybe Double, Bool)]
nrrdDiff3 epsilonConfig fs ms
  = M.foldMapWithKey diff $ M.filterWithKey (\k _ -> k/="content") mapWithLists
    where
      diff _ [] = []
      diff k vs@(v:vs') = map (\(f,v,b) -> (f, k, v, eps, b)) (zip3 fs vs bools)
            where
              (eps, _) = veq epsilonConfig v v
              bools = True : map (snd . (veq epsilonConfig v)) vs'
      mapWithLists = M.unionsWith (++) (map (M.map (:[])) ms)


-- epsilonOption :: String ->
epsilonOption :: String -> Parser Double
epsilonOption name =
  option auto ( long name
              <> help ("Epsilon tolerance for " ++ name)
              <> showDefault
              <> value epsilon
              <> metavar "DOUBLE" )

nrrdList :: String -> (String, String, [String])
nrrdList s = case words s of
  (x:x':xs) -> (x,x',xs)
  _ -> error "Must have at least 2 input nrrds."

nrrdOption :: Parser String
nrrdOption = (strOption
      ( long "in"
      <> short 'i'
      <> metavar "NRRD"
      <> help "a Nrrd file"
      ))

args :: Parser NrrdCheckerArgs
args = NrrdCheckerArgs
      <$> ((,,) <$> nrrdOption <*> nrrdOption <*> many nrrdOption)
      <*> optional (strOption
          (long "out"
           <> short 'o'
           <> metavar "OUTFILE"
           <> help "Output csv (if omitted, prints to stdout)"
          ))
      <*> (EpsilonConfig
        <$> epsilonOption "spaceDirections"
        <*> epsilonOption "measurementFrame"
        <*> epsilonOption "spaceOrigin"
        <*> epsilonOption "gradientDirection"
      )


main :: IO ()
main = nrrdchecker =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> header "Compares the headers of two or more nrrd files." )


-- toCsv :: String -> String -> [(Key, Value, Value)] -> String
-- toCsv nrrd nrrdRef tuples = unlines . map (intercalate ",") $ [header] ++ rows
--   where header = ["nrrd", "nrrdRef", "key", "value", "valueRef"]
--         rows = (map (\(k, v, v') -> [nrrd,nrrdRef,show k, printVal v, printVal v']) tuples)
--         printVal v = unwords . tail . words . show $ v

nrrdchecker :: NrrdCheckerArgs -> IO ()
nrrdchecker NrrdCheckerArgs{..} = do
  let inputNrrds' = x:x':xs where (x,x',xs) = inputNrrds
      header = encode [["filepath" :: String, "key", "value", "epsilon", "isequal"]] :: BL.ByteString
  parsedNhdrs <- sequenceA <$> traverse readNrrdHeader inputNrrds'
  case nrrdDiff3 epsilonConfig inputNrrds' <$> parsedNhdrs of
    Success tuples -> do
      case outfile of
        Nothing -> do B.putStr . BL.toStrict . (BL.append header) . encode $ tuples
                      exitSuccess
        Just outfile' -> BL.writeFile outfile' . (BL.append header) . encode $ tuples
    failure -> do -- failed to parse nrrd
      printResult failure
      exitFailure
