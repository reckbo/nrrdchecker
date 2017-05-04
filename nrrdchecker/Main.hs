{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


import           Data.List           (intercalate)
import qualified Data.Map            as M (filterWithKey, foldMapWithKey,
                                            intersectionWith)
import           Data.Nrrd
import           System.Exit         (exitFailure, exitSuccess)
-- import           System.IO           (hPutStr, stderr)
-- import           Text.Printf         (hPrintf, printf)
-- import           System.Console.CmdArgs
import           Data.Csv            (ToField (..), encode)
import           Data.Semigroup      ((<>))
import           Options.Applicative hiding (Success)
import qualified Data.ByteString.Char8 as B (pack, putStr)
import qualified Data.ByteString.Lazy as BL (toStrict)


epsilon :: Double
epsilon = 0.05


data EpsilonConfig = EpsilonConfig
  {epsSpaceDirections, epsMeasurementFrame, epsSpaceOrigin, epsGradientDirection :: Double}


data NrrdCheckerArgs = NrrdCheckerArgs
  {inNrrd, refNrrd :: FilePath,
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
instance ToField Bool where
  toField = B.pack . show

nrrdDiff :: EpsilonConfig -> KVPs -> KVPs -> [(Key, Value, Value, Maybe Double, Bool)]
nrrdDiff epsilonConfig kvps kvpsRef
  = M.foldMapWithKey diff $ M.filterWithKey (\k _ -> k/="content") $ kvpsZipped
    where
      diff k (v,v') = [(k, v, v', eps, ans)]
        where (eps, ans) = (veq epsilonConfig v v')
      kvpsZipped = M.intersectionWith (,) kvps kvpsRef


-- epsilonOption :: String ->
epsilonOption :: String -> Parser Double
epsilonOption name =
  option auto ( long name
              <> help ("Epsilon tolerance for " ++ name)
              <> showDefault
              <> value epsilon
              <> metavar "DOUBLE" )


args :: Parser NrrdCheckerArgs
args = NrrdCheckerArgs
      <$> strOption
          ( long "inNrrd"
          <> short 'i'
          <> metavar "NRRD"
          <> help "Input nrrd")
      <*> strOption
          ( long "refNrrd"
          <> short 'r'
          <> metavar "NRRD"
          <> help "Reference nrrd")
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
     <> header "Compares the headers of two nrrd files." )


toCsv :: String -> String -> [(Key, Value, Value)] -> String
toCsv nrrd nrrdRef tuples = unlines . map (intercalate ",") $ [header] ++ rows
  where header = ["nrrd", "nrrdRef", "key", "value", "valueRef"]
        rows = (map (\(k, v, v') -> [nrrd,nrrdRef,show k, printVal v, printVal v']) tuples)
        printVal v = unwords . tail . words . show $ v

nrrdchecker :: NrrdCheckerArgs -> IO ()
nrrdchecker NrrdCheckerArgs{..} = do
  refNhdr <- readNrrdHeader $ refNrrd
  inNhdr <- readNrrdHeader $ inNrrd
  case nrrdDiff epsilonConfig <$> inNhdr <*> refNhdr of
    Success tuples -> do
      B.putStr . BL.toStrict . encode $ tuples
      exitSuccess
    failure -> do -- failed to parse nrrd
      printResult failure
      exitFailure
