{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map                     as M (foldMapWithKey, filterWithKey, intersectionWith, fromList, Map)
import           System.Exit                  (exitFailure, exitSuccess)
import           System.IO                    (stderr, hPutStr)
import           Text.Printf                  (printf, hPrintf)
import           Data.Nrrd
-- import           System.Console.CmdArgs
import Options.Applicative hiding (Success)
import Data.Semigroup ((<>))


epsilon :: Double
epsilon = 0.05


data EpsilonConfig = EpsilonConfig
  {epsSpaceDirections, epsMeasurementFrame, epsSpaceOrigin, epsGradientDirection :: Double}


data NrrdCheckerArgs = NrrdCheckerArgs
  {inNrrd, refNrrd :: FilePath,
   epsilonConfig :: EpsilonConfig
  }


eqTuple3 (x0,x1,x2) (x0',x1',x2') eps =
  abs (abs x0 - abs x0') < eps &&
  abs (abs x1 - abs x1') < eps &&
  abs (abs x2 - abs x2') < eps


veq :: EpsilonConfig -> Value -> Value -> Bool
veq EpsilonConfig{..} (VGradientDir x) (VGradientDir x') = eqTuple3 x x' epsGradientDirection
veq EpsilonConfig{..} (VSpaceDirections (StructuralSpace t0 t1 t2))
  (VSpaceDirections (StructuralSpace t0' t1' t2')) =
  eqTuple3 t0 t0' epsSpaceDirections &&
  eqTuple3 t1 t1' epsSpaceDirections &&
  eqTuple3 t2 t2' epsSpaceDirections
veq EpsilonConfig{..} (VSpaceDirections (DWISpace t0 t1 t2))
  (VSpaceDirections (DWISpace t0' t1' t2')) =
  eqTuple3 t0 t0' epsSpaceDirections &&
  eqTuple3 t1 t1' epsSpaceDirections &&
  eqTuple3 t2 t2' epsSpaceDirections
veq EpsilonConfig{..} (VSpaceOrigin t) (VSpaceOrigin t') = eqTuple3 t t' epsSpaceOrigin
veq EpsilonConfig{..} (VMeasurementFrame t0 t1 t2)
  (VMeasurementFrame t0' t1' t2') =
  eqTuple3 t0 t0' epsMeasurementFrame &&
  eqTuple3 t1 t1' epsMeasurementFrame &&
  eqTuple3 t2 t2' epsMeasurementFrame
veq _ x x' = x == x'


nrrdDiff :: EpsilonConfig -> KVPs -> KVPs -> Maybe String
nrrdDiff epsilonConfig kvps kvpsRef
  = M.foldMapWithKey diff $ M.filterWithKey (\k _ -> k/="content") $ kvpsZipped
    where
      diff k (v,v') | veq epsilonConfig v v' = Nothing
                    | otherwise = Just (printf "* %s:\n input value: %s\n   ref value: %s\n" k (show v) (show v'))
      kvpsZipped = M.intersectionWith (,) kvps kvpsRef


-- epsilonOption :: String ->
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


nrrdchecker :: NrrdCheckerArgs -> IO ()
nrrdchecker NrrdCheckerArgs{..} = do
  refHeader <- readNrrdHeader $ refNrrd
  inHeader <- readNrrdHeader $ inNrrd
  case nrrdDiff epsilonConfig <$> inHeader <*> refHeader of
    Success Nothing -> do
      printf "%s,pass\n" inNrrd
      exitSuccess
    Success (Just diff) -> do
      hPrintf stderr "INPUT: %s\n  REF: %s\n" inNrrd refNrrd
      hPutStr stderr diff
      printf "%s,fail\n" inNrrd
      exitFailure
    failure -> do
      printf "%s,fail\n" inNrrd
      printResult failure
      exitFailure
