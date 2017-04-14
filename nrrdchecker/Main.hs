{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.Map                     as M (foldMapWithKey, filterWithKey, intersectionWith)
import           System.Console.CmdArgs
import           System.Exit                  (exitFailure, exitSuccess)
import           System.IO                    (stderr, hPutStr)
import           Text.Printf                  (printf, hPrintf)
import           Data.Nrrd

nrrdDiff :: KVPs -> KVPs -> Maybe String
nrrdDiff kvps kvpsRef
  = M.foldMapWithKey diff $ M.filterWithKey (\k _ -> k/="content") $ kvpsZipped
    where
      diff k (v,v') | v == v' = Nothing
                    | otherwise = Just (printf "* %s:\n input value: %s\n   ref value: %s\n" k (show v) (show v'))
      kvpsZipped = M.intersectionWith (,) kvps kvpsRef

data NrrdChecker = NrrdChecker {inNrrd, refNrrd :: FilePath}
  deriving (Show, Data, Typeable)

-- filterGradientDir :: KVPS ->

nrrdchecker :: NrrdChecker
nrrdchecker
  = NrrdChecker { inNrrd = def &= typFile
                , refNrrd = def &= typFile }
    &= summary "Computes the diff between the headers of two nrrd files."

main :: IO ()
main = do
  args <- cmdArgs nrrdchecker
  refHeader <- readNrrdHeader $ refNrrd args
  inHeader <- readNrrdHeader $ inNrrd args
  case nrrdDiff <$> inHeader <*> refHeader of
    Success Nothing -> do
      printf "%s,pass\n" (inNrrd args)
      exitSuccess
    Success (Just diff) -> do
      hPrintf stderr "INPUT: %s\n  REF: %s\n" (inNrrd args) (refNrrd args)
      hPutStr stderr diff
      printf "%s,fail\n" (inNrrd args)
      exitFailure
    failure -> do
      printf "%s,fail\n" (inNrrd args)
      printResult failure
      exitFailure
