{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.Map                     as M (foldMapWithKey, filterWithKey, intersectionWith)
import           System.Console.CmdArgs
import           System.Exit                  (exitFailure, exitSuccess)
import           System.IO                    (stderr, hPutStr)
import           Text.Printf                  (printf, hPrintf)
import           Nrrd.Parser                  (readNrrdHeader)
import           Nrrd.Types                   (KVPs)
import           Text.PrettyPrint.ANSI.Leijen (linebreak, hPutDoc, (<>), pretty)
import           Text.Trifecta                (Result (..))

nrrdDiff :: KVPs -> KVPs -> Maybe String
nrrdDiff kvps kvpsRef
  = M.foldMapWithKey diff $ M.filterWithKey (\k _ -> k/="content") $ kvpsZipped
    where
      diff k (v,v') | v == v' = Nothing
                    | otherwise = Just (printf "* %s:\n input value: %s\n   ref value: %s\n" k (show v) (show v'))
      kvpsZipped = M.intersectionWith (,) kvps kvpsRef

data NrrdCheckerArgs = NrrdCheckerArgs {inNrrd, refNrrd :: String}
  deriving (Show, Data, Typeable)

nrrdcheckerArgs :: NrrdCheckerArgs
nrrdcheckerArgs
  = NrrdCheckerArgs{ inNrrd = def &= typ "NRRD"
                   , refNrrd = def &= typ "NRRD" }

main :: IO ()
main = do
  args <- cmdArgs nrrdcheckerArgs
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
      exitSuccess
    failure -> do
      printf "%s,fail\n" (inNrrd args)
      hPutDoc stderr $ pretty failure <> linebreak
      exitFailure
