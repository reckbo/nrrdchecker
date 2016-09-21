{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.Map                     as M
import           System.Console.CmdArgs
import           System.Exit                  (exitFailure, exitSuccess)
import           System.IO                    (stderr)
import           Text.Printf                  (printf)
import           Nrrd.Parser                  (readNrrdHeader)
import           Nrrd.Types                   (KVPs)
import           Text.PrettyPrint.ANSI.Leijen (linebreak, hPutDoc, (<>), pretty)
import           Text.Trifecta                (Result (..))

nrrdCheck :: KVPs -> KVPs -> Bool
nrrdCheck kvps kvpsRef
  = and $ map valsAreEqual $ M.elems $ M.filterWithKey (\k _ -> k/="content") $ kvpsZipped
    where
      kvpsZipped = M.intersectionWith (,) kvps kvpsRef
      valsAreEqual (v1, v2) = v1 == v2

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
  case nrrdCheck <$> inHeader <*> refHeader of
    Success True -> do
      printf "%s,pass\n" (inNrrd args)
      exitSuccess
    Success False -> do
      printf "%s,fail\n" (inNrrd args)
      exitSuccess
    failure -> do
      printf "%s,fail\n" (inNrrd args)
      hPutDoc stderr $ pretty failure <> linebreak
      exitFailure
