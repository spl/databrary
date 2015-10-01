{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Transcoder
  ( runTranscoder
  , initTranscoder
  , transcodeEnabled
  ) where

import Control.Applicative ((<$>))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Maybe (isJust)
import Data.Version (showVersion)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Paths_databrary (version, getDataFileName)
import Databrary.Store.Types

runTranscoder :: Transcoder -> [String] -> IO (ExitCode, String, String)
runTranscoder (Transcoder cmd arg) args =
  readProcessWithExitCode cmd (arg ++ args) ""

initTranscoder :: C.Config -> IO (Maybe Transcoder)
initTranscoder conf = do
  host <- C.lookup conf "host"
  dir <- C.lookup conf "dir"
  mount <- C.lookup conf "mount"
  case (host, dir) of
    (Nothing, Nothing) -> return Nothing
    _ -> Just <$> do
      cmd <- getDataFileName "transctl.sh"
      let t = Transcoder cmd $
                [ "-v", showVersion version ]
                ++ maybe [] (\d -> ["-d", d]) dir
                ++ maybe [] (\h -> ["-h", h]) host
                ++ maybe [] (\m -> ["-m", m]) mount
      (r, out, err) <- runTranscoder t ["-t"]
      case r of
        ExitSuccess -> return t
        ExitFailure e -> fail $ "initTranscoder test: " ++ show e ++ "\n" ++ out ++ err

transcodeEnabled :: Storage -> Bool
transcodeEnabled = isJust . storageTranscoder
