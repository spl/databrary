{-# LANGUAGE OverloadedStrings #-}
module Databrary.Search.Service
  ( Solr(..)
  , initSolr
  , finiSolr
  , MonadSolr
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Maybe (isNothing)
import qualified Network.HTTP.Client as HC
import System.FilePath ((</>))
import System.IO (openFile, IOMode(AppendMode))
import qualified System.Process as Proc
import System.Timeout (timeout)

import Databrary.Has (MonadHas)
import Databrary.HTTP.Client (HTTPClient)

data Solr = Solr
  { solrRequest :: HC.Request
  , solrProcess :: Proc.ProcessHandle
  }

initSolr :: C.Config -> IO Solr
initSolr conf = do
  port <- C.require conf "port"
  db <- C.lookupDefault "Databrary" conf "db"
  req <- HC.parseUrl $ "http://localhost/solr/" ++ db ++ "/"
  path <- C.require conf "path"
  logf <- C.lookup conf "log"
  out <- maybe (return Proc.Inherit) (\f -> Proc.UseHandle <$> openFile f AppendMode) logf
  (_, _, _, ph) <- Proc.createProcess (Proc.proc (path </> "bin" </> "solr") ["start", "-h", "localhost", "-p", show port, "-f"])
    { Proc.std_out = out
    , Proc.std_err = out
    , Proc.close_fds = True
    }
  return Solr
    { solrRequest = req
      { HC.port = port
      , HC.redirectCount = 0
      }
    , solrProcess = ph
    }

finiSolr :: Solr -> IO ()
finiSolr Solr{ solrProcess = ph } = do
  -- this timeout doesn't actually seem to work:
  r <- timeout 10000000 $ Proc.waitForProcess ph
  when (isNothing r) $ do
    putStrLn "solr failed to stop; terminating..."
    Proc.terminateProcess ph

type MonadSolr c m = (MonadIO m, MonadHas HTTPClient c m, MonadHas Solr c m)
