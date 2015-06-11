{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Warp
  ( runWarp
  ) where

#ifdef VERSION_warp_tls
import Control.Applicative ((<$>), (<*>))
#endif
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import qualified Data.Traversable as Trav
import Data.Version (showVersion)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
#ifdef VERSION_warp_tls
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
#endif

import Paths_databrary (version)
import Databrary.Service.Types
import Databrary.Service.Log

runWarp :: C.Config -> Service -> Wai.Application -> IO ()
runWarp conf rc app = do
  port <- C.require conf "port"
#ifdef VERSION_warp_tls
  cert <- C.lookup conf "ssl.cert"
  key <- C.lookup conf "ssl.key"
#endif
#ifdef VERSION_warp_tls
  maybe
    Warp.runSettings
    WarpTLS.runTLS
    (WarpTLS.tlsSettings <$> cert <*> key)
#else
  Warp.runSettings
#endif
    ( Warp.setPort port
    $ Warp.setTimeout 300
    $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
    $ Warp.setOnException (\req e -> do
      t <- getCurrentTime
      msg <- Trav.mapM (\q -> requestLog t q $ Warp.exceptionResponseForDebug e) req
      logMsg t (maybe id (\m -> (<>) (m <> "\n")) msg $ toLogStr $ show e) (serviceLogs rc))
    $ Warp.defaultSettings)
    app
