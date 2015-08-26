{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Warp
  ( runWarp
  ) where

import Control.Applicative ((<$>), (<|>))
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
  key <- C.lookup conf "ssl.key"
  let certs c = C.convert c <|> return <$> C.convert c
      run (Just k) (Just (cert:chain)) = WarpTLS.runTLS (WarpTLS.tlsSettingsChain cert chain k)
      run _ _ = Warp.runSettings
  cert <- C.lookup conf "ssl.cert"
  run key (certs =<< cert)
#else
  Warp.runSettings
#endif
    ( Warp.setPort port
    $ Warp.setTimeout 300
    $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
    $ Warp.setOnException (\req e -> do
      t <- getCurrentTime
      msg <- Trav.mapM (\q -> requestLog t q Nothing $ Warp.exceptionResponseForDebug e) req
      logMsg t (maybe id (\m -> (<>) (m <> "\n")) msg $ toLogStr $ show e) (serviceLogs rc))
    $ Warp.defaultSettings)
    app
