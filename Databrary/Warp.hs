{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Warp
  ( runWarp
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS

import Paths_databrary (version)
import qualified Databrary.Store.Config as C
import Databrary.Service.Types
import Databrary.Service.Log

runWarp :: C.Config -> Service -> Wai.Application -> IO ()
runWarp conf rc app =
  run (conf C.! "ssl.key") (certs $ conf C.! "ssl.cert")
    ( Warp.setPort (conf C.! "port")
    $ Warp.setTimeout 300
#ifndef DEVEL
    $ Warp.setFdCacheDuration 300
    $ Warp.setFileInfoCacheDuration 300
#endif
    $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
    $ Warp.setOnException (\req e -> do
      t <- getCurrentTime
      msg <- mapM (\q -> requestLog t q Nothing $ Warp.exceptionResponseForDebug e) req
      logMsg t (maybe id (\m -> (<>) (m <> "\n")) msg $ toLogStr $ show e) (serviceLogs rc))
    $ Warp.setHTTP2Disabled
    $ Warp.defaultSettings)
    app
  where
  certs c = C.config c <|> return <$> C.config c
  run (Just k) (Just (cert:chain)) = WarpTLS.runTLS (WarpTLS.tlsSettingsChain cert chain k)
  run _ _ = Warp.runSettings
