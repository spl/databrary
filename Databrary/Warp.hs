{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Warp
  ( runWarp
  ) where

import Control.Applicative ((<$>), (<|>))
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import qualified Data.Traversable as Trav
import Data.Version (showVersion)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
#ifdef VERSION_warp_tls
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.TLS as TLS
#endif

import Paths_databrary (version)
import qualified Databrary.Store.Config as C
import Databrary.Service.Types
import Databrary.Service.Log

runWarp :: C.Config -> Service -> Wai.Application -> IO ()
runWarp conf rc app = do
#ifdef VERSION_warp_tls
  let certs c = C.config c <|> return <$> C.config c
      run (Just k) (Just (cert:chain)) = WarpTLS.runTLS tlss
        { WarpTLS.tlsServerHooks = (WarpTLS.tlsServerHooks tlss){ TLS.onALPNClientSuggest = Nothing } } where tlss = WarpTLS.tlsSettingsChain cert chain k
      run _ _ = Warp.runSettings
  run (conf C.! "ssl.key") (certs $ conf C.! "ssl.cert")
#else
  Warp.runSettings
#endif
    ( Warp.setPort (conf C.! "port")
    $ Warp.setTimeout 300
    $ Warp.setServerName (BSC.pack $ "databrary/" ++ showVersion version)
    $ Warp.setOnException (\req e -> do
      t <- getCurrentTime
      msg <- Trav.mapM (\q -> requestLog t q Nothing $ Warp.exceptionResponseForDebug e) req
      logMsg t (maybe id (\m -> (<>) (m <> "\n")) msg $ toLogStr $ show e) (serviceLogs rc))
    $ Warp.defaultSettings)
    app
