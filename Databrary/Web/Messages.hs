{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Messages
  ( generateMessagesJS
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString.Builder as BSB
import qualified Data.Configurator as C
import System.IO (withBinaryFile, IOMode(WriteMode), hPutStr)

import Paths_databrary (getDataFileName)
import qualified Databrary.JSON as JSON
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

generateMessagesJS :: WebGenerator
generateMessagesJS fo@(f, _) = do
  -- it'd be nice to use Service.Messages here but there's no good way
  mf <- liftIO $ getDataFileName "messages.conf"
  webRegenerate (do
    ml <- C.getMap =<< C.load [C.Optional mf]
    withBinaryFile (webFileAbs f) WriteMode $ \h -> do
      hPutStr h "app.constant('messageData',"
      BSB.hPutBuilder h $ JSON.encodeToBuilder $ JSON.toJSON ml
      hPutStr h ");")
    [mf] [] fo
