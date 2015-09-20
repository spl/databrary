{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module Databrary.EZID.API
  ( runEZIDM
  , ezidStatus
  , EZIDMeta(..)
  , ezidGet
  , ezidCreate
  , ezidModify
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.Traversable as Trav
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (methodGet, methodPut, methodPost)
import Network.URI (URI, parseURI)
import qualified Text.XML.Light as XML

import Databrary.Has
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.Context
import Databrary.HTTP.Client
import Databrary.EZID.Service
import qualified Databrary.EZID.ANVL as ANVL

data EZIDContext = EZIDContext
  { ezidContext :: !BackgroundContext
  , contextEZID :: !EZID
  }

makeHasRec ''EZIDContext ['ezidContext, 'contextEZID]

type EZIDM a = CookiesT (ReaderT EZIDContext IO) a

runEZIDM :: EZIDM a -> ContextM (Maybe a)
runEZIDM f = ReaderT $ \ctx ->
  Trav.mapM (runReaderT (runCookiesT f) . EZIDContext (BackgroundContext ctx))
    (serviceEZID $ contextService ctx)
  
ezidCall :: BS.ByteString -> BS.ByteString -> ANVL.ANVL -> EZIDM (Maybe ANVL.ANVL)
ezidCall path method body = do
  req <- peeks ezidRequest
  r <- httpHandle $ withResponseCookies (requestAcceptContent "text/plain" req)
    { HC.path = path
    , HC.method = method
    , HC.requestBody = HC.RequestBodyLBS $ B.toLazyByteString $ ANVL.encode body
    } (fmap P.eitherResult . httpParse ANVL.parse)
  either
    (\e -> do
      t <- liftIO getCurrentTime
      focusIO $ logMsg t (toLogStr ("ezid: " <> method <> " " <> path <> ": ") <> toLogStr e)
      return Nothing)
    (return . Just)
    r

ezidCheck :: ANVL.ANVL -> Maybe T.Text
ezidCheck = lookup "success"

ezidStatus :: EZIDM Bool
ezidStatus =
  isJust . (ezidCheck =<<) <$> ezidCall "/status" methodGet []

data EZIDMeta = EZIDMeta
  { ezidTarget :: !URI
  , ezidPublic :: !Bool
  , ezidDataCite :: !XML.Element
  }

ezidMeta :: EZIDMeta -> ANVL.ANVL
ezidMeta EZIDMeta{..} =
  [ ("_target", T.pack $ show ezidTarget)
  , ("_status", if ezidPublic then "public" else "unavailable")
  , ("_profile", "datacite")
  , ("datacite", T.pack $ XML.showTopElement ezidDataCite)
  ]

ezidGet :: BS.ByteString -> EZIDM (Maybe EZIDMeta)
ezidGet hdl = do
  r <- ezidCall ("/id/" <> hdl) methodGet []
  return $ do
    a <- r
    target <- parseURI . T.unpack =<< lookup "_target" a
    status <- lookup "_status" a
    datacite <- XML.parseXMLDoc =<< lookup "datacite" a
    return $ EZIDMeta
      { ezidTarget = target
      , ezidPublic = status == "public"
      , ezidDataCite = datacite
      }

ezidCreate :: BS.ByteString -> EZIDMeta -> EZIDM (Maybe T.Text)
ezidCreate hdl meta = do
  ns <- peeks ezidNS
  (ezidCheck =<<) <$> ezidCall ("/id/" <> ns <> hdl) methodPut (ezidMeta meta)

ezidModify :: BS.ByteString -> EZIDMeta -> EZIDM Bool
ezidModify hdl meta =
  isJust . (ezidCheck =<<) <$> ezidCall ("/id/" <> hdl) methodPost (ezidMeta meta)
