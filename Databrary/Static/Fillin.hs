{-# LANGUAGE OverloadedStrings #-}
module Databrary.Static.Fillin
  ( staticSendInvestigator
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import Data.Maybe (isNothing)
import qualified Data.Text.Encoding as TE
import Data.Time.Format (formatTime)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.URI (renderSimpleQuery)
import System.Locale (defaultTimeLocale)

import Databrary.Has (view)
import Databrary.Service.Types
import Databrary.Service.Log
import Databrary.HTTP.Client
import Databrary.Model.Time
import Databrary.Model.Party
import Databrary.Static.Service

staticSendInvestigator :: Party -> Timestamp -> Service -> IO ()
staticSendInvestigator p t rc@Service{ serviceStatic = Static{ staticAuthorizeAddr = a, staticInvestigator = Just u, staticKey = key } } = void $ forkIO $ do
  req <- HC.parseUrl (show u)
  r <- httpRequest req
    { HC.method = "POST"
    , HC.requestBody = HC.RequestBodyBS $ renderSimpleQuery False fields
    , HC.requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
    } "text/plain" (const $ return $ Just ()) (view rc)
  when (isNothing r) $
    logMsg t ("staticSendInvestigator: call failed" :: LogStr) (view rc)
  where
  fields =
    [ ("auth", Hash.digestToHexByteString $ Hash.hmacGetDigest $ key $ Fold.foldMap snd $ tail fields)
    , ("id", BSC.pack $ show $ partyId p)
    , ("name", TE.encodeUtf8 $ partyName p)
    , ("date", BSC.pack $ formatTime defaultTimeLocale "%B %e, %Y" t)
    , ("mail", TE.encodeUtf8 a)
    ]
staticSendInvestigator _ _ _ = return ()
