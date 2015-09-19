{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.EZID.API
  ( runEZIDM
  , ezidStatus
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.Foldable as Fold
import Data.Monoid (mempty)
import qualified Data.Traversable as Trav
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (methodGet)

import Databrary.Has
import Databrary.Service.Types
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
  httpRequestCookies req
    { HC.path = path
    , HC.method = method
    , HC.requestBody = HC.RequestBodyLBS $ B.toLazyByteString $ ANVL.encode body
    } "text/plain" $ \r -> P.maybeResult <$> P.parseWith (HC.responseBody r) ANVL.parse mempty

ezidStatus :: EZIDM Bool
ezidStatus =
  Fold.any (any (("success" ==) . fst)) <$> ezidCall "/status" methodGet []
