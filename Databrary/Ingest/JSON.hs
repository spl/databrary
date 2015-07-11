{-# LANGUAGE OverloadedStrings #-}
module Databrary.Ingest.JSON
  ( ingestJSON
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (left)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), mapExceptT)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.JsonSchema as JS
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO (withBinaryFile, IOMode(ReadMode))

import Paths_databrary
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types

type IngestT m a = ExceptT [String] m a

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT = ExceptT . return

meither :: (Functor m, Monad m) => Either e a -> ExceptT [e] m a
meither = exceptT . left return

jsErr :: (Functor m, Monad m) => Either (V.Vector JS.ValErr) a -> IngestT m a
jsErr = exceptT . left (map T.unpack . V.toList)

loadSchema :: IngestT IO JS.Schema
loadSchema = do
  schema <- lift $ getDataFileName "volume.json"
  r <- lift $ withBinaryFile schema ReadMode (\h ->
    P.parseWith (BS.hGetSome h defaultChunkSize) JSON.json' BS.empty)
  js <- meither $ JSON.eitherJSON =<< P.eitherResult r
  let rs = JS.RawSchema "" js
  g <- ExceptT $ left (return . T.unpack) <$> JS.fetchRefs JS.draft4 rs mempty
  jsErr $ JS.compileDraft4 g rs

ingestJSON :: (MonadIO m, MonadDB m) => Volume -> JSON.Value -> Bool -> Bool -> IngestT m [Container]
ingestJSON vol jdata' run overwrite = do
  schema <- mapExceptT liftIO loadSchema
  jdata <- jsErr $ JS.validate schema jdata'
  return []
