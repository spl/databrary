{-# LANGUAGE OverloadedStrings #-}
module Databrary.Ingest.JSON
  ( ingestJSON
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (left)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, mapExceptT)
import qualified Data.Aeson.BetterErrors as JE
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

type IngestT m a = ExceptT T.Text m a
type IngestParseT m a = JE.ParseT T.Text m a

liftParse :: (Functor m, Monad m) => IngestT m a -> IngestParseT m a
liftParse f = lift (runExceptT f) >>= either JE.throwCustomError return

jsErr :: (Functor m, Monad m) => Either (V.Vector JS.ValErr) a -> ExceptT [T.Text] m a
jsErr = ExceptT . return . left V.toList

loadSchema :: ExceptT [T.Text] IO JS.Schema
loadSchema = do
  schema <- lift $ getDataFileName "volume.json"
  r <- lift $ withBinaryFile schema ReadMode (\h ->
    P.parseWith (BS.hGetSome h defaultChunkSize) JSON.json' BS.empty)
  js <- ExceptT . return . left (return . T.pack) $ JSON.eitherJSON =<< P.eitherResult r
  let rs = JS.RawSchema "" js
  g <- ExceptT $ left return <$> JS.fetchRefs JS.draft4 rs mempty
  jsErr $ JS.compileDraft4 g rs

ingestJSON :: (MonadIO m, MonadDB m) => Volume -> JSON.Value -> Bool -> Bool -> m (Either [T.Text] [Container])
ingestJSON vol jdata' run overwrite = runExceptT $ do
  schema <- mapExceptT liftIO loadSchema
  jdata <- jsErr $ JS.validate schema jdata'
  if run
    then ExceptT $ left (JE.displayError id) <$> JE.parseValueM ingest jdata
    else return []
  where
  ingest = return []
