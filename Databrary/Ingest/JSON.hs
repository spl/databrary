{-# LANGUAGE OverloadedStrings #-}
module Databrary.Ingest.JSON
  ( ingestJSON
  ) where

import Control.Arrow (left)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, mapExceptT, throwE)
import qualified Data.Aeson.BetterErrors as JE
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.Foldable as Fold
import qualified Data.JsonSchema as JS
import Data.Maybe (isNothing, fromJust)
import Data.Monoid (mempty, (<>))
import Data.Time.Format (parseTime)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Locale (defaultTimeLocale)
import System.IO (withBinaryFile, IOMode(ReadMode))

import Paths_databrary
import Databrary.Ops
import Databrary.Has (Has, view)
import qualified Databrary.JSON as J
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Audit
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Ingest

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
    P.parseWith (BS.hGetSome h defaultChunkSize) J.json' BS.empty)
  js <- ExceptT . return . left (return . T.pack) $ J.eitherJSON =<< P.eitherResult r
  let rs = JS.RawSchema "" js
  g <- ExceptT $ left return <$> JS.fetchRefs JS.draft4 rs mempty
  jsErr $ JS.compileDraft4 g rs

throwPE :: (Functor m, Monad m) => T.Text -> IngestParseT m a
throwPE = JE.throwCustomError

inObj :: forall a m . (Functor m, Kinded a, Has (Id a) a, Show (IdType a)) => a -> IngestParseT m a -> IngestParseT m a
inObj o = JE.mapError $ (<>) $ " for " <> kindOf o <> T.pack (' ' : show (view o :: Id a))

asKey :: (Functor m, Monad m) => IngestParseT m IngestKey
asKey = JE.asText

asDate :: (Functor m, Monad m) => IngestParseT m Date
asDate = JE.withString (maybe (Left "expecting %F") Right . parseTime defaultTimeLocale "%F")

ingestJSON :: (MonadIO m, MonadAudit c m) => Volume -> J.Value -> Bool -> Bool -> m (Either [T.Text] [Container])
ingestJSON vol jdata' run overwrite = runExceptT $ do
  schema <- mapExceptT liftIO loadSchema
  jdata <- jsErr $ JS.validate schema jdata'
  if run
    then ExceptT $ left (JE.displayError id) <$> JE.parseValueM volume jdata
    else return []
  where
  update cur change new =
    liftParse $ Fold.forM_ new $ \v ->
      when (Fold.all (v /=) cur) $ do
        unless (overwrite || isNothing cur) $
          throwE $ "conflicting value: " <> T.pack (show v) <> " <> " <> T.pack (show (fromJust cur))
        lift $ change v
  volume = do
    _ <- JE.keyMay "name" JE.asText >>= update (Just $ volumeName vol) (\n -> changeVolume vol{ volumeName = n })
    JE.key "containers" $ JE.eachInArray container
  container = do
    cid <- JE.keyMay "id" $ Id <$> JE.asIntegral
    key <- JE.key "key" $ asKey
    top <- JE.keyOrDefault "top" False JE.asBool
    name <- JE.keyMay "name" JE.asText
    date <- JE.keyMay "date" asDate
    c <- maybe
      (lift $ do
        c <- addContainer (blankContainer vol)
          { containerTop = top
          , containerName = name
          , containerDate = date
          }
        addIngestContainer c key
        return c)
      (\c -> inObj c $ do
        unless (Fold.all (containerId c ==) cid) $
          throwPE "id mismatch"
        return c)
      =<< lift (lookupIngestContainer vol key)
      
    return c
