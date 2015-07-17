{-# LANGUAGE OverloadedStrings #-}
module Databrary.Ingest.JSON
  ( ingestJSON
  ) where

import Control.Arrow (left)
import Control.Monad (when, unless)
import Control.Monad.Except (ExceptT(..), runExceptT, mapExceptT, throwError, catchError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson.BetterErrors as JE
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.Foldable as Fold
import Data.Function (on)
import qualified Data.JsonSchema as JS
import Data.List (find)
import Data.Monoid (mempty, (<>))
import Data.Time.Format (parseTime)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Locale (defaultTimeLocale)
import System.IO (withBinaryFile, IOMode(ReadMode))

import Paths_databrary
import Databrary.Ops
import Databrary.Has (Has, view, focusIO)
import qualified Databrary.JSON as J
import Databrary.Files
import Databrary.Store.Types
import Databrary.Store.Stage
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Audit
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Release
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Model.RecordSlot
import Databrary.Model.Format
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

inObj :: forall a b m . (Functor m, Kinded a, Has (Id a) a, Show (IdType a)) => a -> IngestParseT m b -> IngestParseT m b
inObj o = JE.mapError $ (<>) $ " for " <> kindOf o <> T.pack (' ' : show (view o :: Id a))

asKey :: (Functor m, Monad m) => IngestParseT m IngestKey
asKey = JE.asText

asDate :: (Functor m, Monad m) => IngestParseT m Date
asDate = JE.withString (maybe (Left "expecting %F") Right . parseTime defaultTimeLocale "%F")

asRelease :: (Functor m, Monad m) => IngestParseT m (Maybe Release)
asRelease = JE.perhaps JE.fromAesonParser

asCategory :: (Functor m, Monad m) => IngestParseT m (Maybe RecordCategory)
asCategory = JE.perhaps $ do
  JE.withIntegral (maybe (Left "category not found") Right . getRecordCategory . Id) `catchError` \_ -> do
    n <- JE.asText
    fromMaybeM (throwPE "category not found") $ find ((n ==) . recordCategoryName) allRecordCategories

asSegment :: (Functor m, Monad m) => IngestParseT m Segment
asSegment = JE.fromAesonParser

data StageFile = StageFile
  { stageFileRel :: FilePath
  , stageFileAbs :: FilePath
  }

asStageFile :: (MonadStorage c m) => FilePath -> IngestParseT m StageFile
asStageFile b = do
  r <- (b </>) <$> JE.asString
  a <- fromMaybeM (throwPE "stage file not found") =<< lift (focusIO (stageFile r))
  return $ StageFile r a

ingestJSON :: (MonadAudit c m, MonadStorage c m) => Volume -> J.Value -> Bool -> Bool -> m (Either [T.Text] [Container])
ingestJSON vol jdata' run overwrite = runExceptT $ do
  schema <- mapExceptT liftIO loadSchema
  jdata <- jsErr $ JS.validate schema jdata'
  if run
    then ExceptT $ left (JE.displayError id) <$> JE.parseValueM volume jdata
    else return []
  where
  check a b
    | a == b = return False
    | not overwrite = throwPE $ "conflicting value: " <> T.pack (show b) <> " <> " <> T.pack (show a)
    | otherwise = return True
  volume = do
    dir <- JE.keyOrDefault "directory" "" $ stageFileRel <$> asStageFile ""
    _ <- JE.keyMay "name" $ do
      name <- JE.asText
      name' <- check (volumeName vol) name
      when name' $ lift $ changeVolume vol{ volumeName = name }
    JE.key "containers" $ JE.eachInArray (container dir)
  container dir = do
    cid <- JE.keyMay "id" $ Id <$> JE.asIntegral
    key <- JE.key "key" $ asKey
    top <- JE.keyOrDefault "top" False JE.asBool
    name <- JE.keyMay "name" JE.asText
    date <- JE.keyMay "date" asDate
    c <- maybe
      (do
        c <- maybe
          (lift $ addContainer (blankContainer vol)
            { containerTop = top
            , containerName = name
            , containerDate = date
            })
          (\i -> fromMaybeM (throwPE $ "container " <> T.pack (show i) <> "/" <> key <> " not found")
            =<< lift (lookupVolumeContainer vol i))
          cid
        inObj c $ lift $ addIngestContainer c key
        return c)
      (\c -> inObj c $ do
        unless (Fold.all (containerId c ==) cid) $ do
          throwPE "id mismatch"
        top' <- check (containerTop c) top
        name' <- check (containerName c) name
        date' <- check (containerDate c) date
        when (top' || name' || date') $ lift $ changeContainer c
          { containerTop = top
          , containerName = name
          , containerDate = date
          }
        return c)
      =<< lift (lookupIngestContainer vol key)
    inObj c $ do
      _ <- JE.keyMay "release" $ do
        release <- asRelease
        release' <- check (containerRelease c) release
        when release' $ do
          r <- lift $ changeRelease (containerSlot c) release
          unless r $ throwPE "update failed"
      _ <- JE.key "records" $ JE.eachInArray $ do
        r <- record
        seg <- JE.keyOrDefault "position" fullSegment asSegment
        o <- lift $ moveRecordSlot (RecordSlot r (Slot c emptySegment)) seg
        unless o $ throwPE "record link failed"
      _ <- JE.key "assets" $ JE.eachInArray $ do
        a <- asset dir
        return a
      return c
  record = do
    rid <- JE.keyMay "id" $ Id <$> JE.asIntegral
    key <- JE.key "key" $ asKey
    category <- JE.key "category" asCategory
    r <- maybe
      (do
        r <- maybe
          (lift $ addRecord (blankRecord vol)
            { recordCategory = category
            })
          (\i -> fromMaybeM (throwPE $ "record " <> T.pack (show i) <> "/" <> key <> " not found")
            =<< lift (lookupVolumeRecord vol i))
          rid
        inObj r $ lift $ addIngestRecord r key
        return r)
      (\r -> inObj r $ do
        unless (Fold.all (recordId r ==) rid) $ do
          throwPE "id mismatch"
        category' <- on check (fmap recordCategoryName) (recordCategory r) category
        when category' $ lift $ changeRecord r
          { recordCategory = category
          }
        return r)
      =<< lift (lookupIngestRecord vol key)
    inObj r $
      return ()
    return r
  asset dir = do
    (file, fmt) <- JE.key "file" $ do
      file <- asStageFile dir
      fmt <- fromMaybeM (throwPE "unknown file format") $ getFormatByFilename $ toRawFilePath $ stageFileRel file
      return (file, fmt)
    return ()
