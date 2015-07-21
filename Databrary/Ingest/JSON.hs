{-# LANGUAGE OverloadedStrings #-}
module Databrary.Ingest.JSON
  ( ingestJSON
  ) where

import Control.Arrow (left)
import Control.Monad (join, when, unless, void)
import Control.Monad.Except (ExceptT(..), runExceptT, mapExceptT, catchError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Aeson.BetterErrors as JE
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.Foldable as Fold
import Data.Function (on)
import qualified Data.JsonSchema as JS
import Data.List (find)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (mempty, (<>))
import Data.Time.Format (parseTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Database.PostgreSQL.Typed.Range as Range
import System.Locale (defaultTimeLocale)
import System.IO (withBinaryFile, IOMode(ReadMode))

import Paths_databrary
import Databrary.Ops
import Databrary.Has (Has, view, focusIO)
import qualified Databrary.JSON as J
import Databrary.Files
import Databrary.Store.Stage
import Databrary.Store.Probe
import Databrary.Store.Transcode
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Release
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.Model.Measure
import Databrary.Model.RecordSlot
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.Transcode
import Databrary.Model.Ingest
import Databrary.Action.Auth

type IngestM a = JE.ParseT T.Text (ReaderT AuthRequest IO) a

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

throwPE :: T.Text -> IngestM a
throwPE = JE.throwCustomError

inObj :: forall a b . (Kinded a, Has (Id a) a, Show (IdType a)) => a -> IngestM b -> IngestM b
inObj o = JE.mapError $ (<>) $ " for " <> kindOf o <> T.pack (' ' : show (view o :: Id a))

noKey :: T.Text -> IngestM ()
noKey k = void $ JE.keyMay k $ throwPE "unhandled value"

asKey :: IngestM IngestKey
asKey = JE.asText

asDate :: IngestM Date
asDate = JE.withString (maybe (Left "expecting %F") Right . parseTime defaultTimeLocale "%F")

asRelease :: IngestM (Maybe Release)
asRelease = JE.perhaps JE.fromAesonParser

asCategory :: IngestM (Maybe RecordCategory)
asCategory = JE.perhaps $ do
  JE.withIntegral (err . getRecordCategory . Id) `catchError` \_ -> do
    JE.withText (\n -> err $ find ((n ==) . recordCategoryName) allRecordCategories)
  where err = maybe (Left "category not found") Right

asSegment :: IngestM Segment
asSegment = JE.fromAesonParser

data StageFile = StageFile
  { stageFileRel :: FilePath
  , stageFileAbs :: FilePath
  }

asStageFile :: FilePath -> IngestM StageFile
asStageFile b = do
  r <- (b </>) <$> JE.asString
  a <- fromMaybeM (throwPE "stage file not found") =<< lift (focusIO (stageFile r))
  return $ StageFile r a

ingestJSON :: Volume -> J.Value -> Bool -> Bool -> AuthActionM (Either [T.Text] [Container])
ingestJSON vol jdata' run overwrite = runExceptT $ do
  schema <- mapExceptT liftIO loadSchema
  jdata <- jsErr $ JS.validate schema jdata'
  if run
    then ExceptT $ left (JE.displayError id) <$> JE.parseValueM volume jdata
    else return []
  where
  check cur new
    | cur == new = return Nothing
    | not overwrite = throwPE $ "conflicting value: " <> T.pack (show new) <> " <> " <> T.pack (show cur)
    | otherwise = return $ Just new
  volume = do
    dir <- JE.keyOrDefault "directory" "" $ stageFileRel <$> asStageFile ""
    _ <- JE.keyMay "name" $ do
      name <- check (volumeName vol) =<< JE.asText
      Fold.forM_ name $ \n -> lift $ changeVolume vol{ volumeName = n }
    JE.key "containers" $ JE.eachInArray (container dir)
  container dir = do
    cid <- JE.keyMay "id" $ Id <$> JE.asIntegral
    key <- JE.key "key" $ asKey
    c <- maybe
      (do
        c <- maybe
          (do
            top <- JE.keyOrDefault "top" False JE.asBool
            name <- JE.keyMay "name" JE.asText
            date <- JE.keyMay "date" asDate
            lift $ addContainer (blankContainer vol)
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
        top <- fmap join . JE.keyMay "top" $ check (containerTop c) =<< JE.asBool
        name <- fmap join . JE.keyMay "name" $ check (containerName c) =<< JE.perhaps JE.asText
        date <- fmap join . JE.keyMay "date" $ check (containerDate c) =<< JE.perhaps asDate
        when (isJust top || isJust name || isJust date) $ lift $ changeContainer c
          { containerTop = fromMaybe (containerTop c) top
          , containerName = fromMaybe (containerName c) name
          , containerDate = fromMaybe (containerDate c) date
          }
        return c)
      =<< lift (lookupIngestContainer vol key)
    inObj c $ do
      _ <- JE.keyMay "release" $ do
        release <- check (containerRelease c) =<< asRelease
        Fold.forM_ release $ \r -> do
          o <- lift $ changeRelease (containerSlot c) r
          unless o $ throwPE "update failed"
      _ <- JE.key "records" $ JE.eachInArray $ do
        r <- record
        inObj r $ do
          seg <- JE.keyOrDefault "position" fullSegment asSegment
          o <- lift $ moveRecordSlot (RecordSlot r (Slot c emptySegment)) seg
          unless o $ throwPE "record link failed"
      _ <- JE.key "assets" $ JE.eachInArray $ do
        a <- asset dir
        inObj a $ do
          as' <- lift $ lookupAssetAssetSlot a
          seg <- JE.keyOrDefault "position" fullSegment $
            JE.withTextM (\t -> if t == "auto" then Right . Segment . Range.point <$> findAssetContainerEnd c else return $ Left "invalid asset position")
              `catchError` \_ -> asSegment
          let seg'
                | Just p <- Range.getPoint (segmentRange seg)
                , Just d <- assetDuration a = Segment $ Range.bounded p (p + d)
                | otherwise = seg
              s = Slot c seg'
          u <- maybe (return True) (\s' -> isJust <$> on check slotId s' s) $ assetSlot as'
          when u $ do
            o <- lift $ changeAssetSlot $ AssetSlot a $ Just s
            unless o $ throwPE "asset link failed"
      return c
  record = do
    rid <- JE.keyMay "id" $ Id <$> JE.asIntegral
    key <- JE.key "key" $ asKey
    r <- maybe
      (do
        r <- maybe
          (do
            category <- JE.key "category" asCategory
            lift $ addRecord (blankRecord vol)
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
        _ <- JE.key "category" $ do
          category <- asCategory
          category' <- (category <$) <$> on check (fmap recordCategoryName) (recordCategory r) category
          Fold.forM_ category' $ \c -> lift $ changeRecord r
            { recordCategory = c
            }
        return r)
      =<< lift (lookupIngestRecord vol key)
    _ <- inObj r $ JE.forEachInObject $ \m ->
      unless (m `elem` ["id", "key", "category", "position"]) $ do
        metric <- fromMaybeM (throwPE "metric not found") $ find ((m ==) . metricName) allMetrics
        datum <- maybe (return . Just) (check . measureDatum) (getMeasure metric (recordMeasures r)) . TE.encodeUtf8 =<< JE.asText
        Fold.forM_ datum $ lift . changeRecordMeasure . Measure r metric
    return r
  asset dir = do
    (file, probe) <- JE.key "file" $ do
      file <- asStageFile dir
      either throwPE (return . (,) file)
        =<< lift (probeFile (toRawFilePath $ stageFileRel file) (toRawFilePath $ stageFileAbs file))
    orig <- JE.keyMay "replace" $
      let err = fmap $ maybe (Left "asset not found") Right in
      JE.withIntegralM (err . lookupVolumeAsset vol . Id) `catchError` \_ ->
        JE.withStringM (err . lookupIngestAsset vol)
    ae <- lift $ lookupIngestAsset vol $ stageFileRel file
    a <- maybe
      (do
        release <- JE.key "release" asRelease
        name <- JE.keyMay "name" JE.asText
        a <- lift $ addAsset (blankAsset vol)
          { assetFormat = probeFormat probe
          , assetRelease = release
          , assetName = name
          } (Just $ toRawFilePath $ stageFileAbs file)
        lift $ addIngestAsset a (stageFileRel file)
        Fold.forM_ orig $ \o -> lift $ supersedeAsset o a
        return a)
      (\a -> inObj a $ do
        unless (assetBacked a) $ throwPE "ingested asset incomplete"
        -- compareFiles file =<< getAssetFile -- assume correct
        release <- fmap join . JE.keyMay "release" $ check (assetRelease a) =<< asRelease
        name <- fmap join . JE.keyMay "name" $ check (assetName a) =<< JE.perhaps JE.asText
        a' <- if isJust release || isJust name
          then lift $ changeAsset a
            { assetRelease = fromMaybe (assetRelease a) release
            , assetName = fromMaybe (assetName a) name
            } Nothing
          else return a
        Fold.forM_ orig $ \o -> lift $ replaceSlotAsset o a'
        return a')
      ae
    t <- inObj a $ case probe of
      ProbePlain _ -> do
        noKey "clip"
        noKey "options"
        return a
      ProbeVideo _ av -> do
        clip <- JE.keyOrDefault "clip" fullSegment asSegment
        opts <- JE.keyOrDefault "options" defaultTranscodeOptions $ JE.eachInArray JE.asString
        t <- lift $ fromMaybeM
          (do
            t <- addTranscode a clip opts av
            _ <- startTranscode t
            return t)
          =<< flatMapM (\_ -> findTranscode a clip opts) ae
        return $ transcodeAsset t
    return t
