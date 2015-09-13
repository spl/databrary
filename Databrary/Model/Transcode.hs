{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Transcode
  ( module Databrary.Model.Transcode.Types
  , defaultTranscodeOptions
  , transcodeAuth
  , lookupTranscode
  , lookupActiveTranscodes
  , addTranscode
  , updateTranscode
  , findTranscode
  , findMatchingTranscode
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Ops
import Databrary.Has (peek)
import Databrary.Service.DB
import Databrary.Service.Types
import Databrary.Service.Crypto
import Databrary.Store.Types
import Databrary.Store.AV
import Databrary.Store.Probe
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Asset
import Databrary.Model.Transcode.Types
import Databrary.Model.Transcode.SQL

defaultTranscodeOptions :: TranscodeArgs
defaultTranscodeOptions = ["-vf", "pad=iw+mod(iw\\,2):ih+mod(ih\\,2)"]

transcodeAuth :: Transcode -> Secret -> BS.ByteString
transcodeAuth t = signature $ BSL.toStrict $ BSB.toLazyByteString
  $ maybe id ((<>) . BSB.byteString) (assetSHA1 $ transcodeOrig t)
  $ BSB.int32LE (unId $ transcodeId t)

lookupTranscode :: MonadDB c m => Id Transcode -> m (Maybe Transcode)
lookupTranscode a =
  dbQuery1 $(selectQuery selectTranscode "WHERE transcode.asset = ${a}")

lookupActiveTranscodes :: MonadDB c m => m [Transcode]
lookupActiveTranscodes =
  dbQuery $(selectQuery selectTranscode "WHERE asset.size IS NULL")

minAppend :: Ord a => Maybe a -> Maybe a -> Maybe a
minAppend (Just x) (Just y) = Just $ min x y
minAppend Nothing x = x
minAppend x Nothing = x

addTranscode :: (MonadHasSiteAuth c m, MonadAudit c m, MonadStorage c m) => Asset -> Segment -> TranscodeArgs -> Probe -> m Transcode
addTranscode orig seg@(Segment rng) opts (ProbeAV _ fmt av) = do
  own <- peek
  a <- addAsset orig
    { assetFormat = fmt
    , assetDuration = dur
    , assetSHA1 = Nothing
    , assetSize = Nothing
    } Nothing
  dbExecute1' [pgSQL|INSERT INTO transcode (asset, owner, orig, segment, options) VALUES (${assetId a}, ${partyId $ accountParty $ siteAccount own}, ${assetId orig}, ${seg}, ${map Just opts})|]
  _ <- dbExecute1 [pgSQL|UPDATE slot_asset SET asset = ${assetId a}, segment = segment(lower(segment) + ${fromMaybe 0 $ lowerBound rng}, COALESCE(lower(segment) + ${upperBound rng}, upper(segment))) WHERE asset = ${assetId orig}|]
  return Transcode
    { transcodeAsset = a
    , transcodeOwner = own
    , transcodeOrig = orig
    , transcodeSegment = seg
    , transcodeOptions = opts
    , transcodeStart = Nothing -- actually now, whatever
    , transcodeProcess = Nothing
    , transcodeLog = Nothing
    }
  where
  dur = maybe id (flip (-) . max 0) (lowerBound rng) <$>
    minAppend (avProbeLength av) (upperBound rng)
addTranscode _ _ _ _ = fail "addTranscode: invalid probe type"

updateTranscode :: MonadDB c m => Transcode -> Maybe TranscodePID -> Maybe String -> m Transcode
updateTranscode tc pid logs = do
  r <- dbQuery1 [pgSQL|UPDATE transcode SET process = ${pid}, log = COALESCE(COALESCE(log || E'\n', '') || ${logs}, log) WHERE asset = ${assetId $ transcodeAsset tc} AND COALESCE(process, 0) = ${fromMaybe 0 $ transcodeProcess tc} RETURNING log|]
  return $ maybe tc (\l -> tc
    { transcodeProcess = pid
    , transcodeLog = l
    }) r

findTranscode :: MonadDB c m => Asset -> Segment -> TranscodeArgs -> m (Maybe Transcode)
findTranscode orig seg opts =
  dbQuery1 $ ($ orig) <$> $(selectQuery selectOrigTranscode "WHERE transcode.orig = ${assetId orig} AND transcode.segment = ${seg} AND transcode.options = ${map Just opts} AND asset.volume = ${volumeId $ assetVolume orig} LIMIT 1")

findMatchingTranscode :: MonadDB c m => Transcode -> m (Maybe Transcode)
findMatchingTranscode Transcode{..} =
  dbQuery1 $(selectQuery selectTranscode "WHERE orig.sha1 = ${assetSHA1 transcodeOrig} AND transcode.segment = ${transcodeSegment} AND transcode.options = ${map Just transcodeOptions} AND asset.id < ${assetId transcodeAsset} ORDER BY asset.id LIMIT 1")

