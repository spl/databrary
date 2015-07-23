{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.CSV
  ( csvVolume
  ) where

import Control.Arrow (second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.List (foldl', nubBy, groupBy)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (hContentType)

import Databrary.Ops
import Databrary.Store.Filename
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Model.RecordSlot
import Databrary.Model.Metric
import Databrary.Model.Measure
import Databrary.Store.CSV
import Databrary.HTTP
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Volume

type Records = [[Record]]
type Metrics = [[Metric]]
type Header = (Maybe RecordCategory, Metrics)
type Headers = [Header]

tshow :: Show a => a -> BS.ByteString
tshow = BSC.pack . show

tmaybe :: (a -> BS.ByteString) -> Maybe a -> BS.ByteString
tmaybe = maybe BS.empty

tenc :: T.Text -> BS.ByteString
tenc = TE.encodeUtf8

recordMetrics :: Record -> [Metric]
recordMetrics = map measureMetric . recordMeasures

recordsMetrics :: [Record] -> [[Metric]]
recordsMetrics = map recordMetrics

updateMetrics :: Metrics -> [Record] -> Metrics
updateMetrics m [] = m
updateMetrics [] r = recordsMetrics r
updateMetrics (m:ml) (r:rl) = mergeBy compare m (recordMetrics r) : updateMetrics ml rl

updateHeaders :: Headers -> Records -> Headers
updateHeaders h [] = h
updateHeaders [] l = map (\rl@(r:_) -> (recordCategory r, recordsMetrics rl)) l
updateHeaders hl@(cm@(c,m):hl') rll@(~rl@(r:_):rll') = case compare c rc of
  LT -> cm                      : updateHeaders hl' rll
  EQ -> (c, updateMetrics m rl) : updateHeaders hl' rll'
  GT -> (rc, recordsMetrics rl) : updateHeaders hl rll'
  where rc = recordCategory r

metricHeader :: [Metric] -> [BS.ByteString]
metricHeader = map (tenc . metricName)

metricsHeader :: BS.ByteString -> Metrics -> [BS.ByteString]
metricsHeader p [m] = map (BSC.snoc p '-' <>) $ metricHeader m
metricsHeader p ml = mh 0 ml where
  mh _ [] = []
  mh i (m:l) = map (p' <>) (metricHeader m) ++ mh i' l where
    p' = p <> BSC.snoc (tshow i') '-'
    i' = succ i :: Integer

headerRow :: Headers -> [BS.ByteString]
headerRow = concatMap $ uncurry $ metricsHeader . maybe "record" (tenc . recordCategoryName)

metricsRow :: [Metric] -> [Measure] -> [BS.ByteString]
metricsRow m [] = map (const BS.empty) m
metricsRow ~mh@(m:h) dl@(d:l) = case compare m dm of
  LT -> BS.empty : metricsRow h dl
  EQ -> measureDatum d : metricsRow h l
  GT -> error "csvMetricsRow" : metricsRow mh l
  where dm = measureMetric d

recordsRow :: Metrics -> [Record] -> [BS.ByteString]
recordsRow h [] = concatMap (`metricsRow` []) h
recordsRow ~(h:hl) (r:rl) = metricsRow h (recordMeasures r) ++ recordsRow hl rl

dataRow :: Headers -> Records -> [BS.ByteString]
dataRow _ [] = []
dataRow ~hl@((c,m):hl') rll@(~rl@(r:_):rll') = case compare c rc of
  LT -> recordsRow m [] ++ dataRow hl' rll
  EQ -> recordsRow m rl ++ dataRow hl' rll'
  GT -> error "csvDataRow" ++ dataRow hl rll'
  where rc = recordCategory r

csvVolume :: AppRoute (Id Volume)
csvVolume = action GET (pathId </< "csv") $ \vi -> withAuth $ do
  vol <- getVolume PermissionPUBLIC vi
  crsl <- lookupVolumeContainersRecords vol
  let grm r = r{ recordMeasures = getRecordMeasures r }
      crl = map (second $ map (nubBy ((==) `on` recordId)) . groupBy ((==) `on` recordCategory) . map (grm . slotRecord)) crsl
      hl = foldl' updateHeaders [] $ map snd crl
      cr c r = tshow (containerId c) : tmaybe tenc (containerName c) : tmaybe BSC.pack (formatContainerDate c) : tmaybe tshow (containerRelease c) : dataRow hl r
      hr = "session-id" : "session-name" : "session-date" : "session-release" : headerRow hl
      csv = hr : map (uncurry cr) crl
  okResponse 
    [ (hContentType, "text/csv;charset=utf-8")
    , ("content-disposition", "attachment; filename=" <> quoteHTTP (makeFilename (volumeDownloadName vol) <> ".csv"))
    ]
    $ buildCSV csv
