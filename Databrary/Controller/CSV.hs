{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.CSV
  ( csvVolume
  ) where

import Control.Arrow (second)
import Data.Function (on)
import Data.List (foldl', nubBy, groupBy)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TLE
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

tshow :: Show a => a -> T.Text
tshow = T.pack . show

tmaybe :: (a -> T.Text) -> Maybe a -> T.Text
tmaybe = maybe T.empty

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

metricHeader :: [Metric] -> [T.Text]
metricHeader = map metricName

metricsHeader :: T.Text -> Metrics -> [T.Text]
metricsHeader p [m] = map (T.snoc p '-' <>) $ metricHeader m
metricsHeader p ml = mh 0 ml where
  mh _ [] = []
  mh i (m:l) = map (p' <>) (metricHeader m) ++ mh i' l where
    p' = p <> T.snoc (tshow i') '-'
    i' = succ i :: Integer

headerRow :: Headers -> [T.Text]
headerRow = concatMap $ uncurry $ metricsHeader . maybe "record" recordCategoryName

metricsRow :: [Metric] -> [Measure] -> [T.Text]
metricsRow m [] = map (const T.empty) m
metricsRow (_:h) (m:l) = measureDatum m : metricsRow h l

recordsRow :: Metrics -> [Record] -> [T.Text]
recordsRow h [] = concatMap (`metricsRow` []) h
recordsRow (h:hl) (r:rl) = metricsRow h r ++ recordsRow hl rl

dataRow :: Headers -> Records -> [T.Text]
dataRow _ [] = []
dataRow hl@(~cm@(c,m):hl') rll@(~rl@(r:_):rll') = case compare c rc of
  LT -> recordsRow m [] ++ dataRow hl' rll
  EQ -> recordsRow m rl ++ dataRow hl' rll'
  GT -> error "csvDataRow" ++ dataRow hl rll'
  where rc = recordCategory r

csvVolume :: AppRoute (Id Volume)
csvVolume = action GET (pathId </< "csv") $ \vi -> withAuth $ do
  vol <- getVolume PermissionPUBLIC vi
  name <- volumeDownloadName vol
  crsl <- lookupVolumeContainersRecords vol
  let grm r = r{ recordMeasures = getRecordMeasures r }
      crl = map (second (map (nubBy ((==) `on` recordId)) . groupBy ((==) `on` recordCategory) . map (grm . slotRecord))) crsl
      hl = foldl' updateHeaders [] $ map snd crl
      cr c r = tshow (containerId c) : tmaybe id (containerName c) : tmaybe T.pack (formatContainerDate c) : tmaybe tshow (containerRelease c) : dataRow hl r
      hr = "session-id" : "session-name" : "session-date" : "session-release" : headerRow hl
      csv = hr : map (uncurry cr) crl
  okResponse 
    [ (hContentType, "text/csv;charset=utf-8")
    , ("content-disposition", "attachment; filename=" <> quoteHTTP (makeFilename name <> ".csv"))
    ]
    $ TLE.encodeUtf8Builder $ TB.toLazyText $ buildCSV csv
