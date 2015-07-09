{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Transcode.SQL
  ( selectTranscode
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Segment
import Databrary.Model.Transcode.Types

makeTranscode :: Segment -> [Maybe String] -> Maybe Timestamp -> Maybe Int32 -> Maybe BS.ByteString -> SiteAuth -> (Volume -> Asset) -> (Volume -> Asset) -> (Permission -> Volume) -> Transcode
makeTranscode s f t p l u a o vp =
  Transcode (a v) u (o v) s (map (fromMaybe (error "NULL transcode options")) f) t p l
  where v = vp PermissionADMIN

selectTranscode :: Selector -- ^ @'Transcode'@
selectTranscode = selectJoin 'id
  [ selectColumns 'makeTranscode "transcode" ["segment", "options", "start", "process", "log"]
  , joinOn "transcode.owner = party.id"
    selectSiteAuth
  , joinOn "transcode.asset = asset.id"
    selectVolumeAsset
  , joinOn "transcode.orig = orig.id"
    $ selectVolumeAsset `fromAlias` "orig"
  , selectMap (`TH.AppE` TH.ListE [])
    $ joinOn "asset.volume = volume.id AND orig.volume = volume.id"
      volumeRow
  ]
