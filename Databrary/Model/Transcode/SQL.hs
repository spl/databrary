{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Transcode.SQL
  ( selectOrigTranscode
  , selectTranscode
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

makeOrigTranscode :: Segment -> [Maybe String] -> Maybe Timestamp -> Maybe Int32 -> Maybe BS.ByteString -> SiteAuth -> (Volume -> Asset) -> Asset -> Transcode
makeOrigTranscode s f t p l u a o =
  Transcode (a $ assetVolume o) u o s (map (fromMaybe (error "NULL transcode options")) f) t p l

selectOrigTranscode :: Selector -- ^ @'Asset' -> 'Transcode'@
selectOrigTranscode = selectJoin 'id
  [ selectColumns 'makeOrigTranscode "transcode" ["segment", "options", "start", "process", "log"]
  , joinOn "transcode.owner = party.id"
    selectSiteAuth
  , joinOn "transcode.asset = asset.id"
    selectVolumeAsset
  ]

makeTranscode :: (Asset -> Transcode) -> (Volume -> Asset) -> (Permission -> Volume) -> Transcode
makeTranscode t o vp = t $ o $ vp PermissionADMIN

selectTranscode :: Selector -- ^ @'Transcode'@
selectTranscode = selectJoin 'makeTranscode
  [ selectOrigTranscode
  , joinOn "transcode.orig = orig.id"
    $ selectVolumeAsset `fromAlias` "orig"
  , selectMap (`TH.AppE` TH.ListE [])
    $ joinOn "asset.volume = volume.id AND orig.volume = volume.id"
      volumeRow
  ]
