{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Databrary.Model.Transcode.Types
  ( Transcode(..)
  , TranscodePID
  , TranscodeArgs
  , transcodeId
  ) where

import qualified Data.ByteString as BS

import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Time
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Party.Types

type TranscodePID = Int32
type TranscodeArgs = [String]

type instance IdType Transcode = Int32

data Transcode = Transcode
  { transcodeAsset :: Asset
  , transcodeOwner :: SiteAuth
  , transcodeOrig :: Asset
  , transcodeSegment :: Segment
  , transcodeOptions :: TranscodeArgs
  , transcodeStart :: Maybe Timestamp
  , transcodeProcess :: Maybe TranscodePID
  , transcodeLog :: Maybe BS.ByteString
  }

transcodeId :: Transcode -> Id Transcode
transcodeId = Id . unId . assetId . transcodeAsset

instance Kinded Transcode where
  kindOf _ = "transcode"

