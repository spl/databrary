module Databrary.Controller.AssetSegment where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Action

viewAssetSegment :: AppRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
serveAssetSegment :: Bool -> AssetSegment -> AppAction
downloadAssetSegment :: AppRoute (Id Slot, Id Asset)
