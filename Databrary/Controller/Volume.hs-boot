module Databrary.Controller.Volume where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Action

viewVolume :: AppRoute (API, Id Volume)
viewVolumeEdit :: AppRoute (Id Volume)
postVolume :: AppRoute (API, Id Volume)
createVolume :: AppRoute API
viewVolumeLinks :: AppRoute (Id Volume)
postVolumeLinks :: AppRoute (API, Id Volume)
thumbVolume :: AppRoute (Id Volume)
queryVolumes :: AppRoute API
