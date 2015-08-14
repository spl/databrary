module Databrary.Controller.Container where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Action

viewContainer :: AppRoute (API, (Maybe (Id Volume), Id Container))
postContainer :: AppRoute (API, Id Slot)
createContainer :: AppRoute (API, Id Volume)
