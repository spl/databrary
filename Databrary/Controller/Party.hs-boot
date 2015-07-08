module Databrary.Controller.Party where

import Databrary.Action
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Controller.Paths

viewParty :: AppRoute (API, PartyTarget)
viewPartyEdit :: AppRoute PartyTarget
postParty :: AppRoute (API, PartyTarget)
createParty :: AppRoute API
queryParties :: AppRoute API
viewAvatar :: AppRoute (Id Party)
