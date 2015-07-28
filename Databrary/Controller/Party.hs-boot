module Databrary.Controller.Party where

import Databrary.Action
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Controller.Paths

viewParty :: AppRoute (API, PartyTarget)
viewPartyEdit :: AppRoute PartyTarget
postParty :: AppRoute (API, PartyTarget)
createParty :: AppRoute API
deleteParty :: AppRoute (Id Party)
viewPartyDelete :: AppRoute (Id Party)
viewAvatar :: AppRoute (Id Party)
queryParties :: AppRoute API
adminParties :: AppRoute ()
