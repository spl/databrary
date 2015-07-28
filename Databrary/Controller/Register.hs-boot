module Databrary.Controller.Register where

import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Action

viewRegister :: AppRoute ()
postRegister :: AppRoute API
viewPasswordReset :: AppRoute ()
postPasswordReset :: AppRoute API
resendInvestigator :: AppRoute (Id Party)
