{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Activity.SQL
  ( selectActivityParty
  , selectActivityAccount
  , selectActivityAuthorize
  , selectActivityVolume
  , selectActivityAccess
  , selectActivityContainer
  , activityQual
  ) where

import Data.List (stripPrefix)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.Types
import Databrary.Model.Audit.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Authorize.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.VolumeAccess.SQL
import Databrary.Model.Container.SQL
import Databrary.Model.Activity.Types

delim :: String -> Bool
delim "" = True
delim (' ':_) = True
delim (',':_) = True
delim _ = False

makeActivity :: Audit -> ActivityTarget -> PartyRow -> Activity
makeActivity a x u = Activity a u x Nothing

targetActivitySelector :: String -> Selector -> Selector
targetActivitySelector t Selector{ selectOutput = o, selectSource = ts, selectJoined = (',':tj) }
  | Just s <- stripPrefix t ts, delim s, ts == tj = selectJoin '($)
    [ selector ("audit." ++ ts) $ OutputJoin False 'makeActivity [selectOutput (selectAudit t), o]
    , joinOn (t ++ ".audit_user = audit_party.id")
      (selectPartyRow `fromAlias` "audit_party")
    ]
targetActivitySelector t Selector{ selectSource = ts } = error $ "targetActivitySelector " ++ t ++ ": " ++ ts

selectActivityParty :: Selector
selectActivityParty = targetActivitySelector "party" $
  selectMap (TH.ConE 'ActivityParty `TH.AppE`) selectPartyRow

selectActivityAccount :: Selector
selectActivityAccount = targetActivitySelector "account" $
  selectColumns 'ActivityAccount "account" ["id", "email", "password"]

selectActivityAuthorize :: TH.Name -> TH.Name -> Selector
selectActivityAuthorize p ident = targetActivitySelector "authorize" $
  selectMap (TH.ConE 'ActivityAuthorize `TH.AppE`) (selectAuthorizeChild p ident)

selectActivityVolume :: Selector
selectActivityVolume = targetActivitySelector "volume" $
  selectMap (TH.ConE 'ActivityVolume `TH.AppE`) selectVolumeRow

selectActivityAccess :: TH.Name -> TH.Name -> Selector
selectActivityAccess vol ident = targetActivitySelector "volume_access" $
  selectMap (TH.ConE 'ActivityAccess `TH.AppE`) (selectVolumeAccess vol ident)

selectActivityContainer :: Selector
selectActivityContainer = targetActivitySelector "container" $
  selectMap (TH.ConE 'ActivityContainer `TH.AppE`) selectContainerRow

activityQual :: String
activityQual = "audit_action >= 'add' ORDER BY audit_time"
