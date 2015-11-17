{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Activity.SQL
  ( selectActivityVolume
  , selectActivityAccess
  , activityQual
  ) where

import Data.List (stripPrefix)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Audit.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.VolumeAccess.SQL
import Databrary.Model.Activity.Types

delim :: String -> Bool
delim "" = True
delim (' ':_) = True
delim (',':_) = True
delim _ = False

makeActivity :: Timestamp -> AuditAction -> ActivityTarget -> PartyRow -> Activity
makeActivity t a x u = Activity t a u x Nothing

targetActivitySelector :: String -> Selector -> Selector
targetActivitySelector t Selector{ selectOutput = o, selectSource = ts, selectJoined = (',':tj) }
  | Just s <- stripPrefix t ts, delim s, ts == tj = selectJoin '($)
    [ selector ("audit." ++ ts) $ OutputJoin False 'makeActivity [SelectColumn t "audit_time", SelectColumn t "audit_action", o]
    , joinOn (t ++ ".audit_user = audit_party.id")
      (selectPartyRow `fromAlias` "audit_party")
    ]
targetActivitySelector t Selector{ selectSource = ts } = error $ "targetActivitySelector " ++ t ++ ": " ++ ts

selectActivityVolume :: Selector
selectActivityVolume = targetActivitySelector "volume" $
  selectMap (TH.ConE 'ActivityVolume `TH.AppE`) selectVolumeRow

selectActivityAccess :: TH.Name -> TH.Name -> Selector
selectActivityAccess vol ident = targetActivitySelector "volume_access" $
  selectMap (TH.ConE 'ActivityAccess `TH.AppE`) (selectVolumeAccess vol ident)

activityQual :: String
activityQual = "audit_action >= 'add' ORDER BY audit_time"
