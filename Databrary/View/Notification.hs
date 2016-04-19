{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Notification
  ( htmlNotification
  ) where

import Control.Arrow (second)
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.Maybe (fromMaybe)
import qualified Text.Blaze.Html5 as H

import Databrary.Ops
import Databrary.Model.Notification
import Databrary.Model.Party.Types
import Databrary.View.Party (htmlPartyViewLink)
import Databrary.Controller.Paths
import Databrary.Controller.Party
import Databrary.View.Html

htmlNotification :: Notification -> H.Html
htmlNotification Notification{..} = case notificationNotice of
  NoticeAccountChange -> agent >> " changed " >> partys >> " " >> partyEdit (fromMaybe target notificationParty) [("page", "account")] "account information" >> "."
  NoticeAuthorizeRequest -> agent >> " requested " >> partyEdit target [("page", "grant"), ("party", maybe "" (BSC.pack . show . partyId) notificationParty)] "authorization" >> " for " >> party >> "."
  NoticeAuthorizeGranted -> agent >> " granted " >> partyEdit target [("page", "grant"), ("party", maybe "" (BSC.pack . show . partyId) notificationParty)] "authorization" >> " to " >> party >> "."
  where
  target = partyRow (accountParty notificationTarget)
  person p = on (==) partyId p target ?!> htmlPartyViewLink p ([] :: Query)
  agent = fromMaybe "You" $ person notificationAgent
  partyp = person =<< notificationParty
  party = fromMaybe "you" partyp
  partys = maybe "your" (>> "'s") partyp
  link u a q h = H.a H.! actionLink u a (map (second Just) q :: Query) $ h
  partyEdit = link viewPartyEdit . TargetParty . partyId
