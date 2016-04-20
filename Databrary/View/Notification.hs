{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Notification
  ( mailNotification
  , htmlNotification
  ) where

import Control.Arrow (second)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Text.Blaze.Html5 as H

import Databrary.Ops
import Databrary.Model.Notification
import Databrary.Model.Party
import Databrary.View.Party (htmlPartyViewLink)
import Databrary.Action.Route
import Databrary.Controller.Paths
import Databrary.Controller.Party
import Databrary.View.Html

mailNotification :: Notification -> TL.Text
mailNotification Notification{..} = case notificationNotice of
  NoticeAccountChange ->
    maybe "Your" (<> "'s") partyp <> " account information has been changed. To review or update your information, go to: "
    <> partyEdit (fromMaybe target notificationParty) [("page", "account")]
    <> "\nIf you did not make this change, please contact us immediately."
  where
  target = partyRow (accountParty notificationTarget)
  person p = on (/=) partyId p target ?> TL.fromStrict (partyName p)
  partyp = person =<< notificationParty
  link u a q = TLE.decodeLatin1 $ BSB.toLazyByteString $ actionURL Nothing u a (map (second Just) q :: Query)
  partyEdit = link viewPartyEdit . TargetParty . partyId

htmlNotification :: Notification -> H.Html
htmlNotification Notification{..} = case notificationNotice of
  NoticeAccountChange ->
    agent >> " changed " >> partys >> " "
    >> partyEdit (fromMaybe target notificationParty) [("page", "account")] "account information" >> "."
  NoticeAuthorizeRequest ->
    agent >> " requested "
    >> partyEdit target [("page", "apply"), partyq] "authorization" >> " from " >> party >> "."
  NoticeAuthorizeGranted ->
    agent >> " " >> granted >> " you "
    >> partyEdit target [("page", "apply"), partyq] "authorization" >> " under " >> party >> "."
  NoticeAuthorizeChildRequest ->
    agent >> " requested "
    >> partyEdit target [("page", "grant"), partyq] "authorization" >> " for " >> party >> "."
  NoticeAuthorizeChildGranted ->
    agent >> " " >> granted >> " "
    >> partyEdit target [("page", "grant"), partyq] "authorization" >> " to " >> party >> "."
  where
  target = partyRow (accountParty notificationTarget)
  person p = on (/=) partyId p target ?> htmlPartyViewLink p ([] :: Query)
  agent = fromMaybe "You" $ person notificationAgent
  partyp = any (on (/=) partyId notificationAgent) notificationParty ?$> person =<< notificationParty
  party = maybe "you" (fromMaybe "themselves") partyp
  partys = maybe "your" (maybe "their own" (>> "'s")) partyp
  partyq = ("party", maybe "" (BSC.pack . show . partyId) notificationParty)
  link u a q h = H.a H.! actionLink u a (map (second Just) q :: Query) $ h
  partyEdit = link viewPartyEdit . TargetParty . partyId
  granted = maybe "revoked" (const "granted") notificationPermission
