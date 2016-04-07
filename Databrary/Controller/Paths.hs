{-# LANGUAGE OverloadedStrings, TypeOperators, QuasiQuotes #-}
module Databrary.Controller.Paths
  ( pathId
  , PartyTarget(..)
  , pathPartyTarget
  , AuthorizeTarget(..)
  , pathAuthorizeTarget
  , VolumeAccessTarget(..)
  , pathVolumeAccessTarget
  , pathSegment
  , pathSlotId
  , TagId(..)
  , pathTagId
  ) where

import qualified Data.Isomorphism as I

import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.HTTP.Path.Types
import Databrary.HTTP.Path.Parser

idIso :: IdType a I.<-> Id a
idIso = [I.isoCase|a <-> Id a|]

pathIdWith :: forall a . (Kinded a) => PathParser (IdType a) -> PathParser (Id a)
pathIdWith p = PathFixed (kindOf (undefined :: a)) >/> idIso >$< p

pathId :: forall a . (PathParameter (IdType a), Kinded a) => PathParser (Id a)
pathId = pathIdWith PathParameter

data PartyTarget
  = TargetProfile
  | TargetParty (Id Party)

pathPartyTarget :: PathParser PartyTarget
pathPartyTarget = [I.isoCase|
    Left () <-> TargetProfile
    Right i <-> TargetParty i
  |] >$< ("profile" |/| pathId)

data AuthorizeTarget = AuthorizeTarget
  { authorizeApply :: Bool
  , authorizeTarget :: Id Party
  }

pathAuthorizeTarget :: PathParser AuthorizeTarget
pathAuthorizeTarget = [I.isoCase|(a, t) <-> AuthorizeTarget a t|] >$<
  (I.isRight >$< ("authorize" |/| "apply")
   </> idIso >$< PathParameter)

newtype VolumeAccessTarget = VolumeAccessTarget
  { volumeAccessTarget :: Id Party
  }

pathVolumeAccessTarget :: PathParser VolumeAccessTarget
pathVolumeAccessTarget = "access" >/> [I.isoCase|i <-> VolumeAccessTarget (Id i)|] >$< PathParameter

slotIdIso :: (Id Container, Segment) I.<-> SlotId
slotIdIso = [I.isoCase|(c, s) <-> SlotId c s|]

pathSegment :: PathParser Segment
pathSegment = fullSegment =/= PathParameter

pathSlot :: PathParser SlotId
pathSlot = slotIdIso >$< (idIso >$< PathParameter </> pathSegment)

pathSlotId :: PathParser (Id Slot)
pathSlotId = pathIdWith pathSlot

data TagId = TagId
  { tagIdKeyword :: Bool
  , tagIdName :: TagName
  }

pathTagId :: PathParser TagId
pathTagId = [I.isoCase|(b, t) <-> TagId b t|] >$<
  (I.isRight >$< ("tag" |/| "keyword") </> PathParameter)

