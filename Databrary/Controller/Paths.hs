{-# LANGUAGE OverloadedStrings, TypeOperators, QuasiQuotes #-}
module Databrary.Controller.Paths
  ( pathId
  , PartyTarget(..)
  , pathPartyTarget
  , AuthorizeTarget(..)
  , pathAuthorizeTarget
  , VolumeAccessTarget(..)
  , pathVolumeAccessTarget
  , pathSlotId
  , TagId(..)
  , pathTagId
  ) where

import qualified Databrary.Iso as I
import Databrary.Iso.TH
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
idIso = [iso|a <-> Id a|]

pathIdWith :: forall a . (Kinded a) => PathParser (IdType a) -> PathParser (Id a)
pathIdWith p = PathFixed (kindOf (undefined :: a)) >/> idIso I.<$> p

pathId :: forall a . (PathParameter (IdType a), Kinded a) => PathParser (Id a)
pathId = pathIdWith PathParameter

data PartyTarget
  = TargetProfile
  | TargetParty (Id Party)

pathPartyTarget :: PathParser PartyTarget
pathPartyTarget = [iso|
    Left () <-> TargetProfile
    Right i <-> TargetParty i
  |] I.<$> ("profile" |/| pathId)

data AuthorizeTarget = AuthorizeTarget
  { authorizeApply :: Bool
  , authorizeTarget :: Id Party
  }

pathAuthorizeTarget :: PathParser AuthorizeTarget
pathAuthorizeTarget = [iso|(a, t) <-> AuthorizeTarget a t|] I.<$>
  (I.isRight I.<$> ("authorize" |/| "apply")
   </> idIso I.<$> PathParameter)

newtype VolumeAccessTarget = VolumeAccessTarget
  { volumeAccessTarget :: Id Party
  }

pathVolumeAccessTarget :: PathParser VolumeAccessTarget
pathVolumeAccessTarget = "access" >/> [iso|i <-> VolumeAccessTarget (Id i)|] I.<$> PathParameter

slotIdIso :: (Id Container, Segment) I.<-> SlotId
slotIdIso = [iso|(c, s) <-> SlotId c s|]

pathSlot :: PathParser SlotId
pathSlot = slotIdIso I.<$> (idIso I.<$> PathParameter </> fullSegment =/= PathParameter)

pathSlotId :: PathParser (Id Slot)
pathSlotId = pathIdWith pathSlot

data TagId = TagId
  { tagIdKeyword :: Bool
  , tagIdName :: TagName
  }

pathTagId :: PathParser TagId
pathTagId = [iso|(b, t) <-> TagId b t|] I.<$>
  (I.isRight I.<$> ("tag" |/| "keyword") </> PathParameter)

