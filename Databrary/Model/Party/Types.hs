{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Party.Types 
  ( Party(..)
  , MonadHasParty
  , Account(..)
  , MonadHasAccount
  , SiteAuth(..)
  , MonadHasSiteAuth
  , nobodySiteAuth
  , blankParty
  ) where

import qualified Data.ByteString as BS
import Data.Monoid (mempty)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.Has (makeHasRec, Has(..))
import Databrary.Model.URL (URI)
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.ORCID

type instance IdType Party = Int32

data Party = Party
  { partyId :: Id Party
  , partySortName :: T.Text
  , partyPreName :: Maybe T.Text
  , partyORCID :: Maybe ORCID
  , partyAffiliation :: Maybe T.Text
  , partyURL :: Maybe URI
  , partyAccount :: Maybe Account
  , partyPermission :: Permission -- permission current user has over this party
  , partyAccess :: Maybe Access -- direct authorization this party has granted to current user
  }

data Account = Account
  { accountEmail :: BS.ByteString
  , accountParty :: Party
  }

makeHasRec ''Party ['partyId]
makeHasRec ''Account ['accountParty]

instance Has Access Party where
  view Party{ partyAccess = Just a } = a
  view _ = mempty
instance Has Permission Party where
  view = partyPermission

instance Kinded Party where
  kindOf _ = "party"

-- Access to the site by a (current) account
data SiteAuth = SiteAuth
  { siteAccount :: Account -- maybe should be Party (for nobody)
  , accountPasswd :: Maybe BS.ByteString
  , siteAccess :: Access
  }

makeHasRec ''SiteAuth ['siteAccount, 'siteAccess]

deriveLiftMany [''Party, ''Account]

-- this is unfortunate, mainly to avoid untangling Party.SQL
nobodySiteAuth :: SiteAuth
nobodySiteAuth = SiteAuth
  { siteAccount = Account
    { accountEmail = "nobody@databrary.org"
    , accountParty = Party
      { partyId = Id (-1)
      , partySortName = "Nobody"
      , partyPreName = Nothing
      , partyORCID = Nothing
      , partyAffiliation = Nothing
      , partyURL = Nothing
      , partyAccount = Nothing
      , partyPermission = PermissionREAD
      , partyAccess = Just minBound
      }
    }
  , accountPasswd = Nothing
  , siteAccess = mempty
  }

blankParty :: Party
blankParty = Party
  { partyId = error "blankParty"
  , partySortName = ""
  , partyPreName = Nothing
  , partyORCID = Nothing
  , partyAffiliation = Nothing
  , partyURL = Nothing
  , partyAccount = Nothing
  , partyPermission = PermissionNONE
  , partyAccess = Nothing
  }
