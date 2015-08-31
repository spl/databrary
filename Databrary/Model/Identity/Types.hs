{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Identity.Types
  ( Identity(..)
  , MonadHasIdentity
  , foldIdentity
  , identityVerf
  , identityAdmin
  , identitySuperuser
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as BS

import Databrary.Has (Has(..))
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Token.Types

data Identity
  = PreIdentified
  | NotIdentified
  | Identified Session
  | ReIdentified SiteAuth

instance Has SiteAuth Identity where
  view (Identified Session{ sessionAccountToken = AccountToken{ tokenAccount = t } }) = t
  view (ReIdentified a) = a
  view _ = nobodySiteAuth

instance Has Party Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has Account Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has (Id Party) Identity where
  view = view . (view :: Identity -> SiteAuth)
instance Has Access Identity where
  view = view . (view :: Identity -> SiteAuth)

type MonadHasIdentity c m = (Functor m, Applicative m, MonadReader c m, Has Identity c, Has SiteAuth c, Has Party c, Has (Id Party) c, Has Access c)

foldIdentity :: a -> (Session -> a) -> Identity -> a
foldIdentity _ i (Identified s) = i s
foldIdentity u _ _ = u

identityVerf :: Identity -> Maybe BS.ByteString
identityVerf = foldIdentity Nothing (Just . sessionVerf)

identitySuperuserFor :: (Access -> Permission) -> Identity -> Bool
identitySuperuserFor f (Identified t) = sessionSuperuser t && f (view t) == PermissionADMIN
identitySuperuserFor _ (ReIdentified _) = True
identitySuperuserFor _ _ = False

identityAdmin, identitySuperuser :: Identity -> Bool
identityAdmin = identitySuperuserFor accessMember
identitySuperuser = identitySuperuserFor accessPermission
