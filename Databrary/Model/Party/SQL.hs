{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Party.SQL
  ( partyRow
  , selectParty
  , selectAuthParty
  , selectAccount
  , selectSiteAuth
  , updateParty
  , updateAccount
  , insertParty
  , insertAccount
  , deleteParty
  , deleteAccount
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import qualified Language.Haskell.TH as TH

import Databrary.Ops
import Databrary.Has (Has, view)
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Permission.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types

partyRow :: Selector -- ^ @Maybe 'Account' -> 'Permission' -> Maybe 'Access' -> 'Party'@
partyRow = selectColumns 'Party "party" ["id", "name", "prename", "orcid", "affiliation", "url"]

accountRow :: Selector -- ^ @'Party' -> 'Account'@
accountRow = selectColumns 'Account "account" ["email"]

makeParty :: (Maybe Account -> Permission -> Maybe Access -> Party) -> Maybe (Party -> Account) -> Permission -> Maybe Access -> Party
makeParty pc ac perm a = p where
  p = pc (fmap ($ p) ac) perm a

selectPermissionParty :: Selector -- ^ @'Permission' -> Maybe 'Access' -> 'Party'@
selectPermissionParty = selectJoin 'makeParty 
  [ partyRow
  , maybeJoinUsing ["id"] accountRow
  ]

permissionParty :: Has (Id Party) a => (Permission -> Maybe Access -> a) -> Maybe Access -> Identity -> a
permissionParty pf a ident = p where
  p = pf
    (max PermissionPUBLIC $ min PermissionREAD $ accessSite ident)
    (((identitySuperuser ident || foldIdentity False (((view p :: Id Party) ==) . view) ident) ?> maxBound) <|> a)

selectParty :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Party'@
selectParty ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` (TH.ConE 'Nothing)) . (TH.VarE 'permissionParty `TH.AppE`)) $
  selectPermissionParty

selectAuthParty :: TH.Name -- ^ 'Identity`
  -> Selector -- ^ @'Party'@
selectAuthParty ident = selectMap (`TH.AppE` TH.VarE ident) $ selectJoin 'permissionParty
  [ selectPermissionParty
  , maybeJoinOn ("party.id = authorize_valid.parent AND authorize_valid.child = ${view " ++ nameRef ident ++ " :: Id Party}")
    $ accessRow "authorize_valid" -- optimization, should be authorize_view if we used site
  ]

makeAccount :: (Maybe Account -> Permission -> Maybe Access -> Party) -> (Party -> Account) -> Permission -> Maybe Access -> Account
makeAccount pc ac perm ma = a where
  a = ac $ pc (Just a) perm ma

selectPermissionAccount :: Selector -- ^ @'Permission' -> Maybe 'Access' -> 'Account'@
selectPermissionAccount = selectJoin 'makeAccount 
  [ partyRow
  , joinUsing ["id"] accountRow
  ]

selectAccount :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Account'@
selectAccount ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` (TH.ConE 'Nothing)) . (TH.VarE 'permissionParty `TH.AppE`)) $
  selectPermissionAccount

makeSiteAuth :: (Permission -> Maybe Access -> Account) -> Maybe BS.ByteString -> Maybe Access -> SiteAuth
makeSiteAuth p w a = SiteAuth (p maxBound $ Just maxBound) w (Fold.fold a)

selectSiteAuth :: Selector -- @'SiteAuth'@
selectSiteAuth = selectJoin 'makeSiteAuth
  [ selectPermissionAccount
  , columnSelector $ SelectColumn "account" "password"
  , maybeJoinOn "party.id = authorize_view.child AND authorize_view.parent = 0"
    $ accessRow "authorize_view"
  ]

partyKeys :: String -- ^ @'Party'@
  -> [(String, String)]
partyKeys p =
  [ ("id", "${partyId " ++ p ++ "}") ]

accountKeys :: String -- ^ @'Account'@
  -> [(String, String)]
accountKeys a = partyKeys $ "(accountParty " ++ a ++ ")"

partySets :: String -- ^ @'Party'@
  -> [(String, String)]
partySets p =
  [ ("name",        "${partySortName "    ++ p ++ "}")
  , ("prename",     "${partyPreName "     ++ p ++ "}")
  , ("affiliation", "${partyAffiliation " ++ p ++ "}")
  , ("url",         "${partyURL "         ++ p ++ "}")
  ]

accountSets :: String -- ^ @'Account'@
  -> [(String, String)]
accountSets a =
  [ ("email", "${accountEmail " ++ a ++ "}")
  ]

updateParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ()
updateParty ident p = auditUpdate ident "party"
  (partySets ps)
  (whereEq $ partyKeys ps)
  Nothing
  where ps = nameRef p

updateAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Account'@
  -> TH.ExpQ -- ()
updateAccount ident a = auditUpdate ident "account"
  (accountSets as ++ [("password", "${accountPasswd " ++ us ++ "}")])
  (whereEq $ accountKeys as)
  Nothing
  where
  as = "(siteAccount " ++ us ++ ")"
  us = nameRef a

insertParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @'Permission' -> 'Party'@
insertParty ident p = auditInsert ident "party"
  (partySets ps)
  (Just $ OutputMap False (`TH.AppE` TH.ConE 'Nothing) $ selectOutput partyRow)
  where ps = nameRef p

insertAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Account'@
  -> TH.ExpQ
insertAccount ident a = auditInsert ident "account"
  (accountKeys as ++ accountSets as)
  Nothing
  where as = nameRef a

deleteParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @()@
deleteParty ident p = auditDelete ident "party"
  (whereEq $ partyKeys (nameRef p))
  Nothing

deleteAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @()@
deleteAccount ident p = auditDelete ident "account"
  (whereEq $ partyKeys (nameRef p))
  Nothing
