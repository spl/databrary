{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Volume.SQL
  ( volumeRow
  , selectVolume
  , updateVolume
  , insertVolume
  ) where

import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Model.Time
import Databrary.Model.SQL.Select
import Databrary.Model.Permission.Types
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.Types

defaulting :: a -> (a -> b) -> Maybe a -> b
defaulting d f = f . fromMaybe d

setCreation :: (Timestamp -> a) -> Maybe Timestamp -> a
setCreation = defaulting $ volumeCreation blankVolume

setPermission :: (Permission -> a) -> Maybe Permission -> a
setPermission = defaulting PermissionNONE

volumeRow :: Selector -- ^ @'Permission' -> 'Volume'@
volumeRow = addSelects 'setCreation
  (selectColumns 'Volume "volume" ["id", "name", "alias", "body", "doi"])
  ["volume_creation(volume.id)"]

selectVolume :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Volume'@
selectVolume i = selectJoin 'setPermission
  [ volumeRow
  , joinOn "volume_permission.permission >= 'PUBLIC'::permission"
    $ selector ("LATERAL (VALUES (CASE WHEN ${identitySuperuser " ++ is ++ "} THEN enum_last(NULL::permission) ELSE volume_access_check(volume.id, ${view " ++ is ++ " :: Id Party}) END)) AS volume_permission (permission)")
    "volume_permission.permission"
  ]
  where is = nameRef i

volumeKeys :: String -- ^ @'Volume'@
  -> [(String, String)]
volumeKeys v =
  [ ("id", "${volumeId " ++ v ++ "}") ]

volumeSets :: String -- ^ @'Account'@
  -> [(String, String)]
volumeSets v =
  [ ("name",  "${volumeName "  ++ v ++ "}")
  , ("alias", "${volumeAlias " ++ v ++ "}")
  , ("body",  "${volumeBody "  ++ v ++ "}")
  ]

updateVolume :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Volume'@
  -> TH.ExpQ -- ()
updateVolume ident v = auditUpdate ident "volume"
  (volumeSets vs)
  (whereEq $ volumeKeys vs)
  Nothing
  where vs = nameRef v

insertVolume :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Volume'@
  -> TH.ExpQ -- ^ @'Permission' -> 'Volume'@
insertVolume ident v = auditInsert ident "!volume"
  (volumeSets vs)
  (Just $ selectOutput volumeRow)
  where vs = nameRef v

