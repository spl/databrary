{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Volume 
  ( module Databrary.Model.Volume.Types
  , coreVolume
  , lookupVolume
  , changeVolume
  , addVolume
  , auditVolumeDownload
  , VolumeFilter(..)
  , findVolumes
  , getVolumeAlias
  , volumeJSON
  , updateVolumeIndex
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Query (pgSQL, unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep)

import Databrary.Ops
import Databrary.Has (peek, view)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Paginate
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Volume.Boot

useTPG

coreVolume :: Volume
coreVolume = $(loadVolume (Id 0))

lookupVolume :: (MonadDB m, MonadHasIdentity c m) => Id Volume -> m (Maybe Volume)
lookupVolume vi = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectVolume 'ident) "$WHERE volume.id = ${vi}")

changeVolume :: MonadAudit c m => Volume -> m ()
changeVolume v = do
  ident <- getAuditIdentity
  dbExecute1' $(updateVolume 'ident 'v)

addVolume :: MonadAudit c m => Volume -> m Volume
addVolume bv = do
  ident <- getAuditIdentity
  dbQuery1' $ fmap (\v -> v [] PermissionADMIN) $(insertVolume 'ident 'bv)

getVolumeAlias :: Volume -> Maybe T.Text
getVolumeAlias v = guard (volumePermission v >= PermissionREAD) >> volumeAlias v

auditVolumeDownload :: MonadAudit c m => Bool -> Volume -> m ()
auditVolumeDownload success vol = do
  ai <- getAuditIdentity
  dbExecute1' [pgSQL|$INSERT INTO audit.volume (audit_action, audit_user, audit_ip, id) VALUES
    (${if success then AuditActionOpen else AuditActionAttempt}, ${auditWho ai}, ${auditIp ai}, ${volumeId vol})|]

volumeJSON :: Volume -> JSON.Object
volumeJSON v@Volume{..} = JSON.record volumeId $ catMaybes
  [ Just $ "name" JSON..= volumeName
  , ("alias" JSON..=) <$> getVolumeAlias v
  , Just $ "body" JSON..= volumeBody
  , ("doi" JSON..=) <$> volumeDOI
  , Just $ "creation" JSON..= volumeCreation
  , Just $ "owners" JSON..= JSON.object (map (\(i, n) -> T.pack (show i) JSON..= n) volumeOwners)
  , Just $ "permission" JSON..= volumePermission
  ]

data VolumeFilter = VolumeFilter
  { volumeFilterQuery :: Maybe String
  , volumeFilterParty :: Maybe (Id Party)
  , volumeFilterPaginate :: Paginate
  }

instance Monoid VolumeFilter where
  mempty = VolumeFilter Nothing Nothing def
  mappend (VolumeFilter q1 p1 p) (VolumeFilter q2 p2 _) =
    VolumeFilter (q1 <> q2) (p1 <|> p2) p

volumeFilter :: VolumeFilter -> BS.ByteString
volumeFilter VolumeFilter{..} = BS.concat
  [ withq volumeFilterParty (const " JOIN volume_access ON volume.id = volume_access.volume")
  , withq volumeFilterQuery (\n -> " JOIN volume_text_idx ON volume.id = volume_text_idx.volume, plainto_tsquery('english', " <> pgLiteralRep n <> ") query")
  , " WHERE volume.id > 0 "
  , withq volumeFilterParty (\p -> " AND volume_access.party = " <> pgLiteralRep p <> " AND volume_access.individual >= 'EDIT'")
  , withq volumeFilterQuery (const " AND ts @@ query")
  , " ORDER BY "
  , withq volumeFilterQuery (const "ts_rank(ts, query) DESC,")
  , withq volumeFilterParty (const "volume_access.individual DESC,")
  , "volume.id DESC "
  , paginateSQL volumeFilterPaginate
  ]
  where
  withq v f = maybe "" f v

findVolumes :: (MonadHasIdentity c m, MonadDB m) => VolumeFilter -> m [Volume]
findVolumes pf = do
  ident <- peek
  dbQuery $ unsafeModifyQuery $(selectQuery (selectVolume 'ident) "")
    (<> volumeFilter pf)

updateVolumeIndex :: MonadDB m => m ()
updateVolumeIndex =
  dbExecute_ "SELECT volume_text_refresh()"
