{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Party.Types
  , nobodyParty
  , rootParty
  , partyName
  , partyEmail
  , lookupParty
  , lookupAuthParty
  , lookupSiteAuthByEmail
  , changeParty
  , changeAccount
  , addParty
  , addAccount
  , auditAccountLogin
  , recentAccountLogins
  , partyJSON
  , PartyFilter(..)
  , findParties
  , lookupAvatar
  , changeAvatar
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isNothing, fromMaybe)
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep, pgLiteralString, pgSafeLiteral)

import Databrary.Ops
import Databrary.Has (Has(..), peek)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Request
import Databrary.Model.Id
import Databrary.Model.SQL
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Audit.SQL
import Databrary.Model.Identity.Types
import Databrary.Model.Volume
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Party.Boot

useTPG

nobodyParty, rootParty :: Party
nobodyParty = $(loadParty (Id (-1)) PermissionREAD)
rootParty = $(loadParty (Id 0) PermissionSHARED)

partyName :: Party -> T.Text
partyName Party{ partyPreName = Just p, partySortName = n } = p <> T.cons ' ' n
partyName Party{ partySortName = n } = n

emailPermission :: Permission
emailPermission = PermissionSHARED

showEmail :: Identity -> Bool
showEmail i = accessSite i >= emailPermission

partyEmail :: Party -> Maybe T.Text
partyEmail p =
  guard (partyPermission p >= emailPermission) >> accountEmail <$> partyAccount p

partyJSON :: Party -> JSON.Object
partyJSON p@Party{..} = JSON.record partyId $ catMaybes
  [ Just $ "sortname" JSON..= partySortName
  , ("prename" JSON..=) <$> partyPreName
  , Just $ "name" JSON..= partyName p
  , ("orcid" JSON..=) . show <$> partyORCID
  , ("affiliation" JSON..=) <$> partyAffiliation
  , ("url" JSON..=) <$> partyURL
  , "institution" JSON..= True <? isNothing partyAccount
  , ("email" JSON..=) <$> partyEmail p
  , "permission" JSON..= partyPermission <? (partyPermission > PermissionREAD)
  ]

changeParty :: MonadAudit c m => Party -> m ()
changeParty p = do
  ident <- getAuditIdentity
  dbExecute1' $(updateParty 'ident 'p)

changeAccount :: MonadAudit c m => SiteAuth -> m ()
changeAccount a = do
  ident <- getAuditIdentity
  dbExecute1' $(updateAccount 'ident 'a)

addParty :: MonadAudit c m => Party -> m Party
addParty bp = do
  ident <- getAuditIdentity
  dbQuery1' $ fmap (\p -> p PermissionREAD Nothing) $(insertParty 'ident 'bp)

addAccount :: MonadAudit c m => Account -> m Account
addAccount ba@Account{ accountParty = bp } = do
  ident <- getAuditIdentity
  p <- dbQuery1' $ fmap (\p -> p PermissionREAD Nothing) $(insertParty 'ident 'bp)
  let pa = p{ partyAccount = Just a }
      a = ba{ accountParty = pa }
  dbExecute1' $(insertAccount 'ident 'a)
  return a

lookupFixedParty :: Id Party -> Identity -> Maybe Party
lookupFixedParty (Id (-1)) _ = Just nobodyParty
lookupFixedParty (Id 0) i = Just rootParty{ partyAccess = accessMember i > PermissionNONE ?> view i }
lookupFixedParty i a = view a <? (i == view a)

lookupParty :: (MonadDB m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupParty i = do
  ident <- peek
  lookupFixedParty i ident `orElseM`
    dbQuery1 $(selectQuery (selectParty 'ident) "$WHERE party.id = ${i}")

lookupAuthParty :: (MonadDB m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupAuthParty i = do
  ident <- peek
  lookupFixedParty i ident `orElseM`
    dbQuery1 $(selectQuery (selectAuthParty 'ident) "$WHERE party.id = ${i}")

lookupSiteAuthByEmail :: MonadDB m => T.Text -> m (Maybe SiteAuth)
lookupSiteAuthByEmail e =
  dbQuery1 $(selectQuery selectSiteAuth "WHERE account.email = ${e}")

auditAccountLogin :: (MonadHasRequest c m, MonadDB m) => Bool -> Party -> T.Text -> m ()
auditAccountLogin success who email = do
  ip <- getRemoteIp
  dbExecute1' [pgSQL|INSERT INTO audit.account (audit_action, audit_user, audit_ip, id, email) VALUES
    (${if success then AuditActionOpen else AuditActionAttempt}, -1, ${ip}, ${partyId who}, ${email})|]

recentAccountLogins :: MonadDB m => Party -> m Int64
recentAccountLogins who = fromMaybe 0 <$>
  dbQuery1 [pgSQL|!SELECT count(*) FROM audit.account WHERE audit_action = 'attempt' AND id = ${partyId who} AND audit_time > CURRENT_TIMESTAMP - interval '1 hour'|]

data PartyFilter = PartyFilter
  { partyFilterQuery :: Maybe String
  , partyFilterAuthorization :: Maybe Permission
  , partyFilterInstitution :: Maybe Bool
  , partyFilterAuthorize :: Maybe (Id Party)
  , partyFilterVolume :: Maybe Volume
  }

instance Monoid PartyFilter where
  mempty = PartyFilter Nothing Nothing Nothing Nothing Nothing
  mappend (PartyFilter q1 a1 i1 p1 v1) (PartyFilter q2 a2 i2 p2 v2) =
    PartyFilter (q1 <> q2) (a1 <|> a2) (i1 <|> i2) (p1 <|> p2) (v1 <|> v2)

partyFilter :: PartyFilter -> Identity -> BS.ByteString
partyFilter PartyFilter{..} ident = BS.concat
  [ withq partyFilterAuthorization (const " JOIN authorize_view ON party.id = child AND parent = 0")
  , " WHERE id > 0"
  , withq partyFilterQuery (\n -> " AND " <> queryVal <> " ILIKE " <> pgLiteralRep (wordPat n))
  , withq partyFilterAuthorization (\a -> " AND site = " <> pgSafeLiteral a)
  , withq partyFilterInstitution (\i -> if i then " AND account.id IS NULL" else " AND account.password IS NOT NULL")
  , withq partyFilterAuthorize (\a -> let i = pgSafeLiteral a in " AND party.id <> " <> i <> " AND id NOT IN (SELECT child FROM authorize WHERE parent = " <> i <> " UNION SELECT parent FROM authorize WHERE child = " <> i <> ")")
  , withq partyFilterVolume (\v -> " AND id NOT IN (SELECT party FROM volume_access WHERE volume = " <> pgSafeLiteral (volumeId v) <> ")")
  , " ORDER BY name, prename"
  ]
  where
  withq v f = maybe "" f v
  wordPat = intercalate "%" . ("":) . (++[""]) . words
  queryVal
    | showEmail ident = "(COALESCE(prename || ' ', '') || name || COALESCE(' ' || email, ''))"
    | otherwise = "(COALESCE(prename || ' ', '') || name)"

findParties :: (MonadHasIdentity c m, MonadDB m) => PartyFilter -> Int32 -> Int32 -> m [Party]
findParties pf limit offset = do
  ident <- peek
  dbQuery $ unsafeModifyQuery $(selectQuery (selectParty 'ident) "")
    (<> partyFilter pf ident <> " LIMIT " <> pgLiteralRep limit <> " OFFSET " <> pgLiteralRep offset)

lookupAvatar :: MonadDB m => Id Party -> m (Maybe Asset)
lookupAvatar p =
  dbQuery1 $ ($ coreVolume) <$> $(selectQuery selectVolumeAsset $ "$JOIN avatar ON asset.id = avatar.asset WHERE avatar.party = ${p} AND asset.volume = " ++ pgLiteralString (volumeId coreVolume))

changeAvatar :: MonadAudit c m => Party -> Maybe Asset -> m Bool
changeAvatar p Nothing = do
  ident <- getAuditIdentity
  dbExecute1 $(auditDelete 'ident "avatar" "party = ${partyId p}" Nothing)
changeAvatar p (Just a) = do
  ident <- getAuditIdentity
  (0 <) . fst <$> updateOrInsert
    $(auditUpdate 'ident "avatar" [("asset", "${assetId a}")] "party = ${partyId p}" Nothing)
    $(auditInsert 'ident "avatar" [("asset", "${assetId a}"), ("party", "${partyId p}")] Nothing)
