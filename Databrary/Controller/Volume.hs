{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Volume
  ( getVolume
  , viewVolume
  , viewVolumeEdit
  , viewVolumeCreate
  , postVolume
  , createVolume
  , viewVolumeLinks
  , postVolumeLinks
  , queryVolumes
  , thumbVolume
  , volumeDownloadName
  , volumeJSONQuery
  ) where

import Control.Applicative (Applicative, (<*>), (<|>), optional)
import Control.Arrow ((&&&))
import Control.Monad (mfilter, guard, void, when, liftM2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT(..), evalStateT, get, put)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Lazy as HML
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (Monoid(..), (<>), mempty)
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has (view, peeks, peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Enum
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Party
import Databrary.Model.Citation
import Databrary.Model.Citation.CrossRef
import Databrary.Model.Funding
import Databrary.Model.Container
import Databrary.Model.Record
import Databrary.Model.RecordSlot
import Databrary.Model.Slot
import Databrary.Model.Asset
import Databrary.Model.Excerpt
import Databrary.Model.Tag
import Databrary.Model.Comment
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.Web
import {-# SOURCE #-} Databrary.Controller.AssetSegment
import Databrary.View.Volume

getVolume :: Permission -> Id Volume -> AuthActionM Volume
getVolume p i =
  checkPermission p =<< maybeAction =<< lookupVolume i

data VolumeCache = VolumeCache
  { volumeCacheRecords :: Maybe (HML.HashMap (Id Record) Record)
  , volumeCacheTopContainer :: Maybe Container
  }

instance Monoid VolumeCache where
  mempty = VolumeCache mempty Nothing
  mappend (VolumeCache r1 t1) (VolumeCache r2 t2) = VolumeCache (r1 <> r2) (t1 <|> t2)

cacheVolumeRecords :: MonadDB m => Volume -> StateT VolumeCache m ([Record], HML.HashMap (Id Record) Record)
cacheVolumeRecords vol = do
  vc <- get
  maybe (do
    l <- lookupVolumeRecords vol
    let m = HML.fromList [ (recordId r, r) | r <- l ]
    put vc{ volumeCacheRecords = Just m }
    return (l, m))
    (return . (HML.elems &&& id))
    $ volumeCacheRecords vc

cacheVolumeTopContainer :: MonadDB m => Volume -> StateT VolumeCache m Container
cacheVolumeTopContainer vol = do
  vc <- get
  fromMaybeM (do
    t <- lookupVolumeTopContainer vol
    put vc{ volumeCacheTopContainer = Just t }
    return t)
    $ volumeCacheTopContainer vc

volumeJSONField :: (MonadDB m, MonadHasIdentity c m) => Volume -> BS.ByteString -> Maybe BS.ByteString -> StateT VolumeCache m (Maybe JSON.Value)
volumeJSONField vol "access" ma = do
  Just . JSON.toJSON . map (\va -> 
    volumeAccessJSON va JSON..+ ("party" JSON..= partyJSON (volumeAccessParty va)))
    <$> lookupVolumeAccess vol (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
volumeJSONField vol "citation" _ =
  Just . maybe JSON.Null JSON.toJSON <$> lookupVolumeCitation vol
volumeJSONField vol "links" _ =
  Just . JSON.toJSON <$> lookupVolumeLinks vol
volumeJSONField vol "funding" _ =
  Just . JSON.toJSON . map fundingJSON <$> lookupVolumeFunding vol
volumeJSONField vol "containers" (Just "records") = do
  (_, rm) <- cacheVolumeRecords vol
  let rjs c (s, r) = recordSlotJSON $ RecordSlot (HML.lookupDefault (Record r vol Nothing Nothing []) r rm) (Slot c s)
  Just . JSON.toJSON . map (\(c, rl) -> containerJSON c JSON..+ "records" JSON..= map (rjs c) rl) <$> lookupVolumeContainersRecordIds vol
volumeJSONField vol "containers" _ =
  Just . JSON.toJSON . map containerJSON <$> lookupVolumeContainers vol
volumeJSONField vol "top" _ =
  Just . JSON.toJSON . containerJSON <$> cacheVolumeTopContainer vol
volumeJSONField vol "records" _ = do
  (l, _) <- cacheVolumeRecords vol
  return $ Just $ JSON.toJSON $ map recordJSON l
volumeJSONField o "excerpts" _ =
  Just . JSON.toJSON . map excerptJSON <$> lookupVolumeExcerpts o
volumeJSONField o "tags" n = do
  t <- cacheVolumeTopContainer o
  tc <- lookupSlotTagCoverage (containerSlot t) (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.toJSON $ map tagCoverageJSON tc
volumeJSONField o "comments" n = do
  t <- cacheVolumeTopContainer o
  tc <- lookupSlotComments (containerSlot t) (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.toJSON $ map commentJSON tc
volumeJSONField _ _ _ = return Nothing

volumeJSONQuery :: (MonadDB m, MonadHasIdentity c m) => Volume -> JSON.Query -> m JSON.Object
volumeJSONQuery vol q = evalStateT (JSON.jsonQuery (volumeJSON vol) (volumeJSONField vol) q) mempty

volumeDownloadName :: (MonadDB m, MonadHasIdentity c m) => Volume -> m [T.Text]
volumeDownloadName v = do
  owns <- lookupVolumeAccess v PermissionADMIN
  return $ (T.pack $ "databrary" ++ show (volumeId v))
    : map (partySortName . volumeAccessParty) owns
    ++ [fromMaybe (volumeName v) (getVolumeAlias v)]

viewVolume :: AppRoute (API, Id Volume)
viewVolume = action GET (pathAPI </> pathId) $ \(api, vi) -> withAuth $ do
  when (api == HTML) angular
  v <- getVolume PermissionPUBLIC vi
  case api of
    JSON -> okResponse [] =<< volumeJSONQuery v =<< peeks Wai.queryString
    HTML -> okResponse [] $ volumeName v -- TODO

volumeForm :: (Functor m, Monad m) => Volume -> DeformT m Volume
volumeForm v = do
  name <- "name" .:> deform
  alias <- "alias" .:> deformNonEmpty deform
  body <- "body" .:> deformNonEmpty deform
  return v
    { volumeName = name
    , volumeAlias = alias
    , volumeBody = body
    }

volumeCitationForm :: Volume -> DeformActionM AuthRequest (Volume, Maybe Citation)
volumeCitationForm v = do
  csrfForm
  vol <- volumeForm v
  cite <- "citation" .:> Citation
    <$> ("head" .:> deform)
    <*> ("url" .:> deformNonEmpty deform)
    <*> ("year" .:> deformNonEmpty deform)
    <$- Nothing
  look <- flatMapM (lift . lookupCitation) $
    guard (T.null (volumeName vol) || T.null (citationHead cite) || isNothing (citationYear cite)) >> citationURL cite
  let fill = maybe cite (cite <>) look
      empty = isNothing (citationURL fill) && isNothing (citationYear fill)
      name 
        | Just title <- citationTitle fill
        , T.null (volumeName vol) = title
        | otherwise = volumeName vol
  _ <- "name" .:> deformRequired name
  when (not empty) $ void $
    "citation" .:> "name" .:> deformRequired (citationHead fill)
  return (vol{ volumeName = name }, empty ?!> fill)

viewVolumeEdit :: AppRoute (Id Volume)
viewVolumeEdit = action GET (pathHTML >/> pathId </< "edit") $ \vi -> withAuth $ do
  angular
  v <- getVolume PermissionEDIT vi
  blankForm . htmlVolumeForm (Just v) =<< lookupVolumeCitation v

viewVolumeCreate :: AppRoute ()
viewVolumeCreate = action GET (pathHTML </< "volume" </< "create") $ \() -> withAuth $ do
  angular
  blankForm $ htmlVolumeForm Nothing Nothing

postVolume :: AppRoute (API, Id Volume)
postVolume = action POST (pathAPI </> pathId) $ \arg@(api, vi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  cite <- lookupVolumeCitation v
  (v', cite') <- runForm (api == HTML ?> htmlVolumeForm (Just v) cite) $ volumeCitationForm v
  changeVolume v'
  _ <- changeVolumeCitation v' cite'
  case api of
    JSON -> okResponse [] $ volumeJSON v'
    HTML -> redirectRouteResponse [] viewVolume arg []

createVolume :: AppRoute API
createVolume = action POST (pathAPI </< "volume") $ \api -> withAuth $ do
  u <- peek
  (bv, cite, owner) <- runForm (api == HTML ?> htmlVolumeForm Nothing Nothing) $ do
    csrfForm
    (bv, cite) <- volumeCitationForm blankVolume
    own <- "owner" .:> do
      oi <- deformOptional deform
      own <- maybe (return $ Just $ selfAuthorize u) (lift . lookupAuthorizeParent u) oi
      deformMaybe' "You are not authorized to create volumes for that owner." $
        authorizeParent . authorization <$> mfilter ((PermissionADMIN <=) . accessMember) own
    auth <- lift $ lookupAuthorization own rootParty
    deformGuard "Insufficient site authorization to create volume." $
      PermissionEDIT <= accessSite auth
    return (bv, cite, own)
  v <- addVolume bv
  _ <- changeVolumeCitation v cite
  _ <- changeVolumeAccess $ VolumeAccess PermissionADMIN PermissionEDIT owner v
  case api of
    JSON -> okResponse [] $ volumeJSON v
    HTML -> redirectRouteResponse [] viewVolume (api, volumeId v) []

viewVolumeLinks :: AppRoute (Id Volume)
viewVolumeLinks = action GET (pathHTML >/> pathId </< "link") $ \vi -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  blankForm . htmlVolumeLinksForm v =<< lookupVolumeLinks v

postVolumeLinks :: AppRoute (API, Id Volume)
postVolumeLinks = action POST (pathAPI </> pathId </< "link") $ \arg@(api, vi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  links <- lookupVolumeLinks v
  links' <- runForm (api == HTML ?> htmlVolumeLinksForm v links) $ do
    csrfForm
    withSubDeforms $ Citation
      <$> ("head" .:> deform)
      <*> ("url" .:> (Just <$> deform))
      <$- Nothing
      <$- Nothing
  changeVolumeLinks v links'
  case api of
    JSON -> okResponse [] $ volumeJSON v JSON..+ ("links" JSON..= links')
    HTML -> redirectRouteResponse [] viewVolume arg []

volumeSearchForm :: (Applicative m, Monad m) => DeformT m VolumeFilter
volumeSearchForm = VolumeFilter
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("party" .:> optional deform)

queryVolumes :: AppRoute API
queryVolumes = action GET (pathAPI </< "volume") $ \api -> withAuth $ do
  when (api == HTML) angular
  (vf, (limit, offset)) <- runForm (api == HTML ?> htmlVolumeSearchForm mempty) $
    liftM2 (,) volumeSearchForm paginationForm
  p <- findVolumes vf limit offset
  case api of
    JSON -> okResponse [] $ JSON.toJSON $ map volumeJSON p
    HTML -> blankForm $ htmlVolumeSearchForm vf

thumbVolume :: AppRoute (Id Volume)
thumbVolume = action GET (pathId </< "thumb") $ \vi -> withAuth $ do
  v <- getVolume PermissionPUBLIC vi
  e <- lookupVolumeThumb v
  q <- peeks Wai.queryString
  maybe
    (redirectRouteResponse [] webFile (Just $ staticPath ["images", "draft.png"]) q)
    (\as -> redirectRouteResponse [] downloadAssetSegment (slotId $ view as, assetId $ view as) q)
    e
