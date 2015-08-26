{-# LANGUAGE OverloadedStrings, TupleSections #-}
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
import Control.Arrow ((&&&), (***))
import Control.Monad (mfilter, guard, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT(..), evalStateT, get, put)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (Monoid(..), (<>), mempty)
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has
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
import Databrary.Model.VolumeMetric
import Databrary.Model.RecordSlot
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.Model.Excerpt
import Databrary.Model.Tag
import Databrary.Model.Comment
import Databrary.Store.Filename
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

getVolume :: Permission -> Id Volume -> ActionM Volume
getVolume p i =
  checkPermission p =<< maybeAction =<< lookupVolume i

data VolumeCache = VolumeCache
  { volumeCacheAccess :: Maybe [VolumeAccess]
  , volumeCacheTopContainer :: Maybe Container
  , volumeCacheRecords :: Maybe (HML.HashMap (Id Record) Record)
  }

instance Monoid VolumeCache where
  mempty = VolumeCache Nothing Nothing Nothing
  mappend (VolumeCache a1 t1 r1) (VolumeCache a2 t2 r2) = VolumeCache (a1 <|> a2) (t1 <|> t2) (r1 <> r2)

runVolumeCache :: Monad m => StateT VolumeCache m a -> m a
runVolumeCache f = evalStateT f mempty

cacheVolumeAccess :: (MonadDB m, MonadHasIdentity c m) => Volume -> Permission -> StateT VolumeCache m [VolumeAccess]
cacheVolumeAccess vol perm = do
  vc <- get
  takeWhile ((perm <=) . volumeAccessIndividual) <$>
    fromMaybeM (do
      a <- lookupVolumeAccess vol PermissionNONE
      put vc{ volumeCacheAccess = Just a }
      return a)
      (volumeCacheAccess vc)

cacheVolumeRecords :: MonadDB m => Volume -> StateT VolumeCache m ([Record], HML.HashMap (Id Record) Record)
cacheVolumeRecords vol = do
  vc <- get
  maybe (do
    l <- lookupVolumeRecords vol
    let m = HML.fromList [ (recordId r, r) | r <- l ]
    put vc{ volumeCacheRecords = Just m }
    return (l, m))
    (return . (HML.elems &&& id))
    (volumeCacheRecords vc)

cacheVolumeTopContainer :: MonadDB m => Volume -> StateT VolumeCache m Container
cacheVolumeTopContainer vol = do
  vc <- get
  fromMaybeM (do
    t <- lookupVolumeTopContainer vol
    put vc{ volumeCacheTopContainer = Just t }
    return t)
    (volumeCacheTopContainer vc)

leftJoin :: (a -> b -> Bool) -> [a] -> [b] -> [(a, [b])]
leftJoin _ [] [] = []
leftJoin _ [] _ = error "leftJoin: leftovers"
leftJoin p (a:al) b = uncurry (:) $ ((a, ) *** leftJoin p al) $ span (p a) b

volumeJSONField :: (MonadDB m, MonadHasIdentity c m) => Volume -> BS.ByteString -> Maybe BS.ByteString -> StateT VolumeCache m (Maybe JSON.Value)
volumeJSONField vol "access" ma = do
  Just . JSON.toJSON . map volumeAccessPartyJSON
    <$> cacheVolumeAccess vol (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
volumeJSONField vol "citation" _ =
  Just . JSON.toJSON <$> lookupVolumeCitation vol
volumeJSONField vol "links" _ =
  Just . JSON.toJSON <$> lookupVolumeLinks vol
volumeJSONField vol "funding" _ =
  Just . JSON.toJSON . map fundingJSON <$> lookupVolumeFunding vol
volumeJSONField vol "containers" o = do
  cl <- if records
    then lookupVolumeContainersRecordIds vol
    else nope <$> lookupVolumeContainers vol
  cl' <- if assets
    then leftJoin (\(c, _) (_, SlotId a _) -> containerId c == a) cl <$> lookupVolumeAssetSlotIds vol
    else return $ nope cl
  rm <- if records then snd <$> cacheVolumeRecords vol else return HM.empty
  let rjs c (s, r) = recordSlotJSON $ RecordSlot (HML.lookupDefault (blankRecord vol){ recordId = r } r rm) (Slot c s)
      ajs c (a, SlotId _ s) = assetSlotJSON $ AssetSlot a (Just (Slot c s))
  return $ Just $ JSON.toJSON $ map (\((c, rl), al) -> containerJSON c
    JSON..+? (records ?> "records" JSON..= map (rjs c) rl)
    JSON..+? (assets ?> "assets" JSON..= map (ajs c) al)) cl'
  where
  full = o == Just "all"
  assets = full || o == Just "assets"
  records = full || o == Just "records"
  nope = map (, [])
volumeJSONField vol "top" _ =
  Just . JSON.toJSON . containerJSON <$> cacheVolumeTopContainer vol
volumeJSONField vol "records" _ = do
  (l, _) <- cacheVolumeRecords vol
  return $ Just $ JSON.toJSON $ map recordJSON l
volumeJSONField vol "metrics" _ = do
  Just . JSON.toJSON . JSON.object . map (T.pack . show *** JSON.toJSON) <$> lookupVolumeMetrics vol
volumeJSONField o "excerpts" _ =
  Just . JSON.toJSON . map (\e -> sc (view e) $ excerptJSON e) <$> lookupVolumeExcerpts o
  where
  sc :: Id Container -> JSON.Object -> JSON.Object
  sc c a
    | HM.member "asset" a = HM.adjust (\(JSON.Object j) -> JSON.Object (sc c j)) "asset" a
    | otherwise = a JSON..+ "container" JSON..= c
volumeJSONField o "tags" n = do
  t <- cacheVolumeTopContainer o
  tc <- lookupSlotTagCoverage (containerSlot t) (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.toJSON $ map tagCoverageJSON tc
volumeJSONField o "comments" n = do
  t <- cacheVolumeTopContainer o
  tc <- lookupSlotComments (containerSlot t) (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.toJSON $ map commentJSON tc
volumeJSONField o "filename" _ =
  return $ Just $ JSON.toJSON $ makeFilename $ volumeDownloadName o
volumeJSONField _ _ _ = return Nothing

volumeJSONQuery :: Volume -> JSON.Query -> ActionM JSON.Object
volumeJSONQuery vol = runVolumeCache . JSON.jsonQuery (volumeJSON vol) (volumeJSONField vol)

volumeDownloadName :: Volume -> [T.Text]
volumeDownloadName v =
  (T.pack $ "databrary" ++ show (volumeId v))
    : map (T.takeWhile (',' /=) . snd) (volumeOwners v)
    ++ [fromMaybe (volumeName v) (getVolumeAlias v)]

viewVolume :: ActionRoute (API, Id Volume)
viewVolume = action GET (pathAPI </> pathId) $ \(api, vi) -> withAuth $ do
  when (api == HTML) angular
  v <- getVolume PermissionPUBLIC vi
  case api of
    JSON -> okResponse [] <$> (volumeJSONQuery v =<< peeks Wai.queryString)
    HTML -> peeks $ okResponse [] . htmlVolumeView v

volumeForm :: (Functor m, Monad m) => Volume -> DeformT f m Volume
volumeForm v = do
  name <- "name" .:> deform
  alias <- "alias" .:> deformNonEmpty deform
  body <- "body" .:> deformNonEmpty deform
  return v
    { volumeName = name
    , volumeAlias = alias
    , volumeBody = body
    }

volumeCitationForm :: Volume -> DeformActionM f Context (Volume, Maybe Citation)
volumeCitationForm v = do
  csrfForm
  vol <- volumeForm v
  cite <- "citation" .:> Citation
    <$> ("head" .:> deform)
    <*> ("url" .:> deformNonEmpty deform)
    <*> ("year" .:> deformNonEmpty deform)
    <$- Nothing
  look <- flatMapM (lift . focusIO . lookupCitation) $
    guard (T.null (volumeName vol) || T.null (citationHead cite) || isNothing (citationYear cite)) >> citationURL cite
  let fill = maybe cite (cite <>) look
      empty = T.null (citationHead fill) && isNothing (citationURL fill) && isNothing (citationYear fill)
      name 
        | Just title <- citationTitle fill
        , T.null (volumeName vol) = title
        | otherwise = volumeName vol
  _ <- "name" .:> deformRequired name
  when (not empty) $ void $
    "citation" .:> "name" .:> deformRequired (citationHead fill)
  return (vol{ volumeName = name }, empty ?!> fill)

viewVolumeEdit :: ActionRoute (Id Volume)
viewVolumeEdit = action GET (pathHTML >/> pathId </< "edit") $ \vi -> withAuth $ do
  angular
  v <- getVolume PermissionEDIT vi
  cite <- lookupVolumeCitation v
  peeks $ blankForm . htmlVolumeEdit (Just (v, cite))

viewVolumeCreate :: ActionRoute ()
viewVolumeCreate = action GET (pathHTML </< "volume" </< "create") $ \() -> withAuth $ do
  angular
  peeks $ blankForm . htmlVolumeEdit Nothing

postVolume :: ActionRoute (API, Id Volume)
postVolume = action POST (pathAPI </> pathId) $ \arg@(api, vi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  cite <- lookupVolumeCitation v
  (v', cite') <- runForm (api == HTML ?> htmlVolumeEdit (Just (v, cite))) $ volumeCitationForm v
  changeVolume v'
  r <- changeVolumeCitation v' cite'
  case api of
    JSON -> return $ okResponse [] $ volumeJSON v' JSON..+ "citation" JSON..= if r then cite' else cite
    HTML -> peeks $ otherRouteResponse [] viewVolume arg

createVolume :: ActionRoute API
createVolume = action POST (pathAPI </< "volume") $ \api -> withAuth $ do
  u <- peek
  (bv, cite, owner) <- runForm (api == HTML ?> htmlVolumeEdit Nothing) $ do
    csrfForm
    (bv, cite) <- volumeCitationForm blankVolume
    own <- "owner" .:> do
      oi <- deformOptional deform
      own <- maybe (return $ Just $ selfAuthorize u) (lift . lookupAuthorizeParent u) $ mfilter (peek u /=) oi
      deformMaybe' "You are not authorized to create volumes for that owner." $
        authorizeParent . authorization <$> mfilter ((PermissionADMIN <=) . accessMember) own
    auth <- lift $ lookupAuthorization own rootParty
    deformGuard "Insufficient site authorization to create volume." $
      PermissionEDIT <= accessSite auth
    return (bv, cite, own)
  v <- addVolume bv
  _ <- changeVolumeCitation v cite
  _ <- changeVolumeAccess $ VolumeAccess PermissionADMIN PermissionADMIN owner v
  case api of
    JSON -> return $ okResponse [] $ volumeJSON v
    HTML -> peeks $ otherRouteResponse [] viewVolume (api, volumeId v)

viewVolumeLinks :: ActionRoute (Id Volume)
viewVolumeLinks = action GET (pathHTML >/> pathId </< "link") $ \vi -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  links <- lookupVolumeLinks v
  peeks $ blankForm . htmlVolumeLinksEdit v links

postVolumeLinks :: ActionRoute (API, Id Volume)
postVolumeLinks = action POST (pathAPI </> pathId </< "link") $ \arg@(api, vi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  links <- lookupVolumeLinks v
  links' <- runForm (api == HTML ?> htmlVolumeLinksEdit v links) $ do
    csrfForm
    withSubDeforms $ Citation
      <$> ("head" .:> deform)
      <*> ("url" .:> (Just <$> deform))
      <$- Nothing
      <$- Nothing
  changeVolumeLinks v links'
  case api of
    JSON -> return $ okResponse [] $ volumeJSON v JSON..+ ("links" JSON..= links')
    HTML -> peeks $ otherRouteResponse [] viewVolume arg

volumeSearchForm :: (Applicative m, Monad m) => DeformT f m VolumeFilter
volumeSearchForm = VolumeFilter
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("party" .:> optional deform)
  <*> paginateForm

queryVolumes :: ActionRoute API
queryVolumes = action GET (pathAPI </< "volume") $ \api -> withAuth $ do
  when (api == HTML) angular
  vf <- runForm (api == HTML ?> htmlVolumeSearch mempty []) volumeSearchForm
  p <- findVolumes vf
  case api of
    JSON -> return $ okResponse [] $ JSON.toJSON $ map volumeJSON p
    HTML -> peeks $ blankForm . htmlVolumeSearch vf p

thumbVolume :: ActionRoute (Id Volume)
thumbVolume = action GET (pathId </< "thumb") $ \vi -> withAuth $ do
  v <- getVolume PermissionPUBLIC vi
  e <- lookupVolumeThumb v
  maybe
    (peeks $ otherRouteResponse [] webFile (Just $ staticPath ["images", "draft.png"]))
    (\as -> peeks $ otherRouteResponse [] downloadAssetSegment (slotId $ view as, view as))
    e
