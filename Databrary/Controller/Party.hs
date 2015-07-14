{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( getParty
  , viewParty
  , viewPartyEdit
  , viewPartyCreate
  , postParty
  , createParty
  , queryParties
  , viewAvatar
  ) where

import Control.Applicative (Applicative, (<*>), pure, optional)
import Control.Monad (unless, when, liftM2, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (mempty)
import qualified Data.Text.Encoding as TE
import qualified Data.Traversable as Trav
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..))

import Databrary.Ops
import Databrary.Has (view, MonadHas, peek, peeks, focusIO)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Time
import Databrary.Model.Enum
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.ORCID
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Model.Format
import Databrary.Store.Temp
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action.Route
import Databrary.Action.Auth
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.AssetSegment
import Databrary.Controller.Web
import Databrary.View.Party

getParty :: Maybe Permission -> PartyTarget -> AuthActionM Party
getParty (Just p) (TargetParty i) =
  checkPermission p =<< maybeAction =<< lookupAuthParty i
getParty _ mi = do
  u <- accountParty <$> authAccount
  let isme TargetProfile = True
      isme (TargetParty i) = partyId u == i
  unless (isme mi) $ result =<< forbiddenResponse
  return u

partyJSONField :: (MonadDB m, MonadHasIdentity c m, MonadHas Timestamp c m) => Party -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
partyJSONField p "parents" o = do
  now <- peek
  fmap (Just . JSON.toJSON) . mapM (\a -> do
    let ap = authorizeParent (authorization a)
    acc <- if access then Just . accessSite <$> lookupAuthorization ap rootParty else return Nothing
    return $ (if admin then authorizeJSON a else mempty)
      JSON..+ ("party" JSON..= (partyJSON ap JSON..+? (("access" JSON..=) <$> acc)))
      JSON..+? (admin && authorizeExpired a now ?> "expired" JSON..= True))
    =<< lookupAuthorizedParents p admin
  where
  admin = view p >= PermissionADMIN
  access = admin && o == Just "access"
partyJSONField p "children" _ =
  Just . JSON.toJSON . map (\a ->
    let ap = authorizeChild (authorization a) in
    (if admin then authorizeJSON a else mempty) JSON..+ ("party" JSON..= partyJSON ap))
    <$> lookupAuthorizedChildren p admin
  where admin = view p >= PermissionADMIN
partyJSONField p "volumes" ma = do
  Just . JSON.toJSON . map (\va -> 
    volumeAccessJSON va JSON..+ ("volume" JSON..= volumeJSON (volumeAccessVolume va)))
    <$> lookupPartyVolumeAccess p (fromMaybe PermissionNONE $ readDBEnum . BSC.unpack =<< ma)
partyJSONField p "access" _ = do
  Just . JSON.toJSON . accessSite <$> lookupAuthorization p rootParty
partyJSONField _ _ _ = return Nothing

partyJSONQuery :: (MonadDB m, MonadHasIdentity c m, MonadHas Timestamp c m) => Party -> JSON.Query -> m JSON.Object
partyJSONQuery p = JSON.jsonQuery (partyJSON p) (partyJSONField p)

viewParty :: AppRoute (API, PartyTarget)
viewParty = action GET (pathAPI </> pathPartyTarget) $ \(api, i) -> withAuth $ do
  when (api == HTML) angular
  p <- getParty (Just PermissionNONE) i
  case api of
    JSON -> okResponse [] =<< partyJSONQuery p =<< peeks Wai.queryString
    HTML -> okResponse [] =<< peeks (htmlPartyView p)

processParty :: API -> Maybe Party -> AuthActionM (Party, Maybe Asset)
processParty api p = do
  (p', a) <- runFormFiles [("avatar", maxAvatarSize)] (api == HTML ?> htmlPartyForm p) $ do
    csrfForm
    name <- "sortname" .:> (deformRequired =<< deform)
    prename <- "prename" .:> deformNonEmpty deform
    orcid <- "orcid" .:> deformNonEmpty (deformRead blankORCID)
    affiliation <- "affiliation" .:> deformNonEmpty deform
    url <- "url" .:> deformNonEmpty deform
    avatar <- "avatar" .:>
      (Trav.mapM (\a -> do
        f <- deformCheck "Must be an image." formatIsImage =<<
          deformMaybe' "Unknown or unsupported file format."
          (getFormatByFilename (fileName a))
        return (a, f)) =<< deform)
    return ((fromMaybe blankParty p)
      { partySortName = name
      , partyPreName = prename
      , partyORCID = orcid
      , partyAffiliation = affiliation
      , partyURL = url
      }, avatar)
  a' <- Trav.forM a $ \(af, fmt) -> do
    a' <- addAsset (blankAsset coreVolume)
      { assetFormat = fmt
      , assetRelease = Just ReleasePUBLIC
      , assetName = Just $ TE.decodeUtf8 $ fileName af
      } $ Just $ tempFilePath (fileContent af)
    focusIO $ releaseTempFile $ fileContent af
    return a'
  return (p', a')
  where maxAvatarSize = 10*1024*1024

viewPartyEdit :: AppRoute PartyTarget
viewPartyEdit = action GET (pathHTML >/> pathPartyTarget </< "edit") $ \i -> withAuth $ do
  angular
  p <- getParty (Just PermissionADMIN) i
  blankForm $ htmlPartyForm $ Just p

viewPartyCreate :: AppRoute ()
viewPartyCreate = action GET (pathHTML </< "party" </< "create") $ \() -> withAuth $ do
  checkMemberADMIN
  blankForm $ htmlPartyForm Nothing

postParty :: AppRoute (API, PartyTarget)
postParty = multipartAction $ action POST (pathAPI </> pathPartyTarget) $ \(api, i) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  (p', a) <- processParty api (Just p)
  changeParty p'
  when (isJust a) $
    void $ changeAvatar p' a
  case api of
    JSON -> okResponse [] $ partyJSON p'
    HTML -> redirectRouteResponse [] viewParty (api, i) []

createParty :: AppRoute API
createParty = multipartAction $ action POST (pathAPI </< "party") $ \api -> withAuth $ do
  checkMemberADMIN
  (bp, a) <- processParty api Nothing
  p <- addParty bp
  when (isJust a) $
    void $ changeAvatar p a
  case api of
    JSON -> okResponse [] $ partyJSON p
    HTML -> redirectRouteResponse [] viewParty (api, TargetParty $ partyId p) []

partySearchForm :: (Applicative m, Monad m) => DeformT f m PartyFilter
partySearchForm = PartyFilter
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("access" .:> optional deform)
  <*> ("institution" .:> optional deform)
  <*> ("authorize" .:> optional deform)
  <*> pure Nothing

queryParties :: AppRoute API
queryParties = action GET (pathAPI </< "party") $ \api -> withAuth $ do
  when (api == HTML) angular
  (pf, (limit, offset)) <- runForm (api == HTML ?> htmlPartySearchForm mempty) $
    liftM2 (,) partySearchForm paginationForm
  p <- findParties pf limit offset
  case api of
    JSON -> okResponse [] $ JSON.toJSON $ map partyJSON p
    HTML -> blankForm $ htmlPartySearchForm pf

viewAvatar :: AppRoute (Id Party)
viewAvatar = action GET (pathId </< "avatar") $ \i -> withoutAuth $
  maybe
    (redirectRouteResponse [] webFile (Just $ staticPath ["images", "avatar.png"]) [])
    (serveAssetSegment False . assetSlotSegment . assetNoSlot)
    =<< lookupAvatar i
