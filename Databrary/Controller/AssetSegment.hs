{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.Controller.AssetSegment
  ( getAssetSegment
  , viewAssetSegment
  , serveAssetSegment
  , downloadAssetSegment
  , thumbAssetSegment
  ) where

import Control.Monad ((<=<), join, when, mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.Wai as Wai
import Text.Read (readMaybe)

import Databrary.Ops
import Databrary.Has (view, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Volume
import Databrary.Model.Slot
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Store.Asset
import Databrary.Store.AssetSegment
import Databrary.Store.Filename
import Databrary.HTTP.File
import Databrary.HTTP.Request
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Angular
import Databrary.Controller.Permission
import Databrary.Controller.Volume
import Databrary.Controller.Slot
import Databrary.Controller.Asset
import Databrary.Controller.Format

getAssetSegment :: Permission -> Maybe (Id Volume) -> Id Slot -> Id Asset -> AuthActionM AssetSegment
getAssetSegment p mv s a =
  checkPermission p =<< maybeAction . maybe id (\v -> mfilter $ (v ==) . view) mv =<< lookupSlotAssetSegment s a

assetSegmentJSONField :: (MonadDB m, MonadHasIdentity c m) => AssetSegment -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
assetSegmentJSONField a v o = assetJSONField (segmentAsset a) v o

assetSegmentJSONQuery :: (MonadDB m, MonadHasIdentity c m) => AssetSegment -> JSON.Query -> m JSON.Object
assetSegmentJSONQuery vol = JSON.jsonQuery (assetSegmentJSON vol) (assetSegmentJSONField vol)

assetSegmentDownloadName :: AssetSegment -> [T.Text]
assetSegmentDownloadName a =
  volumeDownloadName (view a) ++ slotDownloadName (view a) ++ assetDownloadName (view a)

viewAssetSegment :: AppRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
viewAssetSegment = action GET (pathAPI </>>> pathMaybe pathId </>> pathSlotId </> pathId) $ \(api, vi, si, ai) -> withAuth $ do
  when (api == HTML) angular
  as <- getAssetSegment PermissionPUBLIC vi si ai
  case api of
    JSON -> okResponse [] =<< assetSegmentJSONQuery as =<< peeks Wai.queryString
    HTML -> okResponse [] $ T.pack $ show $ assetId $ slotAsset $ segmentAsset as -- TODO

serveAssetSegment :: Bool -> AssetSegment -> AuthAction
serveAssetSegment dl as = do
  _ <- checkDataPermission as
  sz <- peeks $ readMaybe . BSC.unpack <=< join . listToMaybe . lookupQueryParameters "size"
  when dl $ auditAssetSegmentDownload True as
  store <- maybeAction =<< getAssetFile a
  (hd, part) <- fileResponse store (view as) (dl ?> makeFilename (assetSegmentDownloadName as)) (BSL.toStrict $ BSB.toLazyByteString $
    BSB.byteStringHex (fromJust (assetSHA1 a)) <> BSB.string7 (assetSegmentTag as sz))
  either
    (okResponse hd)
    (okResponse hd . (, part))
    =<< getAssetSegmentStore as sz
  where
  a = slotAsset $ segmentAsset as

downloadAssetSegment :: AppRoute (Id Slot, Id Asset)
downloadAssetSegment = action GET (pathSlotId </> pathId </< "download") $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment PermissionPUBLIC Nothing si ai
  inline <- peeks $ boolQueryParameter "inline"
  serveAssetSegment (not inline) as

thumbAssetSegment :: AppRoute (Id Slot, Id Asset)
thumbAssetSegment = action GET (pathSlotId </> pathId </< "thumb") $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment PermissionPUBLIC Nothing si ai
  q <- peeks Wai.queryString
  let as' = assetSegmentInterp 0.25 as
  if formatIsImage (view as') && assetBacked (view as)
    then do
      redirectRouteResponse [] downloadAssetSegment (slotId $ view as', assetId $ view as') q
    else
      redirectRouteResponse [] formatIcon (view as) q
