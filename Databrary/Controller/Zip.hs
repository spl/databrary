{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Databrary.Controller.Zip
  ( zipContainer
  , zipVolume
  , viewVolumeDescription
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as Fold
import Data.Function (on)
import Data.List (groupBy, partition, foldl')
import Data.Maybe (fromJust, maybeToList, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.RangeSet.List as RS
import qualified Data.RangeSet.Parse as RS
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (hContentType, hCacheControl, hContentLength)
import qualified Network.Wai as Wai
import System.Posix.FilePath ((<.>))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html

import Databrary.Ops
import Databrary.Has (view, peek, peeks)
import Databrary.Store.Asset
import Databrary.Store.Filename
import Databrary.Store.Zip
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.RecordSlot
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.Format
import Databrary.Model.Party
import Databrary.Model.Citation
import Databrary.Model.Funding
import Databrary.HTTP
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Asset
import Databrary.Controller.Container
import Databrary.Controller.Volume
import Databrary.Controller.Party
import Databrary.Controller.CSV
import Databrary.Controller.Angular
import Databrary.View.Zip

assetZipEntry :: AssetSlot -> ActionM ZipEntry
assetZipEntry AssetSlot{ slotAsset = a } = do
  Just f <- getAssetFile a
  req <- peek
  -- (t, _) <- assetCreation a
  -- Just (t, s) <- fileInfo f
  return blankZipEntry
    { zipEntryName = makeFilename (assetDownloadName a) `addFormatExtension` assetFormat a
    , zipEntryTime = Nothing
    , zipEntryComment = BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) viewAsset (HTML, assetId a) []
    , zipEntryContent = ZipEntryFile (fromIntegral $ fromJust $ assetSize a) f
    }

containerZipEntry :: Container -> [AssetSlot] -> ActionM ZipEntry
containerZipEntry c l = do
  req <- peek
  a <- mapM assetZipEntry l
  return blankZipEntry
    { zipEntryName = makeFilename (containerDownloadName c)
    , zipEntryComment = BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) viewContainer (HTML, (Nothing, containerId c)) []
    , zipEntryContent = ZipDirectory a
    }

volumeDescription :: Bool -> Volume -> Bool -> [AssetSlot] -> ActionM (Html.Html, [[AssetSlot]], [[AssetSlot]])
volumeDescription inzip v full al = do
  cite <- lookupVolumeCitation v
  links <- lookupVolumeLinks v
  fund <- lookupVolumeFunding v
  desc <- peeks $ htmlVolumeDescription inzip v (maybeToList cite ++ links) fund full at ab
  return (desc, at, ab)
  where
  (at, ab) = partition (Fold.any (containerTop . slotContainer) . assetSlot . head) $ groupBy (me `on` fmap (containerId . slotContainer) . assetSlot) al
  me (Just x) (Just y) = x == y
  me _ _ = False

volumeZipEntry :: Volume -> Bool -> Maybe BSB.Builder -> [AssetSlot] -> ActionM ZipEntry
volumeZipEntry v full csv al = do
  req <- peek
  (desc, at, ab) <- volumeDescription True v full al
  zt <- mapM ent at
  zb <- mapM ent ab
  return blankZipEntry
    { zipEntryName = makeFilename $ volumeDownloadName v ++ if full then [] else ["PARTIAL"]
    , zipEntryComment = BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) viewVolume (HTML, volumeId v) []
    , zipEntryContent = ZipDirectory
      $ blankZipEntry
        { zipEntryName = "description.html"
        , zipEntryContent = ZipEntryPure $ Html.renderHtml $ desc
        }
      : maybe id (\c -> (blankZipEntry
        { zipEntryName = "spreadsheet.csv"
        , zipEntryContent = ZipEntryPure $ BSB.toLazyByteString c
        } :)) csv
      (if null zb then zt else (zt ++ [blankZipEntry
        { zipEntryName = "sessions"
        , zipEntryContent = ZipDirectory zb
        }]))
    }
  where
  ent [a@AssetSlot{ assetSlot = Nothing }] = assetZipEntry a
  ent l@(AssetSlot{ assetSlot = Just s } : _) = containerZipEntry (slotContainer s) l
  ent _ = fail "volumeZipEntry"

zipResponse :: BS.ByteString -> [ZipEntry] -> ActionM Response
zipResponse n z = do
  req <- peek
  u <- peek
  let comment = BSL.toStrict $ BSB.toLazyByteString
        $ BSB.string8 "Downloaded by " <> TE.encodeUtf8Builder (partyName u) <> BSB.string8 " <" <> actionURL (Just req) viewParty (HTML, TargetParty $ partyId u) [] <> BSB.char8 '>'
  return $ okResponse
    [ (hContentType, "application/zip")
    , ("content-disposition", "attachment; filename=" <> quoteHTTP (n <.> "zip"))
    , (hCacheControl, "max-age=31556926, private")
    , (hContentLength, BSC.pack $ show $ sizeZip z + fromIntegral (BS.length comment))
    ] (streamZip z comment)

zipEmpty :: ZipEntry -> Bool
zipEmpty ZipEntry{ zipEntryContent = ZipDirectory l } = all zipEmpty l
zipEmpty _ = False

checkAsset :: AssetSlot -> Bool
checkAsset a = dataPermission a > PermissionNONE && assetBacked (view a)

zipContainer :: ActionRoute (Maybe (Id Volume), Id Slot)
zipContainer = action GET (pathMaybe pathId </> pathSlotId </< "zip") $ \(vi, ci) -> withAuth $ do
  c <- getContainer PermissionPUBLIC vi ci True
  z <- containerZipEntry c . filter checkAsset =<< lookupContainerAssets c
  auditSlotDownload (not $ zipEmpty z) (containerSlot c)
  zipResponse ("databrary-" <> BSC.pack (show (volumeId (containerVolume c))) <> "-" <> BSC.pack (show (containerId c))) [z]

inclExcl :: Wai.Request -> RS.RSet (Id Container)
inclExcl = pe . mapMaybe ie . Wai.queryString where
  pe [] = RS.full
  pe (h:l) = foldl' ae (either RS.complement id h) l
  ae s (Right i) = s `RS.union` i
  ae s (Left e) = s `RS.difference` e
  ie (k, Nothing) = Right <$> ps k
  ie (k, Just v)
    | k `BSC.isPrefixOf` "include" = Right <$> ps v
    | k `BSC.isPrefixOf` "exclude" = Left <$> ps v
  ie _ = Nothing
  ps = RS.parseRangeSet . BSC.unpack

zipVolume :: ActionRoute (Id Volume)
zipVolume = action GET (pathId </< "zip") $ \vi -> withAuth $ do
  s <- peeks inclExcl
  let full = s == RS.full
  v <- getVolume PermissionPUBLIC vi
  _:cr <- lookupVolumeContainersRecords v
  let cr' = filter ((`RS.member` s) . containerId . fst) cr
  csv <- null cr' ?!$> volumeCSV v cr'
  a <- filter (\a@AssetSlot{ assetSlot = Just c } -> checkAsset a && RS.member (containerId (slotContainer c)) s) <$>
    lookupVolumeAssetSlots v False
  z <- volumeZipEntry v full csv a
  auditVolumeDownload (not $ null a) v
  zipResponse (BSC.pack $ "databrary-" ++ show (volumeId v) ++ if full then "" else "-partial") [z]

viewVolumeDescription :: ActionRoute (Id Volume)
viewVolumeDescription = action GET (pathId </< "description") $ \vi -> withAuth $ do
  angular
  v <- getVolume PermissionPUBLIC vi
  al <- filter checkAsset <$> lookupVolumeAssetSlots v False
  (desc, _, _) <- volumeDescription False v True al
  return $ okResponse [] desc
