{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Slot
  ( getSlot
  , viewSlot
  , slotDownloadName
  ) where

import Control.Monad (when, mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has (view, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.Model.Excerpt
import Databrary.Model.Record
import Databrary.Model.RecordSlot
import Databrary.Model.Tag
import Databrary.Model.Comment
import Databrary.Store.Filename
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Angular
import Databrary.Controller.Container

getSlot :: Permission -> Maybe (Id Volume) -> Id Slot -> AuthActionM Slot
getSlot p mv i =
  checkPermission p =<< maybeAction . maybe id (\v -> mfilter $ (v ==) . view) mv =<< lookupSlot i

slotJSONField :: (MonadDB m, MonadHasIdentity c m) => Slot -> BS.ByteString -> Maybe BS.ByteString -> m (Maybe JSON.Value)
slotJSONField o "assets" _ =
  Just . JSON.toJSON . map assetSlotJSON <$> lookupSlotAssets o
slotJSONField o "records" _ =
  Just . JSON.toJSON . map (\r -> recordSlotJSON r JSON..+ ("record" JSON..= recordJSON (slotRecord r))) <$> lookupSlotRecords o
slotJSONField o "tags" n = do
  tc <- lookupSlotTagCoverage o (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.recordMap $ map tagCoverageJSON tc
slotJSONField o "comments" n = do
  c <- lookupSlotComments o (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.toJSON $ map commentJSON c
slotJSONField o "excerpts" _ =
  Just . JSON.toJSON . map excerptJSON <$> lookupSlotExcerpts o
slotJSONField o "filename" _ =
  return $ Just $ JSON.toJSON $ makeFilename $ slotDownloadName o
slotJSONField _ _ _ = return Nothing

slotJSONQuery :: (MonadDB m, MonadHasIdentity c m) => Slot -> JSON.Query -> m JSON.Object
slotJSONQuery o = JSON.jsonQuery (slotJSON o) (slotJSONField o)

slotDownloadName :: Slot -> [T.Text]
slotDownloadName s =
  containerDownloadName (slotContainer s)

viewSlot :: AppRoute (API, (Maybe (Id Volume), Id Slot))
viewSlot = action GET (pathAPI </> pathMaybe pathId </> pathSlotId) $ \(api, (vi, i)) -> withAuth $ do
  when (api == HTML) angular
  c <- getSlot PermissionPUBLIC vi i
  case api of
    JSON -> okResponse [] =<< slotJSONQuery c =<< peeks Wai.queryString
    HTML -> okResponse [] $ BSC.pack $ show $ containerId $ slotContainer c -- TODO
