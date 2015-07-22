{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Container
  ( getContainer
  , viewContainer
  , viewContainerEdit
  , createContainer
  , postContainer
  , deleteContainer
  , containerDownloadName
  ) where

import Control.Monad (when, mfilter)
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), noContent204, conflict409)

import Databrary.Ops
import qualified Databrary.Iso as I
import Databrary.Has (view)
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Release
import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.Volume
import {-# SOURCE #-} Databrary.Controller.Slot
import Databrary.View.Container

getContainer :: Permission -> Maybe (Id Volume) -> Id Slot -> AuthActionM Container
getContainer p mv (Id (SlotId i s))
  | segmentFull s = checkPermission p =<< maybeAction . maybe id (\v -> mfilter $ (v ==) . view) mv =<< lookupContainer i
  | otherwise = result =<< notFoundResponse

containerDownloadName :: Container -> [T.Text]
containerDownloadName c = T.pack (show (containerId c)) : maybeToList (containerName c)

viewContainer :: AppRoute (API, (Maybe (Id Volume), Id Container))
viewContainer = I.second (I.second $ slotContainerId . unId I.:<->: containerSlotId) I.<$> viewSlot

containerForm :: Container -> DeformActionM () AuthRequest Container
containerForm c = do
  csrfForm
  name <- "name" .:> deformOptional (deformNonEmpty deform)
  date <- "date" .:> deformOptional (deformNonEmpty deform)
  release <- "release" .:> deformOptional (deformNonEmpty deform)
  return c
    { containerName = fromMaybe (containerName c) name
    , containerDate = fromMaybe (containerDate c) date
    , containerRelease = fromMaybe (containerRelease c) release
    }

viewContainerEdit :: AppRoute (Maybe (Id Volume), Id Slot)
viewContainerEdit = action GET (pathHTML >/> pathMaybe pathId </> pathSlotId </< "edit") $ \(vi, ci) -> withAuth $ do
  angular
  c <- getContainer PermissionEDIT vi ci
  blankForm $ htmlContainerEdit (Right c)

createContainer :: AppRoute (API, Id Volume)
createContainer = action POST (pathAPI </> pathId </< "slot") $ \(api, vi) -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  bc <- runForm (api == HTML ?> htmlContainerEdit (Left vol)) $ do
    top <- "top" .:> deform
    containerForm (blankContainer vol)
      { containerTop = top }
  c <- addContainer bc
  case api of
    JSON -> okResponse [] $ containerJSON c
    HTML -> redirectRouteResponse [] viewContainer (api, (Just vi, containerId c)) []

postContainer :: AppRoute (API, Id Slot)
postContainer = action POST (pathAPI </> pathSlotId) $ \(api, ci) -> withAuth $ do
  c <- getContainer PermissionEDIT Nothing ci
  c' <- runForm (api == HTML ?> htmlContainerEdit (Right c)) $ containerForm c
  changeContainer c'
  when (containerRelease c' /= containerRelease c) $ do
    r <- changeRelease (containerSlot c') (containerRelease c')
    guardAction r $
      emptyResponse conflict409 []
  case api of
    JSON -> okResponse [] $ containerJSON c'
    HTML -> redirectRouteResponse [] viewSlot (api, (Just (view c'), ci)) []

deleteContainer :: AppRoute (API, Id Slot)
deleteContainer = action DELETE (pathAPI </> pathSlotId) $ \(api, ci) -> withAuth $ do
  guardVerfHeader
  c <- getContainer PermissionEDIT Nothing ci
  r <- removeContainer c
  guardAction r $ case api of
    JSON -> returnResponse conflict409 [] (containerJSON c)
    HTML -> returnResponse conflict409 [] ("This container is not empty." :: T.Text)
  case api of
    JSON -> emptyResponse noContent204 []
    HTML -> redirectRouteResponse [] viewVolume (api, view c) []
