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

import Control.Monad (when, unless, mfilter)
import qualified Data.Foldable as Fold
import Data.Maybe (isJust, fromMaybe, maybeToList)
import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), noContent204, movedPermanently301, conflict409)

import Databrary.Ops
import qualified Databrary.Iso as I
import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Release
import Databrary.Action.Response
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

getContainer :: Permission -> Maybe (Id Volume) -> Id Slot -> Bool -> ActionM Container
getContainer p mv (Id (SlotId i s)) top
  | segmentFull s = do
    c <- checkPermission p =<< maybeAction . maybe id (\v -> mfilter $ (v ==) . view) mv =<< lookupContainer i
    unless top $ do
      t <- lookupVolumeTopContainer (containerVolume c)
      when (containerId c == containerId t) $ result =<< peeks notFoundResponse
    return c
  | otherwise = result =<< peeks notFoundResponse

containerDownloadName :: Maybe (Id Container) -> Container -> [T.Text]
containerDownloadName top c
  | Fold.any (containerId c ==) top = ["materials"]
  | otherwise = (if containerTop c then ("materials" :) else id)
    $ T.pack (show (containerId c)) : maybeToList (containerName c)

viewContainer :: ActionRoute (API, (Maybe (Id Volume), Id Container))
viewContainer = I.second (I.second $ slotContainerId . unId I.:<->: containerSlotId) I.<$> viewSlot

containerForm :: Container -> DeformActionM () Context Container
containerForm c = do
  csrfForm
  name <- "name" .:> deformOptional (deformNonEmpty deform)
  top <- "top" .:> deformOptional deform
  date <- "date" .:> deformOptional (deformNonEmpty deform)
  release <- "release" .:> deformOptional (deformNonEmpty deform)
  return c
    { containerName = fromMaybe (containerName c) name
    , containerTop = fromMaybe (containerTop c) top
    , containerDate = fromMaybe (containerDate c) date
    , containerRelease = fromMaybe (containerRelease c) release
    }

viewContainerEdit :: ActionRoute (Maybe (Id Volume), Id Slot)
viewContainerEdit = action GET (pathHTML >/> pathMaybe pathId </> pathSlotId </< "edit") $ \(vi, ci) -> withAuth $ do
  when (isJust vi) $ angular
  c <- getContainer PermissionEDIT vi ci False
  unless (isJust vi) $
    result =<< peeks (redirectRouteResponse movedPermanently301 [] viewContainerEdit (Just (view c), containerSlotId (view c)))
  peeks $ blankForm . htmlContainerEdit (Right c)

createContainer :: ActionRoute (API, Id Volume)
createContainer = action POST (pathAPI </> pathId </< "slot") $ \(api, vi) -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  bc <- runForm (api == HTML ?> htmlContainerEdit (Left vol)) $ containerForm (blankContainer vol)
  c <- addContainer bc
  case api of
    JSON -> return $ okResponse [] $ containerJSON c
    HTML -> peeks $ otherRouteResponse [] viewContainer (api, (Just vi, containerId c))

postContainer :: ActionRoute (API, Id Slot)
postContainer = action POST (pathAPI </> pathSlotId) $ \(api, ci) -> withAuth $ do
  c <- getContainer PermissionEDIT Nothing ci False
  c' <- runForm (api == HTML ?> htmlContainerEdit (Right c)) $ containerForm c
  changeContainer c'
  when (containerRelease c' /= containerRelease c) $ do
    r <- changeRelease (containerSlot c') (containerRelease c')
    unless r $
      result $ emptyResponse conflict409 []
  case api of
    JSON -> return $ okResponse [] $ containerJSON c'
    HTML -> peeks $ otherRouteResponse [] viewSlot (api, (Just (view c'), ci))

deleteContainer :: ActionRoute (API, Id Slot)
deleteContainer = action DELETE (pathAPI </> pathSlotId) $ \(api, ci) -> withAuth $ do
  guardVerfHeader
  c <- getContainer PermissionEDIT Nothing ci False
  r <- removeContainer c
  unless r $ result $ case api of
    JSON -> response conflict409 [] (containerJSON c)
    HTML -> response conflict409 [] ("This container is not empty." :: T.Text)
  case api of
    JSON -> return $ emptyResponse noContent204 []
    HTML -> peeks $ otherRouteResponse [] viewVolume (api, view c)
