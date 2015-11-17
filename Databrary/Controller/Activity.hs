{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Activity
  ( viewSiteActivity
  , viewVolumeActivity
  ) where

import Control.Arrow (second)
import Control.Monad (when)
import Data.Function (on)
import Data.IORef (readIORef)
import Data.List (nubBy)
import Data.Ord (comparing)

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Activity
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Angular
import Databrary.Controller.Volume
import Databrary.View.Activity

viewSiteActivity :: ActionRoute API
viewSiteActivity = action GET (pathAPI </< "activity") $ \api -> withAuth $ do
  ss <- focusIO $ readIORef . serviceStats
  vl <- map (second $ ("volume" JSON..=) . volumeJSON) . nubBy ((==) `on` volumeId . volumeRow . snd) <$> lookupVolumeShareActivity 8
  al <- map (second $ ("party"  JSON..=) . partyJSON)  . nubBy ((==) `on` partyId  . partyRow  . snd) <$> lookupAuthorizeActivity 8
  case api of 
    JSON -> return $ okResponse [] $ JSON.object
      [ "stats" JSON..= ss
      , "activity" JSON..= map ent (take 12 $ mergeBy ((fo .) . comparing fst) vl al)
      ]
    HTML -> peeks $ okResponse [] . htmlSiteActivity ss
  where
  ent (t, j) = JSON.object ["time" JSON..= t, j]
  fo GT = LT
  fo _ = GT

viewVolumeActivity :: ActionRoute (API, Id Volume)
viewVolumeActivity = action GET (pathAPI </> pathId </< "activity") $ \(api, vi) -> withAuth $ do
  when (api == HTML) angular
  v <- getVolume PermissionEDIT vi
  a <- lookupVolumeActivity v
  return $ case api of
    JSON -> okResponse [] $ JSON.toJSON $ map activityJSON a
