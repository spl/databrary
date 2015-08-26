module Databrary.Controller.Metric
  ( postVolumeMetric
  , deleteVolumeMetric
  ) where

import Network.HTTP.Types (StdMethod(PUT, DELETE))

import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.Model.VolumeMetric
import Databrary.HTTP.Path.Parser
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Volume

postVolumeMetric :: ActionRoute (Id Volume, (Id RecordCategory, Maybe (Id Metric)))
postVolumeMetric = action PUT (pathJSON >/> pathId </> (pathId </> pathMaybe pathId)) $ \(vi, (c, mm)) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  r <- maybe (addVolumeCategory v c) (\m -> do
    r <- addVolumeMetric v c m
    return $ if r then [m] else []) mm
  return $ okResponse [] $ JSON.toJSON r

deleteVolumeMetric :: ActionRoute (Id Volume, (Id RecordCategory, Maybe (Id Metric)))
deleteVolumeMetric = action DELETE (pathJSON >/> pathId </> (pathId </> pathMaybe pathId)) $ \(vi, (c, mm)) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  r <- maybe (removeVolumeCategory v c) (fmap fromEnum . removeVolumeMetric v c) mm
  return $ okResponse [] $ JSON.toJSON r
