{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Pivot
  ( pivotVolume
  ) where

import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Volume
import Databrary.View.Pivot

pivotVolume :: ActionRoute (Id Volume)
pivotVolume = action GET (pathId </< "pivot") $ \vi -> withAuth $ do
  vol <- getVolume PermissionPUBLIC vi
  return $ okResponse [] $ htmlPivot vol
