{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Record
  ( getRecord
  , viewRecord
  , createRecord
  , postRecordMeasure
  , deleteRecord
  , postRecordSlot
  , deleteRecordSlot
  ) where

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), noContent204, conflict409)

import Databrary.Ops
import Databrary.Has
import Databrary.Action.Route
import Databrary.Action.Response
import Databrary.Action
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Permission
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Model.RecordSlot
import Databrary.Model.Metric
import Databrary.Model.Measure
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Slot
import Databrary.Controller.Permission
import Databrary.View.Record

getRecord :: Permission -> Id Record -> ActionM Record
getRecord p i =
  checkPermission p =<< maybeAction =<< lookupRecord i

viewRecord :: ActionRoute (API, Id Record)
viewRecord = action GET (pathAPI </> pathId) $ \(api, i) -> withAuth $ do
  rec <- getRecord PermissionPUBLIC i
  return $ case api of
    JSON -> okResponse [] $ recordJSON rec
    HTML -> okResponse [] $ T.pack $ show $ recordId rec -- TODO

createRecord :: ActionRoute (API, Id Volume)
createRecord = action POST (pathAPI </> pathId </< "record") $ \(api, vi) -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  br <- runForm (api == HTML ?> htmlRecordForm vol) $ do
    csrfForm
    cat <- "category" .:> (deformMaybe' "No such record category." . getRecordCategory =<< deform)
    return (blankRecord vol)
      { recordCategory = cat
      }
  rec <- addRecord br
  case api of
    JSON -> return $ okResponse [] $ recordJSON rec
    HTML -> peeks $ otherRouteResponse [] viewRecord (api, recordId rec)

postRecordMeasure :: ActionRoute (API, Id Record, Id Metric)
postRecordMeasure = action POST (pathAPI </>> pathId </> pathId) $ \(api, ri, mi) -> withAuth $ do
  rec <- getRecord PermissionEDIT ri
  met <- maybeAction $ getMetric mi
  let meas = Measure rec met
  rec' <- runForm (api == HTML ?> htmlRecordMeasureForm rec met) $ do
    csrfForm
    deformSync' ("datum" .:> deformNonEmpty deform)
    >>= maybe
      (lift $ removeRecordMeasure $ meas "")
      (\d -> do
        r <- lift $ changeRecordMeasure $ meas d
        when (isNothing r) $ deformError $ T.pack $ "Invalid " ++ show (metricType met)
        return $ fromMaybe rec r)
  case api of
    JSON -> return $ okResponse [] $ recordJSON rec'
    HTML -> peeks $ otherRouteResponse [] viewRecord (api, recordId rec')

deleteRecord :: ActionRoute (API, Id Record)
deleteRecord = action DELETE (pathAPI </> pathId) $ \(api, ri) -> withAuth $ do
  guardVerfHeader
  rec <- getRecord PermissionEDIT ri
  r <- removeRecord rec
  unless r $ result $ case api of
    JSON -> response conflict409 [] (recordJSON rec)
    HTML -> response conflict409 [] ("This record is still used." :: T.Text)
  case api of
    JSON -> return $ emptyResponse noContent204 []
    HTML -> peeks $ otherRouteResponse [] viewVolume (api, view rec)

postRecordSlot :: ActionRoute (API, Id Slot, Id Record)
postRecordSlot = action POST (pathAPI </>> pathSlotId </> pathId) $ \(api, si, ri) -> withAuth $ do
  slot <- getSlot PermissionEDIT Nothing si
  rec <- getRecord PermissionEDIT ri
  src <- runForm Nothing $ do
    csrfForm
    "src" .:> deformNonEmpty deform
  r <- moveRecordSlot (RecordSlot rec slot{ slotSegment = fromMaybe emptySegment src }) (slotSegment slot)
  case api of
    HTML | r      -> peeks $ otherRouteResponse [] viewSlot (api, (Just (view slot), slotId slot))
      | otherwise -> peeks $ otherRouteResponse [] viewRecord (api, recordId rec)
    JSON | r      -> return $ okResponse [] $ recordSlotJSON (RecordSlot rec slot)
      | otherwise -> return $ okResponse [] $ recordJSON rec

deleteRecordSlot :: ActionRoute (API, Id Slot, Id Record)
deleteRecordSlot = action DELETE (pathAPI </>> pathSlotId </> pathId) $ \(api, si, ri) -> withAuth $ do
  guardVerfHeader
  slot <- getSlot PermissionEDIT Nothing si
  rec <- getRecord PermissionEDIT ri
  r <- moveRecordSlot (RecordSlot rec slot) emptySegment
  case api of
    HTML | r -> peeks $ otherRouteResponse [] viewRecord (api, recordId rec)
    JSON | r -> return $ okResponse [] $ recordJSON rec
    _ -> return $ emptyResponse noContent204 []
