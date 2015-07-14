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

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), noContent204, conflict409)

import Databrary.Ops
import Databrary.Has (view)
import Databrary.Action.Route
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

getRecord :: Permission -> Id Record -> AuthActionM Record
getRecord p i =
  checkPermission p =<< maybeAction =<< lookupRecord i

viewRecord :: AppRoute (API, Id Record)
viewRecord = action GET (pathAPI </> pathId) $ \(api, i) -> withAuth $ do
  rec <- getRecord PermissionPUBLIC i
  case api of
    JSON -> okResponse [] $ recordJSON rec
    HTML -> okResponse [] $ T.pack $ show $ recordId rec -- TODO

createRecord :: AppRoute (API, Id Volume)
createRecord = action POST (pathAPI </> pathId </< "record") $ \(api, vi) -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  br <- runForm (api == HTML ?> htmlRecordForm vol) $ do
    csrfForm
    cat <- "category" .:> (flatMapM ((`orElseM` Nothing <$ deformError "No such record category.") . getRecordCategory) =<< deformNonEmpty deform)
    return (blankRecord vol)
      { recordCategory = cat
      }
  rec <- addRecord br
  case api of
    JSON -> okResponse [] $ recordJSON rec
    HTML -> redirectRouteResponse [] viewRecord (api, recordId rec) []

postRecordMeasure :: AppRoute (API, Id Record, Id Metric)
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
    JSON -> okResponse [] $ recordJSON rec'
    HTML -> redirectRouteResponse [] viewRecord (api, recordId rec') []

deleteRecord :: AppRoute (API, Id Record)
deleteRecord = action DELETE (pathAPI </> pathId) $ \(api, ri) -> withAuth $ do
  guardVerfHeader
  rec <- getRecord PermissionEDIT ri
  r <- removeRecord rec
  guardAction r $ case api of
    JSON -> returnResponse conflict409 [] (recordJSON rec)
    HTML -> returnResponse conflict409 [] ("This record is still used." :: T.Text)
  case api of
    JSON -> emptyResponse noContent204 []
    HTML -> redirectRouteResponse [] viewVolume (api, view rec) []

postRecordSlot :: AppRoute (API, Id Slot, Id Record)
postRecordSlot = action POST (pathAPI </>> pathSlotId </> pathId) $ \(api, si, ri) -> withAuth $ do
  slot <- getSlot PermissionEDIT Nothing si
  rec <- getRecord PermissionEDIT ri
  src <- runForm Nothing $ do
    csrfForm
    "src" .:> deformNonEmpty deform
  r <- moveRecordSlot (RecordSlot rec slot{ slotSegment = fromMaybe emptySegment src }) (slotSegment slot)
  case api of
    HTML | r      -> redirectRouteResponse [] viewSlot (api, (Just (view slot), slotId slot)) []
      | otherwise -> redirectRouteResponse [] viewRecord (api, recordId rec) []
    JSON | r      -> okResponse [] $ recordSlotJSON (RecordSlot rec slot)
      | otherwise -> okResponse [] $ recordJSON rec

deleteRecordSlot :: AppRoute (API, Id Slot, Id Record)
deleteRecordSlot = action DELETE (pathAPI </>> pathSlotId </> pathId) $ \(api, si, ri) -> withAuth $ do
  guardVerfHeader
  slot <- getSlot PermissionEDIT Nothing si
  rec <- getRecord PermissionEDIT ri
  r <- moveRecordSlot (RecordSlot rec slot) emptySegment
  case api of
    HTML | r -> redirectRouteResponse [] viewRecord (api, recordId rec) []
    JSON | r -> okResponse [] $ recordJSON rec
    _ -> emptyResponse noContent204 []
