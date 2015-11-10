{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Audit
  ( viewSiteAudit
  ) where

import Control.Arrow (second)
import Data.Function (on)
import Data.IORef (readIORef)
import Data.List (nubBy)
import Data.Ord (comparing)

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.View.Audit

viewSiteAudit :: ActionRoute API
viewSiteAudit = action GET (pathAPI </< "audit") $ \api -> withAuth $ do
  ss <- focusIO $ readIORef . serviceStats
  vl <- map (second $ ("volume" JSON..=) . volumeJSON) . nubBy ((==) `on` volumeId . snd) <$> lookupVolumeActivity 8
  al <- map (second $ ("party"  JSON..=) . partyJSON)  . nubBy ((==) `on` partyId  . snd) <$> lookupAuthorizeActivity 8
  case api of 
    JSON -> return $ okResponse [] $ JSON.object
      [ "stats" JSON..= ss
      , "activity" JSON..= map ent (take 12 $ mergeBy ((fo .) . comparing fst) vl al)
      ]
    HTML -> peeks $ okResponse [] . htmlSiteAudit ss
  where
  ent (t, j) = JSON.object ["time" JSON..= t, j]
  fo GT = LT
  fo _ = GT
