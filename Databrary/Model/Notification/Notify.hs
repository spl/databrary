{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Notification.Notify
  ( lookupNotify
  , lookupAccountNotify
  , changeNotify
  , removeNotify
  ) where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Notification.Notice

useTDB

lookupNotify :: MonadDB c m => Account -> Notice -> m Delivery
lookupNotify a n =
  dbQuery1' [pgSQL|!SELECT delivery FROM notify_view WHERE target = ${partyId $ partyRow $ accountParty a} AND notice = ${n}|]

lookupAccountNotify :: MonadDB c m => Account -> m (Map.Map Notice Delivery)
lookupAccountNotify a = Map.fromDistinctAscList <$>
  dbQuery [pgSQL|!SELECT notice, delivery FROM notify_view WHERE target = ${partyId $ partyRow $ accountParty a} ORDER BY notice|]

changeNotify :: MonadDB c m => Account -> Notice -> Delivery -> m ()
changeNotify a n d = do
  (r, _) <- updateOrInsert
    [pgSQL|UPDATE notify SET delivery = ${d} WHERE target = ${partyId $ partyRow $ accountParty a} AND notice = ${n}|]
    [pgSQL|INSERT INTO notify (target, notice, delivery) VALUES (${partyId $ partyRow $ accountParty a}, ${n}, ${d})|]
  when (r /= 1) $ fail $ "changeNotify: " ++ show r ++ " rows"

-- |This resets to the default value (not necessarily DeliveryNone).
removeNotify :: MonadDB c m => Account -> Notice -> m Bool
removeNotify a n =
  dbExecute1 [pgSQL|DELETE FROM notify WHERE target = ${partyId $ partyRow $ accountParty a} AND notice = ${n}|]
