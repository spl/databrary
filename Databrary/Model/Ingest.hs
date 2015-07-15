{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Ingest
  ( IngestKey
  , lookupIngestContainer
  , addIngestContainer
  ) where

import qualified Data.Text as T
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL

type IngestKey = T.Text

lookupIngestContainer :: MonadDB m => Volume -> IngestKey -> m (Maybe Container)
lookupIngestContainer vol k =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeContainer "JOIN ingest.container AS ingest USING (id, volume) WHERE ingest.key = ${k} AND container.volume = ${volumeId vol}")

addIngestContainer :: MonadDB m => Container -> IngestKey -> m ()
addIngestContainer c k =
  dbExecute1' [pgSQL|INSERT INTO ingest.container (id, volume, key) VALUES (${containerId c}, ${volumeId $ containerVolume c}, ${k})|]
