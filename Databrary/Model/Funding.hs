{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Funding
  ( module Databrary.Model.Funding.Types
  , lookupFunder
  , lookupVolumeFunding
  , changeVolumeFunding
  , removeVolumeFunder
  , funderJSON
  , fundingJSON
  ) where

import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Funding.Types
import Databrary.Model.Funding.SQL

useTPG

lookupFunder :: DBM m => Id Funder -> m (Maybe Funder)
lookupFunder fi =
  dbQuery1 $(selectQuery selectFunder "$WHERE funder.fundref_id = ${fi}")

lookupVolumeFunding :: (DBM m) => Volume -> m [Funding]
lookupVolumeFunding vol =
  dbQuery $(selectQuery selectVolumeFunding "$WHERE volume_funding.volume = ${volumeId vol}")

changeVolumeFunding :: DBM m => Volume -> Funding -> m Bool
changeVolumeFunding v Funding{..} =
  (0 <) . fst <$> updateOrInsert
    [pgSQL|UPDATE volume_funding SET awards = ${a} WHERE volume = ${volumeId v} AND funder = ${funderId fundingFunder}|]
    [pgSQL|INSERT INTO volume_funding (volume, funder, awards) VALUES (${volumeId v}, ${funderId fundingFunder}, ${a})|]
  where a = map Just fundingAwards

removeVolumeFunder :: DBM m => Volume -> Id Funder -> m Bool
removeVolumeFunder v f =
  dbExecute1 [pgSQL|DELETE FROM volume_funding WHERE volume = ${volumeId v} AND funder = ${f}|]

funderJSON :: Funder -> JSON.Object
funderJSON Funder{..} = JSON.object
  [ "id" JSON..= funderId
  , "name" JSON..= funderName
  ]

fundingJSON :: Funding -> JSON.Object
fundingJSON Funding{..} = JSON.object
  [ "funder" JSON..= funderJSON fundingFunder
  , "awards" JSON..= fundingAwards
  ]