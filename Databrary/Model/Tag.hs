{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Tag
  ( module Databrary.Model.Tag.Types
  , lookupTag
  , lookupTags
  , findTags
  , addTag
  , lookupVolumeTagUseRows
  , addTagUse
  , removeTagUse
  , lookupTopTagWeight
  , lookupTagCoverage
  , lookupSlotTagCoverage
  , lookupSlotKeywords
  , tagWeightJSON
  , tagCoverageJSON
  ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Maybe (fromMaybe, catMaybes)
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Tag.SQL

lookupTag :: MonadDB c m => TagName -> m (Maybe Tag)
lookupTag n =
  dbQuery1 $(selectQuery selectTag "$WHERE tag.name = ${n}::varchar")

lookupTags :: MonadDB c m => m [Tag]
lookupTags = dbQuery $(selectQuery selectTag "")

findTags :: MonadDB c m => TagName -> Int -> m [Tag]
findTags (TagName n) lim = -- TagName restrictions obviate pattern escaping
  dbQuery $(selectQuery selectTag "$WHERE tag.name LIKE ${n `BSC.snoc` '%'}::varchar LIMIT ${fromIntegral lim :: Int64}")

addTag :: MonadDB c m => TagName -> m Tag
addTag n =
  dbQuery1' $ (`Tag` n) <$> [pgSQL|!SELECT get_tag(${n})|]

lookupVolumeTagUseRows :: MonadDB c m => Volume -> m [TagUseRow]
lookupVolumeTagUseRows v =
  dbQuery $(selectQuery selectTagUseRow "JOIN container ON tag_use.container = container.id WHERE container.volume = ${volumeId v} ORDER BY container.id")

addTagUse :: MonadDB c m => TagUse -> m Bool
addTagUse t = either (const False) id <$> do
  dbTryJust (guard . isExclusionViolation)
    $ dbExecute1 (if tagKeyword t
      then $(insertTagUse True 't)
      else $(insertTagUse False 't))

removeTagUse :: MonadDB c m => TagUse -> m Int
removeTagUse t =
  dbExecute
    (if tagKeyword t 
      then $(deleteTagUse True 't)
      else $(deleteTagUse False 't))

lookupTopTagWeight :: MonadDB c m => Int -> m [TagWeight]
lookupTopTagWeight lim =
  dbQuery $(selectQuery (selectTagWeight "") "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

emptyTagCoverage :: Tag -> Container -> TagCoverage
emptyTagCoverage t c = TagCoverage (TagWeight t 0) c [] [] []

lookupTagCoverage :: (MonadDB c m, MonadHasIdentity c m) => Tag -> Slot -> m TagCoverage
lookupTagCoverage t (Slot c s) = do
  ident <- peek
  fromMaybe (emptyTagCoverage t c) <$> dbQuery1 (($ c) . ($ t) <$> $(selectQuery (selectTagCoverage 'ident "WHERE container = ${containerId c} AND segment && ${s} AND tag = ${tagId t}") "$!"))

lookupSlotTagCoverage :: (MonadDB c m, MonadHasIdentity c m) => Slot -> Int -> m [TagCoverage]
lookupSlotTagCoverage slot lim = do
  ident <- peek
  dbQuery $(selectQuery (selectSlotTagCoverage 'ident 'slot) "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

lookupSlotKeywords :: (MonadDB c m) => Slot -> m [Tag]
lookupSlotKeywords Slot{..} =
  dbQuery $(selectQuery selectTag "JOIN keyword_use ON id = tag WHERE container = ${containerId slotContainer} AND segment = ${slotSegment}")

tagWeightJSON :: TagWeight -> JSON.Object
tagWeightJSON TagWeight{..} = JSON.record (tagName tagWeightTag) $
  [ "weight" JSON..= tagWeightWeight
  ]

tagCoverageJSON :: TagCoverage -> JSON.Object
tagCoverageJSON TagCoverage{..} = tagWeightJSON tagCoverageWeight JSON..++ catMaybes
  [ Just $ "coverage" JSON..= tagCoverageSegments
  , null tagCoverageKeywords ?!> "keyword" JSON..= tagCoverageKeywords
  , null tagCoverageVotes ?!> "vote" JSON..= tagCoverageVotes
  ]
