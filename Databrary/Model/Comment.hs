{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Comment
  ( module Databrary.Model.Comment.Types
  , blankComment
  , lookupComment
  , lookupSlotComments
  , lookupVolumeCommentRows
  , addComment
  , commentJSON
  ) where

import Data.Int (Int64)
import Data.Maybe (catMaybes, listToMaybe)
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Volume.Types
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Comment.Types
import Databrary.Model.Comment.SQL

blankComment :: Account -> Slot -> Comment
blankComment who slot = Comment
  { commentId = error "blankComment"
  , commentWho = who
  , commentSlot = slot
  , commentTime = error "blankComment"
  , commentText = ""
  , commentParents = []
  }

lookupComment :: (MonadDB c m, MonadHasIdentity c m) => Id Comment -> m (Maybe Comment)
lookupComment i = do
  ident <- peek
  dbQuery1 $(selectQuery (selectComment 'ident) "$!WHERE comment.id = ${i}")

lookupSlotComments :: (MonadDB c m, MonadHasIdentity c m) => Slot -> Int -> m [Comment]
lookupSlotComments (Slot c s) n = do
  ident <- peek
  dbQuery $ ($ c) <$> $(selectQuery (selectContainerComment 'ident) "$!WHERE comment.container = ${containerId c} AND comment.segment && ${s} ORDER BY comment.thread LIMIT ${fromIntegral n :: Int64}")

lookupVolumeCommentRows :: MonadDB c m => Volume -> m [CommentRow]
lookupVolumeCommentRows v =
  dbQuery $(selectQuery selectCommentRow "JOIN container ON comment.container = container.id WHERE container.volume = ${volumeId v} ORDER BY container")

addComment :: MonadDB c m => Comment -> m Comment
addComment c@Comment{..} = do
  (i, t) <- dbQuery1' [pgSQL|INSERT INTO comment (who, container, segment, text, parent) VALUES (${partyId $ accountParty commentWho}, ${containerId $ slotContainer commentSlot}, ${slotSegment commentSlot}, ${commentText}, ${listToMaybe commentParents}) RETURNING id, time|]
  return c
    { commentId = i
    , commentTime = t
    }

commentJSON :: Comment -> JSON.Object
commentJSON Comment{ commentSlot = Slot{..}, ..} = JSON.record commentId $ catMaybes
  [ Just $ "container" JSON..= containerJSON slotContainer
  , segmentJSON slotSegment
  , Just $ "who" JSON..= partyJSON (accountParty commentWho)
  , Just $ "time" JSON..= commentTime
  , Just $ "text" JSON..= commentText
  , null commentParents ?!> "parents" JSON..= commentParents
  ]
