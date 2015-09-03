{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Comment.SQL
  ( selectContainerComment
  , selectComment
  , selectCommentRow
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Comment.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Segment
import Databrary.Model.Slot.Types

makeComment :: Id Comment -> Segment -> Timestamp -> T.Text -> [Maybe (Id Comment)] -> Account -> Container -> Comment
makeComment i s t x p w c = Comment i w (Slot c s) t x (map (fromMaybe (error "NULL comment thread")) p)

commentRow :: Selector -- ^ @'Account' -> 'Container' -> 'Comment'@
commentRow = selectColumns 'makeComment "comment" ["id", "segment", "time", "text", "thread"]

selectAccountContainerComment :: Selector -- ^ @'Account' -> 'Container' -> 'Comment'@
selectAccountContainerComment = fromMap ("comment_thread AS " ++) commentRow

selectContainerComment :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Container' -> 'Comment'@
selectContainerComment ident = selectJoin '($)
  [ selectAccountContainerComment
  , joinOn "comment.who = account.id"
    $ selectAccount ident
  ]

selectComment :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Comment'@
selectComment ident = selectJoin '($)
  [ selectContainerComment ident
  , joinOn "comment.container = container.id"
    $ selectContainer ident
  ]

makeCommentRow :: Id Comment -> Id Container -> Segment -> Id Party -> Timestamp -> T.Text -> CommentRow
makeCommentRow i c s w t x = CommentRow i w (SlotId c s) t x

selectCommentRow :: Selector -- ^ @'CommentRow'@
selectCommentRow = selectColumns 'makeCommentRow "comment" ["id", "container", "segment", "who", "time", "text"]

_commentSets :: String -- ^ @'Comment'@
  -> [(String, String)]
_commentSets o =
  [ ("who", "${partyId $ accountParty $ commentWho " ++ o ++ "}")
  , ("container", "${containerId $ slotContainer $ commentSlot " ++ o ++ "}")
  , ("segment", "${slotSegment $ commentSlot " ++ o ++ "}")
  , ("text", "${commentText " ++ o ++ "}")
  , ("parent", "${listToMaybe $ commentParents " ++ o ++ "}")
  ]
