{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Comment
  ( postComment
  ) where

import Data.Maybe (maybeToList)

import Databrary.Ops
import Databrary.Has (view)
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Slot
import Databrary.Model.Comment
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Slot
import Databrary.View.Comment

postComment :: AppRoute (API, Id Slot)
postComment = action POST (pathAPI </> pathSlotId </< "comment") $ \(api, si) -> withAuth $ do
  u <- authAccount
  s <- getSlot PermissionSHARED Nothing si
  c <- runForm (api == HTML ?> htmlCommentForm s) $ do
    csrfForm
    text <- "text" .:> (deformRequired =<< deform)
    parent <- "parent" .:> deformNonEmpty deform
    return (blankComment u s)
      { commentText = text
      , commentParents = maybeToList parent
      }
  c' <- addComment c
  case api of
    JSON -> okResponse [] $ commentJSON c'
    HTML -> otherRouteResponse [] viewSlot (api, (Just (view c'), slotId (commentSlot c')))
