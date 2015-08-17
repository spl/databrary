{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Tag
  ( queryTags
  , postTag
  , deleteTag
  , viewTopTags
  ) where

import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), conflict409)

import Databrary.Has (view)
import Databrary.JSON (toJSON)
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Slot
import Databrary.Model.Tag
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Slot

_tagNameForm :: (Functor m, Monad m) => DeformT f m TagName
_tagNameForm = deformMaybe' "Invalid tag name." . validateTag =<< deform

queryTags :: AppRoute TagName
queryTags = action GET (pathJSON >/> "tags" >/> PathParameter) $ \t ->
  okResponse [] . toJSON . map tagName =<< findTags t

tagResponse :: API -> TagUse -> AuthAction
tagResponse JSON t = okResponse [] . tagCoverageJSON =<< lookupTagCoverage (useTag t) (containerSlot $ slotContainer $ tagSlot t)
tagResponse HTML t = otherRouteResponse [] viewSlot (HTML, (Just (view t), slotId (tagSlot t)))

postTag :: AppRoute (API, Id Slot, TagId)
postTag = action POST (pathAPI </>> pathSlotId </> pathTagId) $ \(api, si, TagId kw tn) -> withAuth $ do
  guardVerfHeader
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) Nothing si
  t <- addTag tn
  let tu = TagUse t kw u s
  r <- addTagUse tu
  guardAction r $ 
    returnResponse conflict409 [] ("The requested tag overlaps your existing tag." :: T.Text)
  tagResponse api tu

deleteTag :: AppRoute (API, Id Slot, TagId)
deleteTag = action DELETE (pathAPI </>> pathSlotId </> pathTagId) $ \(api, si, TagId kw tn) -> withAuth $ do
  guardVerfHeader
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) Nothing si
  t <- maybeAction =<< lookupTag tn
  let tu = TagUse t kw u s
  _r <- removeTagUse tu
  tagResponse api tu

viewTopTags :: AppRoute ()
viewTopTags = action GET (pathJSON >/> "tags") $ \() -> do
  l <- lookupTopTagWeight 16
  okResponse [] $ toJSON $ map tagWeightJSON l
