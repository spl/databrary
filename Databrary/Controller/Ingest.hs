{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Ingest
  ( viewIngest
  , postIngest
  ) where

import Control.Applicative ((<*>))
import Control.Arrow (right)
import System.Posix.FilePath (takeExtension)
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (FileInfo(..))

import Databrary.Ops
import Databrary.Has (focusIO)
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Ingest.Service
import Databrary.Ingest.JSON
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.View.Ingest

viewIngest :: AppRoute (Id Volume)
viewIngest = action GET (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  blankForm $ htmlIngestForm v s

postIngest :: AppRoute (Id Volume)
postIngest = multipartAction $ action POST (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  a <- runFormFiles [("json", 16*1024*1024)] (Just $ htmlIngestForm v s) $ do
    csrfForm
    abort <- "abort" .:> deform
    abort ?!$> (,,)
      <$> ("run" .:> deform)
      <*> ("overwrite" .:> deform)
      <*> ("json" .:>
        (deformCheck "Must be JSON." (\f ->
          fileContentType f `elem` ["text/json", "application/json"] || takeExtension (fileName f) == ".json")
        =<< deform))
  r <- maybe
    (True <$ focusIO abortIngest)
    (\(r,o,j) -> runIngest $ right (map (unId . containerId)) <$> ingestJSON v (fileContent j) r o)
    a
  guardAction r $ returnResponse badRequest400 [] ("failed" :: String)
  redirectRouteResponse [] viewIngest (volumeId v) []
