{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Ingest
  ( viewIngest
  , postIngest
  ) where

import qualified Data.ByteString as BS
import System.Posix.FilePath (takeExtension)
import Network.Wai.Parse (FileInfo(..))

import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
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
  v <- getVolume PermissionEDIT vi
  blankForm $ htmlIngestForm v

postIngest :: AppRoute (Id Volume)
postIngest = multipartAction $ action POST (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  v <- getVolume PermissionEDIT vi
  (j, r, o) <- runFormFiles [("json", 16*1024*1024)] (Just $ htmlIngestForm v) $ do
    csrfForm
    run <- "run" .:> deform
    overwrite <- "overwrite" .:> deform
    json <- "json" .:>
      (deformCheck "Must be JSON." (\f ->
        fileContentType f `elem` ["text/json", "application/json"] || takeExtension (fileName f) == ".json")
      =<< deform)
    return (json, run, overwrite)
  l <- if r
    then ingestJSON v (fileContent j) o
    else return []
  okResponse [] BS.empty
