{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes.API
  ( swagger
  ) where

import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)

import Paths_databrary (version)
import Databrary.HTTP.Route
import Databrary.HTTP.Path.Swagger
import Databrary.Model.Id.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Root
import Databrary.Controller.Login
import Databrary.Controller.Register
import Databrary.Controller.Token
import Databrary.Controller.Party
import Databrary.Controller.Authorize
import Databrary.Controller.Volume
import Databrary.Controller.VolumeAccess
import Databrary.Controller.Funding
import Databrary.Controller.Container
import Databrary.Controller.Slot
import Databrary.Controller.Record
import Databrary.Controller.Citation
import Databrary.Controller.Upload
import Databrary.Controller.Format
import Databrary.Controller.Asset
import Databrary.Controller.AssetSegment
import Databrary.Controller.Excerpt
import Databrary.Controller.Zip
import Databrary.Controller.Tag
import Databrary.Controller.Comment
import Databrary.Controller.CSV
import Databrary.Controller.Audit
import Databrary.Controller.Transcode
import Databrary.Controller.Ingest
import Databrary.Controller.Web

op :: T.Text -> Route r a -> a -> [T.Text] -> (T.Text, Object)
op i r a p = (swaggerPath (routePath r) a p, HM.singleton (T.toLower (TE.decodeLatin1 (routeMethod r))) $ object
  [ "operationId" .= i
  , "summary" .= String mempty
  , "description" .= String mempty
  ])

swagger :: Value
swagger = object
  [ "swagger" .= String "2.0"
  , "info" .=
    [ "title" .= String "Databrary"
    , "version" .= showVersion version
    ]
  , "schemes" .= [String "https"]
  , "paths" .= HM.fromListWith HM.union
    [ op "get" viewRoot (JSON) []
    , op "getUser" viewUser () []
    , op "postUser" postUser (JSON) []
    , op "postLogin" postLogin (JSON) []
    , op "postLogout" postLogout (JSON) []

    , op "getParty" viewParty (JSON, TargetParty party) ["party"]
    ]
  ] where
  -- token = Id ""
  party = Id 0
  -- volume = Id 0
  -- slot = Id (SlotId (Id 0) emptySegment)
  -- container = containerSlotId (Id 0)
  -- asset = Id 0
  -- record = Id 0
  -- metric = Id 0
  -- funder = Id 0
  -- tag = TagName ""
