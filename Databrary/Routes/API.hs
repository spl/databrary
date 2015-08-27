{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Routes.API
  ( swagger
  ) where

import Control.Applicative ((<$>))
import Data.Aeson.Types
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (maybeToList, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)

import Paths_databrary (version)
import Databrary.HTTP.Route
import Databrary.HTTP.Path.Swagger
import Databrary.Model.Id.Types
{-
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
-}
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Root
import Databrary.Controller.Login
import Databrary.Controller.Party
{-
import Databrary.Controller.Register
import Databrary.Controller.Token
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
-}

data Type
  = TypeBoolean
  | TypeInteger
  | TypeNumber
  | TypeString
  -- | TypeArray ...
  | TypeFile
  deriving (Eq, Enum, Bounded, Show)

instance ToJSON Type where
  toJSON = String . T.pack . n . show where
    n ('T':'y':'p':'e':c:r) = toLower c : r 
    n s = error ("toJSON Type: " ++ s)

data DataType = DataType
  { _dataType :: Type
  , _dataFormat :: Maybe T.Text
  }

dataTypeInt32 :: DataType
dataTypeInt32 = DataType TypeInteger (Just "int32")

dataTypeJSON :: DataType -> [Pair]
dataTypeJSON (DataType t f) = ("type" .= t) : maybeToList (("format" .=) <$> f)

data Parameter
  = PathParameter
    { parameterName :: T.Text
    , parameterType :: DataType
    , parameterDescription :: T.Text
    }

instance ToJSON Parameter where
  toJSON PathParameter{..} = object $
    [ "name" .= parameterName
    , "in" .= String "path"
    , "description" .= parameterDescription
    , "required" .= True
    ] ++ dataTypeJSON parameterType

pathParameters :: [Parameter] -> [T.Text]
pathParameters = mapMaybe pp where
  pp PathParameter{ parameterName = n } = Just n
  pp _ = Nothing

op :: T.Text -> Route r a -> a -> T.Text -> T.Text -> [Parameter] -> (T.Text, Object)
op i r a summary desc p =
  ( swaggerPath (routePath r) a $ pathParameters p
  , HM.singleton (T.toLower (TE.decodeLatin1 (routeMethod r))) $ object
    [ "operationId" .= i
    , "summary" .= summary
    , "description" .= desc
    , "parameters" .= p
    ]
  )

swagger :: Value
swagger = object
  [ "swagger" .= String "2.0"
  , "info" .=
    [ "title" .= String "Databrary"
    , "version" .= showVersion version
    ]
  , "schemes" .= [String "https"]
  , "produces" .= [String "application/json"]
  , "paths" .= HM.fromListWith HM.union
    [ op "get" viewRoot (JSON)
        "No-op"
        "Do nothing beyond standard request processing. Can be used to ensure endpoint is active."
        []
    , op "getUser" viewUser ()
        "Current Account"
        "Return the identity of the currently logged-in user from the session cookie."
        []
    , op "postUser" postUser (JSON)
        "Change Account"
        "Change the account information of the current user."
        []
    , op "postLogin" postLogin (JSON)
        "Login"
        "Request a new session given a set of credentials."
        []
    , op "postLogout" postLogout (JSON)
        "Logout"
        "Terminate the current session."
        []

    , op "getParty" viewParty (JSON, TargetParty aparty)
        "Lookup Party"
        "Lookup information about a specific party."
        [pparty]
    ]
  ] where
  -- token = Id ""
  aparty = Id 0
  pparty = PathParameter "party" dataTypeInt32 "Party ID"
  -- volume = Id 0
  -- slot = Id (SlotId (Id 0) emptySegment)
  -- container = containerSlotId (Id 0)
  -- asset = Id 0
  -- record = Id 0
  -- metric = Id 0
  -- funder = Id 0
  -- tag = TagName ""
