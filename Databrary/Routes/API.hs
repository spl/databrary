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
import Databrary.Model.Enum
import Databrary.Model.Permission
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

dataTypeInt32, dataTypeString, dataTypePassword, dataTypeEmail :: DataType
dataTypeInt32 = DataType TypeInteger (Just "int32")
dataTypeString = DataType TypeString Nothing
dataTypePassword = DataType TypeString (Just "password")
dataTypeEmail = DataType TypeString (Just "email")

dataTypeJSON :: DataType -> [Pair]
dataTypeJSON (DataType t f) = ("type" .= t) : maybeToList (("format" .=) <$> f)

data ParameterLoc
  = InPath
  | InQuery
  | InData

instance ToJSON ParameterLoc where
  toJSON InPath = String "path"
  toJSON InQuery = String "query"
  toJSON InData = String "formData" -- also "body", sort of

data Parameter = Parameter
  { parameterLoc :: ParameterLoc
  , parameterName :: T.Text
  , parameterType :: DataType
  , parameterFields :: [Pair]
  }

instance ToJSON Parameter where
  toJSON Parameter{..} = object $
    [ "name" .= parameterName
    , "in" .= parameterLoc
    ] ++ dataTypeJSON parameterType
    ++ parameterFields

pathParameter :: T.Text -> DataType -> T.Text -> [Pair] -> Parameter
pathParameter name dt desc =
  Parameter InPath name dt . ("required" .= True :) . ("description" .= desc :)

queryParameter :: T.Text -> DataType -> Bool -> T.Text -> [Pair] -> Parameter
queryParameter name dt empty desc =
  Parameter InQuery name dt . ("description" .= desc :) . ("allowEmptyValue" .= empty :)

formParameter :: T.Text -> DataType -> Bool -> T.Text -> [Pair] -> Parameter
formParameter name dt req desc =
  Parameter InData name dt . ("required" .= req :) . ("description" .= desc :)

pathParameters :: [Parameter] -> [T.Text]
pathParameters = mapMaybe pp where
  pp Parameter{ parameterLoc = InPath, parameterName = n } = Just n
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
  , "info" .= object
    [ "title" .= String "Databrary"
    , "version" .= showVersion version
    , "description" .= String "All POST operations accepting formData parameters equivalently accept a JSON object body with corresponding keys, where '.' in field names are replaced by nested objects."
    ]
  , "schemes" .= [String "https"]
  , "consumes" .= [String "application/json", String "application/x-www-form-urlencoded", String "multipart/form-data"]
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
        [ formParameter "auth" dataTypePassword True "Current (old) password" []
        , formParameter "email" dataTypeEmail False "New email address for user" []
        , formParameter "password.once" dataTypePassword False "New password for user" []
        , formParameter "password.again" dataTypePassword False "New password for user (must match password.once)" []
        ]
    , op "postLogin" postLogin (JSON)
        "Login"
        "Request and issue a new session given a set of credentials."
        [ formParameter "email" dataTypeEmail True "Email address of account to login" []
        , formParameter "password" dataTypePassword True "Password of account to login" []
        ]
    , op "postLogout" postLogout (JSON)
        "Logout"
        "Terminate and invalidate the current session."
        []

    , op "getParty" viewParty (JSON, TargetParty aparty)
        "Lookup Party"
        "Lookup information about a specific party."
        [ pparty
        , queryParameter "parents" dataTypeString True "Include 'parents' in the response, optionally including 'authorization' in each parent."
          [ "enum" .= [String "authorization"] ]
        , queryParameter "children" dataTypeString True "Include 'children' in the response."
          [ "enum" .= emptyArray ]
        , queryParameter "volumes" dataTypeString True "Include 'volumes' in the response, optionally including 'access' in each volume."
          [ "enum" .= [String "access"] ]
        , queryParameter "access" dataTypeString True  "Include 'access' in the response for all volumes with at least the given permission level."
          [ "default" .= String "EDIT"
          , "enum" .= map snd (pgEnumValues :: [(Permission, String)])
          ]
        , queryParameter "authorization" dataTypeString True  "Include 'authorization' in the response."
          [ "enum" .= emptyArray ]
        ]
    ]
  ] where
  -- token = Id ""
  aparty = Id 0
  pparty = pathParameter "party" dataTypeInt32 "Party ID" []
  -- volume = Id 0
  -- slot = Id (SlotId (Id 0) emptySegment)
  -- container = containerSlotId (Id 0)
  -- asset = Id 0
  -- record = Id 0
  -- metric = Id 0
  -- funder = Id 0
  -- tag = TagName ""
