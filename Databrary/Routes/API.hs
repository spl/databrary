{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Routes.API
  ( swagger
  ) where

import Control.Applicative ((<$>))
import Data.Aeson.Types
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (maybeToList, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)
import qualified Network.HTTP.Types as HTTP

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
import Databrary.Action.Route
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

infixl 4 .++, .+
class HasFields o where
  (.++) :: o -> [Pair] -> o
  (.++) = foldl' (.+)
  (.+) :: o -> Pair -> o
  o .+ l = o .++ [l]

instance HasFields [Pair] where
  (.++) = (++)

instance HasFields Object where
  (.+) = flip $ uncurry HM.insert

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

typeInt32, typeString, typePassword, typeEmail, typeURL :: DataType
typeInt32 = DataType TypeInteger (Just "int32")
typeString = DataType TypeString Nothing
typePassword = DataType TypeString (Just "password")
typeEmail = DataType TypeString (Just "email")
typeURL = DataType TypeString (Just "url")

dataTypeJSON :: DataType -> [Pair]
dataTypeJSON (DataType t f) = ("type" .= t) : maybeToList (("format" .=) <$> f)

val :: DataType -> T.Text -> Object
val t d = HM.fromList $ ("description" .= d) : dataTypeJSON t

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

instance HasFields Parameter where
  p .++ l = p{ parameterFields = parameterFields p .++ l }

instance ToJSON Parameter where
  toJSON Parameter{..} = object $
    [ "name" .= parameterName
    , "in" .= parameterLoc
    ] ++ dataTypeJSON parameterType
    ++ parameterFields

pathParameter :: T.Text -> DataType -> T.Text -> Parameter
pathParameter name dt desc = Parameter InPath name dt
  [ "required" .= True
  , "description" .= desc
  ]

queryParameter :: T.Text -> DataType -> Bool -> T.Text -> Parameter
queryParameter name dt empty desc = Parameter InQuery name dt
  [ "description" .= desc
  , "allowEmptyValue" .= empty
  ]

formParameter :: T.Text -> DataType -> Bool -> T.Text -> Parameter
formParameter name dt req desc = Parameter InData name dt 
  [ "required" .= req
  , "description" .= desc
  ]

pathParameters :: [Parameter] -> [T.Text]
pathParameters = mapMaybe pp where
  pp Parameter{ parameterLoc = InPath, parameterName = n } = Just n
  pp _ = Nothing

data Response = Response
  { _responseStatus :: Maybe HTTP.Status
  , responseFields :: [Pair]
  }

instance HasFields Response where
  r .++ l = r{ responseFields = responseFields r .++ l }

responseJSON :: Response -> Pair
responseJSON (Response s p) = (maybe "default" (T.pack . show . HTTP.statusCode) s, object p)

okResp :: T.Text -> Value -> Response
okResp desc schema = Response (Just HTTP.ok200)
  [ "description" .= desc
  , "schema" .= schema
  ]

op :: T.Text -> Route r a -> a -> T.Text -> T.Text -> [Parameter] -> [Response] -> (T.Text, Object)
op i rte arg summary desc param resp =
  ( swaggerPath (routePath rte) arg $ pathParameters param
  , HM.singleton (T.toLower (TE.decodeLatin1 (routeMethod rte))) $ object
    [ "operationId" .= i
    , "summary" .= summary
    , "description" .= desc
    , "parameters" .= param
    , "responses" .= object (map responseJSON resp)
    ]
  )

instance HasFields (T.Text, Object) where
  (t, o) .++ l = (t, o .++ l)
  (t, o) .+ l = (t, o .+ l)

ref, def :: T.Text -> Object
ref r = HM.singleton "$ref" (String ("#/" <> r))
def = ref . ("definitions/" <>)

enum :: forall a . (DBEnum a, ToJSON a) => a -> Pair
enum _ = "enum" .= (map (toJSON . fst) v ++ map (toJSON . snd) v) where
  v :: [(a, String)]
  v = pgEnumValues

readOnly :: HasFields o => o -> o
readOnly = (.+ "readOnly" .= True)

swagger :: Value
swagger = object
  [ "swagger" .= String "2.0"
  , "info" .= object
    [ "title" .= String "Databrary"
    , "version" .= showVersion version
    , "description" .= String "All POST operations accepting formData parameters equivalently accept a JSON object body with corresponding keys, where '.' in field names are replaced by nested objects."
    ]
  , "schemes" .= [String "https"]
  , "consumes" .= map String ["application/json", "application/x-www-form-urlencoded", "multipart/form-data"]
  , "produces" .= [String "application/json"]
  , "definitions" .= object
    [ "Permission" .= object
      [ "description" .= String "Permission level as name or corresponding integer value (default in responses)"
      , enum PermissionNONE
      ]
    , "Identity" .= object
      [ "description" .= String "A party associated with a session"
      , "properties" .= object
        [ -- party properties,
          "csverf" .= val typeString "Authorization token, which must be included verbatim in all POST requests"
        ]
      ]
    , "Party" .= object
      [ "description" .= String "An individual user, group, organization, or other entity"
      , "properties" .= object
        [ "id" .= val typeInt32 "Party ID"
        , "sortname" .= val typeString "Last name or primary sortable name"
        , "prename" .= val typeString "First name or any part of name that comes before 'sortname'"
        , "name" .= readOnly (val typeString "Full display name, calculated from concatenating 'prename' and 'sortname'")
        , "orcid" .= (val typeString "[ORCID iD](http://en.wikipedia.org/wiki/ORCID)"
          .+ "pattern" .= String "^[0-9]{4}-?[0-9]{4}-?[0-9]{4}-?[0-9]{3}[0-9X]$")
        , "affiliation" .= val typeString "User-supplied university or organizational affiliation for display purposes"
        , "url" .= val typeURL "User-supplied external web site"
        , "email" .= val typeEmail "Email address"
        , "permission" .= readOnly (def "Permission" .+ "description" .= String "Level of access permission the current user has over this party") -- doesn't work

        , "authorization" .= readOnly (def "Permission" .+ "description" .= String "Site authorization level this party has been granted") -- doesn't work
        ]
      , "required" .= map String ["id", "sortname"]
      ]
    ]
  , "paths" .= HM.fromListWith HM.union
    [ op "get" viewRoot (JSON)
        "No-op"
        "Do nothing beyond standard request processing. Can be used to ensure endpoint is active."
        []
        [ okResp "An empty object" (object ["type" .= String "object"]) ]
    , op "getUser" viewUser ()
        "Current Account"
        "Return the identity of the currently logged-in user from the session cookie."
        []
        [ okResp "The current user" (Object $ def "Identity") ]
    , op "postUser" postUser (JSON)
        "Change Account"
        "Change the account information of the current user."
        [ formParameter "auth" typePassword True "Current (old) password"
        , formParameter "email" typeEmail False "New email address for user"
        , formParameter "password.once" typePassword False "New password for user"
        , formParameter "password.again" typePassword False "New password for user (must match password.once)"
        ]
        [ okResp "The (updated) current party" (Object $ def "Party") ]
    , op "postLogin" postLogin (JSON)
        "Login"
        "Request and issue a new session given a set of credentials."
        [ formParameter "email" typeEmail True "Email address of account to login"
        , formParameter "password" typePassword True "Password of account to login"
        ]
        [ okResp "The authenticated current user" (Object $ def "Identity")
          .+ "headers" .= object [ "set-cookie" .= val typeString "Session cookie" ]
        ]
    , op "postLogout" postLogout (JSON)
        "Logout"
        "Terminate and invalidate the current session."
        []
        [ okResp "The now anonymous identity" (Object $ def "Identity") ]

    , op "getParty" viewParty (JSON, TargetParty aparty)
        "Lookup Party"
        "Lookup information about a specific party."
        [ pparty
        , queryParameter "parents" typeString True "Include 'parents' in the response, optionally including 'authorization' in each parent"
          .+ "enum" .= [String "authorization"]
        , queryParameter "children" typeString True "Include 'children' in the response"
        , queryParameter "volumes" typeString True "Include 'volumes' in the response, optionally including 'access' in each volume"
          .+ "enum" .= [String "access"]
        , queryParameter "access" typeString True  "Include 'access' in the response for all volumes with at least the given permission level"
          .++ [enum PermissionNONE, "default" .= String "EDIT"]
        , queryParameter "authorization" typeString True  "Include 'authorization' in the response"
        ]
        [ okResp "The requested party" (Object $ def "Party") ]
    ]
  ] where
  -- token = Id ""
  aparty = Id 0
  pparty = pathParameter "party" typeInt32 "Party ID"
  -- volume = Id 0
  -- slot = Id (SlotId (Id 0) emptySegment)
  -- container = containerSlotId (Id 0)
  -- asset = Id 0
  -- record = Id 0
  -- metric = Id 0
  -- funder = Id 0
  -- tag = TagName ""
