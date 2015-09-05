{-# LANGUAGE OverloadedStrings, CPP #-}
module Databrary.Web.Constants
  ( constantsJSON
  , constantsJS
  , generateConstantsJSON
  , generateConstantsJS
  ) where

import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>))
import Data.Version (showVersion)
import System.IO (withBinaryFile, IOMode(WriteMode))

import Paths_databrary (version)
import qualified Databrary.JSON as JSON
import Databrary.Model.Enum
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Metric
import Databrary.Model.RecordCategory
import Databrary.Model.Format
import Databrary.Model.Party
import Databrary.Web.Types
import Databrary.Web.Generate

constantsJSON :: JSON.Value
constantsJSON = JSON.Object $ JSON.object
  [ "permission" JSON..= enumValues PermissionPUBLIC
  , "release" JSON..= enumValues ReleasePUBLIC
  , "metric" JSON..= JSON.recordMap (map metricJSON allMetrics)
  , "category" JSON..= JSON.recordMap (map recordCategoryJSON allRecordCategories)
  , "format" JSON..= JSON.recordMap (map formatJSON allFormats)
  , "party" JSON..= JSON.object
    [ "nobody" JSON..= partyJSON nobodyParty
    , "root" JSON..= partyJSON rootParty
    ]
  , "version" JSON..= showVersion version
#ifdef DEVEL
  , "devel" JSON..= True
#endif
#ifdef SANDBOX
  , "sandbox" JSON..= True
#endif
  -- TODO: url?
  ]
  where
  enumValues :: forall a . DBEnum a => a -> [String]
  enumValues _ = map show $ enumFromTo minBound (maxBound :: a)

constantsJSONB :: BSB.Builder
constantsJSONB = JSON.encodeToBuilder constantsJSON

constantsJS :: BSB.Builder
constantsJS = BSB.string8 "app.constant('constantData'," <> constantsJSONB <> BSB.string8 ");"

regenerateConstants :: BSB.Builder -> WebGenerator
regenerateConstants b = staticWebGenerate $ \f ->
  withBinaryFile f WriteMode $ \h ->
    BSB.hPutBuilder h b

generateConstantsJSON :: WebGenerator
generateConstantsJSON = regenerateConstants constantsJSONB

generateConstantsJS :: WebGenerator
generateConstantsJS = regenerateConstants constantsJS
