{-# LANGUAGE GADTs #-}
module Databrary.HTTP.Path.Types
  ( Path
  , PathParameter(..)
  , pathParameterAs
  , PathElement(..)
  , PathElements
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as Text
import Data.Typeable (Typeable)
import Text.Read (readMaybe)

import Databrary.Ops
import Databrary.Model.Offset
import Databrary.Model.Segment

type Path = [T.Text]

class Typeable a => PathParameter a where
  pathParameter :: T.Text -> Maybe a
  parameterPath :: a -> T.Text

pathParameterAs :: PathParameter a => Proxy a -> T.Text -> Maybe a
pathParameterAs _ = pathParameter

instance PathParameter T.Text where
  pathParameter = Just
  parameterPath = id

instance PathParameter BS.ByteString where
  pathParameter = Just . TE.encodeUtf8
  parameterPath = TE.decodeUtf8

readText :: Text.Reader a -> T.Text -> Maybe a
readText = (either (const Nothing) (\(a, t) -> T.null t ?> a) .)

instance PathParameter Integer where
  pathParameter = readText (Text.signed Text.decimal)
  parameterPath = T.pack . show

instance PathParameter Int where
  pathParameter = readText (Text.signed Text.decimal)
  parameterPath = T.pack . show

instance PathParameter Int64 where
  pathParameter = readText (Text.signed Text.decimal)
  parameterPath = T.pack . show

instance PathParameter Int32 where
  pathParameter = readText (Text.signed Text.decimal)
  parameterPath = T.pack . show

instance PathParameter Int16 where
  pathParameter = readText (Text.signed Text.decimal)
  parameterPath = T.pack . show

instance PathParameter Offset where
  pathParameter = readMaybe . T.unpack
  parameterPath = T.pack . show . offsetMillis

instance PathParameter Segment where
  pathParameter = readMaybe . T.unpack
  parameterPath s = T.pack $ showSegmentWith (shows . offsetMillis) s ""


data PathElement where
  PathElementFixed :: !T.Text -> PathElement
  PathElementParameter :: PathParameter a => a -> PathElement
  PathElementAny :: Path -> PathElement

type PathElements = [PathElement]
