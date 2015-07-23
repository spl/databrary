{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Html
  ( lazyByteStringHtml
  , byteStringHtml
  , builderHtml
  , lazyByteStringValue
  , byteStringValue
  , builderValue
  , actionLink
  , actionForm
  , (!?)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Network.HTTP.Types (QueryLike(..), renderQueryBuilder)
import qualified Text.Blaze.Internal as Markup
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Blaze.ByteString.Builder.Html.Word (fromHtmlEscapedByteString, fromHtmlEscapedLazyByteString)
import Databrary.Action
import Databrary.HTTP.Route

import {-# SOURCE #-} Databrary.Controller.Angular

lazyByteStringHtml :: BSL.ByteString -> H.Markup
lazyByteStringHtml = H.unsafeLazyByteString . BSB.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringHtml :: BS.ByteString -> H.Markup
byteStringHtml = H.unsafeLazyByteString . BSB.toLazyByteString . fromHtmlEscapedByteString

builderHtml :: BSB.Builder -> H.Markup
builderHtml = lazyByteStringHtml . BSB.toLazyByteString

lazyByteStringValue :: BSL.ByteString -> H.AttributeValue
lazyByteStringValue = H.unsafeLazyByteStringValue . BSB.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringValue :: BS.ByteString -> H.AttributeValue
byteStringValue = H.unsafeLazyByteStringValue . BSB.toLazyByteString . fromHtmlEscapedByteString

builderValue :: BSB.Builder -> H.AttributeValue
builderValue = lazyByteStringValue . BSB.toLazyByteString

actionLink :: QueryLike q => Route r a -> a -> q -> H.Attribute
actionLink r a q = HA.href $ builderValue $ actionURL Nothing r a $ toQuery q

actionForm :: Route r a -> a -> JSOpt -> H.Html -> H.Html
actionForm r@Route{ routeMethod = g, routeMultipart = p } a j = H.form
  H.! HA.method (H.unsafeByteStringValue g)
  H.!? (p, HA.enctype "multipart/form-data")
  H.! HA.action (builderValue $ routeURL Nothing r a <> renderQueryBuilder True (toQuery j))

(!?) :: Markup.Attributable h => h -> Maybe H.Attribute -> h
h !? Nothing = h
h !? (Just a) = h H.! a
