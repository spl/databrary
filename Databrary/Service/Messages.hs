{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Service.Messages
  ( Messages
  , initMessages
  , getMessage
  , listMessages
  ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Short as BSS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

#if !MIN_VERSION_hashable(1,2,3)
import Data.Hashable (Hashable(..))
instance Hashable BSS.ShortByteString where
  hashWithSalt i = hashWithSalt i . BSS.fromShort
  hash = hash . BSS.fromShort
#endif

newtype Messages = Messages { messagesMap :: HM.HashMap BSS.ShortByteString T.Text }

initMessages :: C.Config -> IO Messages
initMessages c = Messages .
  HM.fromList . mapMaybe f . HM.toList <$> C.getMap c where
  f (k, C.String v) | Just m <- T.stripPrefix "message." k = Just (BSS.toShort $ TE.encodeUtf8 m, v)
  f _ = Nothing

getMessage :: BSS.ShortByteString -> Messages -> T.Text
getMessage m = HM.lookupDefault ("[" <> TE.decodeLatin1 (BSS.fromShort m) <> "]") m . messagesMap

listMessages :: Messages -> [(BSS.ShortByteString, T.Text)]
listMessages = HM.toList . messagesMap
