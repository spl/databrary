{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Service.Messages
  ( Messages
  , messagesFile
  , loadMessagesFrom
  , loadMessages
  , getMessage
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Paths_databrary (getDataFileName)
import qualified Databrary.Store.Config as C
import qualified Databrary.JSON as JSON

newtype Messages = Messages (HM.HashMap BS.ByteString T.Text)

confMessages :: C.Config -> [([BS.ByteString], T.Text)]
confMessages = HM.foldrWithKey (\k v -> (map (first (k :)) (val v) ++)) [] where
  val v
    | Just t <- C.config v = [([], t)]
    | Just c <- C.config v = confMessages c
    | otherwise = []

messagesFile :: IO FilePath
messagesFile = getDataFileName "messages.conf"

loadMessagesFrom :: FilePath -> IO Messages
loadMessagesFrom f = Messages . HM.fromList . map (first (C.pathKey . C.Path)) . confMessages <$> C.load f

loadMessages :: IO Messages
loadMessages = loadMessagesFrom =<< messagesFile

getMessage :: C.Path -> Messages -> T.Text
getMessage p (Messages m) = HM.lookupDefault ("[" <> TE.decodeLatin1 k <> "]") k m where k = C.pathKey p

instance JSON.ToJSON Messages where
  toJSON (Messages m) = JSON.Object $ JSON.object $ map (TE.decodeUtf8 *** JSON.String) $ HM.toList m
