{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.URL
  ( URI
  , validHDL
  , hdlURL
  , parseURL
  ) where

import Control.Monad ((<=<))
import Data.Aeson (ToJSON(..))
import qualified Data.Configurator as C ()
import qualified Data.Configurator.Types as C
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Language.Haskell.TH.Lift (deriveLiftMany)
import qualified Text.Blaze as H

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Network.URI

toPG :: URI -> String
toPG u = uriToString id u ""

fromPG :: String -> URI
fromPG u = fromMaybe (error $ "pgDecode URI: " ++ u) $ parseURI u

instance PGParameter "text" URI where
  pgEncode t = pgEncode t . toPG
  pgEncodeValue e t = pgEncodeValue e t . toPG
  pgLiteral t = pgLiteral t . toPG
instance PGColumn "text" URI where
  pgDecode t = fromPG . pgDecode t
  pgDecodeValue e t = fromPG . pgDecodeValue e t

instance ToJSON URI where
  toJSON = toJSON . show

instance C.Configured URI where
  convert = parseAbsoluteURI . T.unpack <=< C.convert

instance H.ToValue URI where
  toValue = H.stringValue . show
  preEscapedToValue = H.preEscapedStringValue . show

deriveLiftMany [''URIAuth, ''URI]

validHDL :: String -> Bool
validHDL = v0 (0 :: Int) where
  v0 n (c:s) | isDigit c = v1 n s
  v0 _ _ = False
  v1 n ('/':_) = n > 0
  v1 n ('.':s) = v0 (succ n) s
  v1 n s = v0 n s

hdlURL :: String -> URI
hdlURL doi = URI "hdl:" Nothing doi "" ""

parseURL :: String -> Maybe URI
parseURL d@('1':'0':'.':c:_) | isDigit c = parseURL $ "doi:" ++ d
parseURL s = do
  u <- parseURI s
  if uriScheme u `elem` ["doi:","hdl:"] && isNothing (uriAuthority u) ||
     uriScheme u == "http:" && uriAuthority u == Just (URIAuth "" "dx.doi.org" "")
    then do
      guard $ validHDL $ uriPath u
      return u
        { uriScheme = "hdl:"
        , uriAuthority = Nothing
        }
    else do
      guard $ uriScheme u `elem` ["http:","https:"]
      return u
