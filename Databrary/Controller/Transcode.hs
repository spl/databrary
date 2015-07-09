{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Transcode
  ( remoteTranscode
  , viewTranscodes
  ) where

import Control.Applicative (optional)
import Control.Monad (liftM3)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.Char (isHexDigit, digitToInt)
import Data.Word (Word8)

import Databrary.Ops
import Databrary.Has (peeks)
import Databrary.Service.Crypto
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Action.Auth
import Databrary.Model.Id
import Databrary.Model.Transcode
import Databrary.Store.Transcode
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.View.Transcode

unHex :: String -> Maybe [Word8]
unHex [] = Just []
unHex [_] = Nothing
unHex (h:l:r) = do
  hb <- unhex h
  lb <- unhex l
  ((shiftL hb 4 .|. lb) :) <$> unHex r
  where unhex x = isHexDigit x ?> fromIntegral (digitToInt x)

sha1Form :: (Functor m, Monad m) => DeformT f m BS.ByteString
sha1Form = do
  b <- deform
  deformGuard "Invalid SHA1 hex string" (length b == 40)
  maybe (deformError "Invalid hex string" >> return BS.empty) (return . BS.pack) $ unHex b

remoteTranscode :: AppRoute (Id Transcode)
remoteTranscode = action POST (pathJSON >/> pathId) $ \ti -> do
  t <- maybeAction =<< lookupTranscode ti
  withReAuth (transcodeOwner t) $ do
    auth <- peeks $ transcodeAuth t
    (res, sha1, logs) <- runForm Nothing $ do
      _ <- "auth" .:> (deformCheck "Invalid authentication" (constEqBytes auth) =<< deform)
      _ <- "pid" .:> (deformCheck "PID mismatch" (transcodeProcess t ==) =<< deformNonEmpty deform)
      liftM3 (,,)
        ("res" .:> deform)
        ("sha1" .:> optional sha1Form)
        ("log" .:> deform)
    collectTranscode t res sha1 logs
    okResponse [] BS.empty

viewTranscodes :: AppRoute ()
viewTranscodes = action GET (pathHTML >/> "transcode") $ \() -> withAuth $ do
  checkMemberADMIN
  t <- lookupActiveTranscodes
  okResponse [] =<< peeks (htmlTranscodes t)
