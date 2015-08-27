{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Mail
  ( sendMail
  ) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Char (isSpace)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Mail.Mime

import Databrary.Model.Party

wrapText :: Int -> BSL.ByteString -> BSL.ByteString
wrapText n s
  | BS.length sn <= n = s
  | Just ni <- fromIntegral <$> BSC.elemIndexEnd '\n' sn = ((. wrapText n) . BSL.append) `uncurry` BSL.splitAt (succ ni) s
  | Just si <- fromIntegral <$> BSC.elemIndexEnd ' ' sn <|> BSLC.findIndex isSpace s =
    ((. (wrapText n . BSL.tail)) . BSL.append . (`BSLC.snoc` '\n')) `uncurry` BSL.splitAt si s
  | otherwise = s
  where sn = BSL.toStrict $ BSL.take (succ (fromIntegral n)) s

wrapMailText :: BSL.ByteString -> BSL.ByteString
wrapMailText = wrapText 78

baseMail :: Mail
baseMail = emptyMail (Address (Just "Databrary") "help@databrary.org")

mailHeader :: BSL.ByteString
mailHeader = mempty

mailFooter :: BSL.ByteString 
mailFooter = "\n\
  \Sincerely,\n\
  \The Databrary Team\n\
  \-- \n\
  \Databrary\n\
  \196 Mercer Street, Suite 807\n\
  \212-998-5536\n\
  \contact@databrary.org\n\
  \databrary.org\n"

sendMail :: MonadIO m => [Either BS.ByteString Account] -> T.Text -> BSL.ByteString -> m ()
sendMail to subj body =
  liftIO $ renderSendMail $ addPart [Part "text/plain; charset=utf-8" None Nothing [] (mailHeader <> wrapMailText body <> mailFooter)] $ baseMail
    { mailTo = map addr to
    , mailHeaders = [("Subject", subj)]
    }
  where
  addr (Left e) = Address Nothing (TE.decodeLatin1 e)
  addr (Right Account{ accountEmail = email, accountParty = p }) =
    Address (Just (partyName p)) (TE.decodeLatin1 email)

