{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Mail
  ( sendMail
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>), mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Mail.Mime

import Databrary.Model.Party

baseMail :: Mail
baseMail = emptyMail (Address (Just "Databrary") "help@databrary.org")

mailHeader :: Either T.Text Account -> BSL.ByteString
mailHeader (Left _) = mempty
mailHeader (Right a) = BSL.fromChunks ["Dear ", TE.encodeUtf8 (partyName (accountParty a)), ",\n\n"]

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

sendMail :: MonadIO m => [Either T.Text Account] -> T.Text -> BSL.ByteString -> m ()
sendMail to@(who:_) subj body =
  liftIO $ renderSendMail $ addPart [Part "text/plain; charset=utf-8" None Nothing [] (mailHeader who <> body <> mailFooter)] $ baseMail
    { mailTo = map addr to
    , mailHeaders = [("Subject", subj)]
    }
  where
  addr (Left e) = Address Nothing e
  addr (Right Account{ accountEmail = email, accountParty = p }) =
    Address (Just (partyName p)) email

