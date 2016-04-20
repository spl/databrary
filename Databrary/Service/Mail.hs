{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Mail
  ( sendMail
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Network.Mail.Mime

import Databrary.Model.Party

wrapText :: Int64 -> TL.Text -> TL.Text
wrapText n s
  | TL.length sp <= n = s
  | (np,nq) <- TL.breakOnEnd "\n" sp, not (TL.null np) = np <> wrapText n (nq <> sq)
  | (bp,bq) <- TL.breakOnEnd " " sp, not (TL.null bp) = TL.init bp `TL.snoc` '\n' <> wrapText n (bq <> sq)
  | (lp,lq) <- TL.breakOn "\n" s, not (TL.null lq) = lp `TL.snoc` '\n' <> wrapText n (TL.tail lq)
  | otherwise = s
  where (sp,sq) = TL.splitAt (succ n) s

baseMail :: Mail
baseMail = emptyMail (Address (Just "Databrary") "help@databrary.org")

mailHeader :: TL.Text
mailHeader = TL.empty

mailFooter :: TL.Text
mailFooter = "\n\
  \Sincerely,\n\
  \The Databrary Team\n\
  \-- \n\
  \Databrary\n\
  \196 Mercer Street, Suite 807\n\
  \212-998-5536\n\
  \contact@databrary.org\n\
  \databrary.org\n"

sendMail :: MonadIO m => [Either BS.ByteString Account] -> [Either BS.ByteString Account] -> T.Text -> TL.Text -> m ()
sendMail to cc subj body =
  liftIO $ renderSendMail $ addPart [plainPart $ mailHeader <> wrapText 78 body <> mailFooter] $ baseMail
    { mailTo = map addr to
    , mailCc = map addr cc
    , mailHeaders = [("Subject", subj)]
    }
  where
  addr (Left e) = Address Nothing (TE.decodeLatin1 e)
  addr (Right Account{ accountEmail = email, accountParty = p }) =
    Address (Just $ partyName $ partyRow p) (TE.decodeLatin1 email)

