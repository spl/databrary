{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Register
  ( viewPasswordReset
  , postPasswordReset
  , viewRegister
  , postRegister
  , resendInvestigator
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>), mempty)
import qualified Data.Text as T

import Databrary.Ops
import Databrary.Has (view, peek, focusIO)
import Databrary.Action
import Databrary.Action.Auth
import Databrary.Service.Mail
import Databrary.Static.Fillin
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Token
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Permission
import Databrary.Controller.Party
import Databrary.Controller.Token
import Databrary.Controller.Angular
import Databrary.View.Register

resetPasswordMail :: Either T.Text SiteAuth -> T.Text -> (Maybe BSL.ByteString -> BSL.ByteString) -> AuthActionM ()
resetPasswordMail (Left email) subj body =
  sendMail [Left email] subj (body Nothing)
resetPasswordMail (Right auth) subj body = do
  tok <- loginTokenId =<< createLoginToken auth True
  req <- peek
  sendMail [Right $ view auth] subj
    (body $ Just $ BSB.toLazyByteString $ actionURL (Just req) viewLoginToken (HTML, tok) [])

viewRegister :: AppRoute ()
viewRegister = action GET (pathHTML </< "user" </< "register") $ \() -> withAuth $ do
  angular
  maybeIdentity
    (blankForm htmlRegister)
    (\_ -> redirectRouteResponse [] viewParty (HTML, TargetProfile) [])

postRegister :: AppRoute API
postRegister = action POST (pathAPI </< "user" </< "register") $ \api -> withoutAuth $ do
  reg <- runForm (api == HTML ?> htmlRegister) $ do
    name <- "sortname" .:> (deformRequired =<< deform)
    prename <- "prename" .:> deformNonEmpty deform
    email <- "email" .:> emailTextForm
    affiliation <- "affiliation" .:> deformNonEmpty deform
    _ <- "agreement" .:> (deformCheck "You must consent to the user agreement." id =<< deform)
    let p = blankParty
          { partySortName = name
          , partyPreName = prename
          , partyAffiliation = affiliation
          , partyAccount = Just a
          }
        a = Account
          { accountParty = p
          , accountEmail = email
          }
    return a
  auth <- maybe (SiteAuth <$> addAccount reg <$- Nothing <$- mempty) return =<< lookupSiteAuthByEmail (accountEmail reg)
  resetPasswordMail (Right auth) 
    "Databrary account created"
    $ \(Just url) ->
      "Thank you for registering with Databrary. Please use this link to complete your\n\
      \registration:\n\n"
      <> url <> "\n\n\
      \By clicking the above link, you also indicate that you have read and understand\n\
      \the Databrary Access agreement, which you can download here:\n\n\
      \http://databrary.org/policies/agreement.pdf\n\n\
      \Once you've validated your e-mail, you will be able to request authorization in\n\
      \order to be granted full access to Databrary.\n"
  date <- peek
  focusIO $ staticSendInvestigator (view auth) date
  okResponse [] $ "Your confirmation email has been sent to '" <> accountEmail reg <> "'."

resendInvestigator :: AppRoute (Id Party)
resendInvestigator = action POST (pathHTML >/> pathId </< "investigator") $ \i -> withAuth $ do
  checkMemberADMIN
  p <- getParty (Just PermissionREAD) (TargetParty i)
  date <- peek
  focusIO $ staticSendInvestigator p date
  okResponse [] ("sent" :: String)

viewPasswordReset :: AppRoute ()
viewPasswordReset = action GET (pathHTML </< "user" </< "password") $ \() -> withoutAuth $ do
  angular
  blankForm htmlPasswordReset

postPasswordReset :: AppRoute API
postPasswordReset = action POST (pathAPI </< "user" </< "password") $ \api -> withoutAuth $ do
  email <- runForm (api == HTML ?> htmlPasswordReset) $ do
    "email" .:> emailTextForm
  auth <- lookupPasswordResetAccount email
  resetPasswordMail (maybe (Left email) Right auth)
    "Databrary password reset" $
    ("Someone (hopefully you) has requested to reset the password for the Databrary\n\
    \account associated with this email address. If you did not request this, let us\n\
    \know (by replying to this message) or simply ignore it.\n\n" <>)
    . maybe
      "Unfortunately, no Databrary account was found for this email address. You can\n\
      \try again with a different email address, or reply to this email for assistance.\n"
      ("Otherwise, you may use this link to reset your Databrary password:\n\n" <>)
  okResponse [] $ "Your password reset information has been sent to '" <> email <> "'."

