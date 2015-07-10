{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( checkPassword
  , loginAccount
  , viewLogin
  , postLogin
  , postLogout
  , viewUser
  , postUser
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Trans.Class (lift)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString as BS
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)

import Databrary.Ops
import Databrary.Has (view, peek)
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Token
import Databrary.HTTP.Cookie
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Action.Auth
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.View.Login

import {-# SOURCE #-} Databrary.Controller.Root
import {-# SOURCE #-} Databrary.Controller.Party

loginAccount :: API -> SiteAuth -> Bool -> AppAction
loginAccount api auth su = do
  sess <- createSession auth su
  let Token (Id tok) ex = view sess
  cook <- setSignedCookie "session" tok ex
  case api of
    JSON -> okResponse [cook] $ identityJSON (Identified sess)
    HTML -> redirectRouteResponse [cook] viewParty (HTML, TargetProfile) []

viewLogin :: AppRoute ()
viewLogin = action GET ("user" >/> "login") $ \() -> withAuth $ do
  angular
  maybeIdentity
    (blankForm htmlLogin)
    (\_ -> redirectRouteResponse [] viewParty (HTML, TargetProfile) [])

checkPassword :: BS.ByteString -> SiteAuth -> Bool
checkPassword p = Fold.any (`BCrypt.validatePassword` p) . accountPasswd

postLogin :: AppRoute API
postLogin = action POST (pathAPI </< "user" </< "login") $ \api -> withoutAuth $ do
  (Just auth, su) <- runForm (api == HTML ?> htmlLogin) $ do
    email <- "email" .:> emailTextForm
    password <- "password" .:> deform
    superuser <- "superuser" .:> deform
    auth <- lift $ lookupSiteAuthByEmail email
    let p = view <$> auth
        su = superuser && Fold.any ((PermissionADMIN ==) . accessPermission) auth
    attempts <- lift $ maybe (return 0) recentAccountLogins p
    let pass = checkPassword password `Fold.any` auth
        block = attempts > 4
    lift $ auditAccountLogin pass (fromMaybe nobodyParty p) email
    when block $ "email" .:> deformError "Too many login attempts. Try again later."
    unless pass $ "password" .:> deformError "Incorrect login"
    return (auth, su)
  withReaderT authApp $ loginAccount api auth su

postLogout :: AppRoute API
postLogout = action POST (pathAPI </< "user" </< "logout") $ \api -> withAuth $ do
  _ <- maybeIdentity (return False) removeSession
  case api of
    JSON -> okResponse [cook] $ identityJSON UnIdentified
    HTML -> redirectRouteResponse [cook] viewRoot HTML []
  where cook = clearCookie "session"

viewUser :: AppRoute ()
viewUser = action GET (pathJSON </< "user") $ \() -> withAuth $
  okResponse [] . identityJSON =<< peek

postUser :: AppRoute API
postUser = action POST (pathAPI </< "user") $ \api -> withAuth $ do
  auth <- peek
  let acct = siteAccount auth
  auth' <- runForm (api == HTML ?> htmlUserForm acct) $ do
    csrfForm
    "auth" .:> (deformGuard "Incorrect password" . (`checkPassword` auth) =<< deform)
    email <- "email" .:> deform
    passwd <- "password" .:> deformNonEmpty (passwordForm acct)
    let acct' = acct
          { accountEmail = email
          , accountParty = (accountParty acct){ partyAccount = Just acct' }
          }
    return auth
      { siteAccount = acct'
      , accountPasswd = passwd <|> accountPasswd auth
      }
  changeAccount auth'
  case api of
    JSON -> okResponse [] $ partyJSON $ accountParty $ siteAccount auth'
    HTML -> redirectRouteResponse [] viewParty (api, TargetProfile) []
