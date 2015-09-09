{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Authorize
  ( viewAuthorize
  , postAuthorize
  , deleteAuthorize
  , postAuthorizeNotFound
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when, liftM2, mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime(..), fromGregorian, addGregorianYearsRollOver)
import Network.HTTP.Types (noContent204, StdMethod(DELETE))

import Databrary.Ops
import Databrary.Has (peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.Mail
import Databrary.Static.Service
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Authorize
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Party
import Databrary.View.Authorize

viewAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
viewAuthorize = action GET (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \(api, i, AuthorizeTarget app oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction =<< lookupParty oi
  let (child, parent) = if app then (p, o) else (o, p)
  c <- lookupAuthorize child parent
  let c' = Authorize (Authorization mempty child parent) Nothing `fromMaybe` c
  case api of
    JSON -> return $ okResponse [] $ JSON.Object $ authorizeJSON c'
    HTML
      | app -> return $ okResponse [] ("" :: T.Text) -- TODO
      | otherwise -> peeks $ blankForm . htmlAuthorizeForm c'

partyDelegates :: Party -> ActionM [Account]
partyDelegates p =
  mapMaybe partyAccount
    . (p :)
    . map (authorizeChild . authorization)
    . filter ((PermissionADMIN <=) . accessPermission)
    <$> lookupAuthorizedChildren p False

authorizeAddr :: Static -> [Either BS.ByteString Account]
authorizeAddr = return . Left . staticAuthorizeAddr

postAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
postAuthorize = action POST (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \arg@(api, i, AuthorizeTarget app oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction . mfilter ((0 <) . unId . partyId) =<< lookupParty oi
  let (child, parent) = if app then (p, o) else (o, p)
  c <- lookupAuthorize child parent
  let c' = Authorize (Authorization mempty child parent) Nothing `fromMaybe` c
  authaddr <- peeks authorizeAddr
  a <- if app
    then do
      when (isNothing c) $ do
        changeAuthorize c'
        dl <- partyDelegates parent
        agent <- peeks $ fmap accountEmail . partyAccount
        req <- peek
        sendMail (map Right dl ++ authaddr)
          ("Databrary authorization request from " <> partyName child)
          $ BSL.fromChunks [TE.encodeUtf8 (partyName child), " <", Fold.fold agent, "> has requested to be authorized by ", TE.encodeUtf8 (partyName parent), ". \
            \To approve or reject this authorization request, go to:\n\n" ] <>
            BSB.toLazyByteString (actionURL (Just req) viewPartyEdit (TargetParty $ partyId parent) [("page", Just "grant")]) <> "#auth-" <> BSLC.pack (show $ partyId child) <> "\n\n\
            \Find more information about authorizing and managing affiliates here: \
            \http://databrary.org/access/guide/investigators/authorization/affiliates.html\n"
      return $ Just $ fromMaybe c' c
    else do
      su <- peeks identityAdmin
      now <- peek
      let maxexp = addGregorianYearsRollOver 2 $ utctDay now
          minexp = fromGregorian 2000 1 1
      a <- runForm (api == HTML ?> htmlAuthorizeForm c') $ do
        csrfForm
        delete <- "delete" .:> deform
        delete ?!$> do
          site <- "site" .:> deform
          member <- "member" .:> deform
          expires <- "expires" .:> (deformCheck "Expiration must be within two years." (Fold.all (\e -> su || e > minexp && e <= maxexp))
            =<< (<|> (su ?!> maxexp)) <$> deformNonEmpty deform)
          return $ Authorize (Authorization (Access site member) child parent) $ fmap (`UTCTime` 43210) expires
      maybe (Fold.mapM_ removeAuthorize c) changeAuthorize a
      when (Fold.any ((PermissionPUBLIC <) . accessSite) a && Fold.all ((PermissionPUBLIC >=) . accessSite) c) $
        sendMail (maybe id (:) (Right <$> partyAccount child) authaddr)
          "Databrary authorization approved"
          $ BSL.fromChunks
          [ "Dear ", TE.encodeUtf8 (partyName child), ",\n\n\
            \You have been authorized by ", TE.encodeUtf8 (partyName parent), ", allowing you to access all the shared data in Databrary. \
            \As you begin exploring, you can find illustrative videos for teaching, see how to conduct procedures, generate citations to your work, preserve data, and repurpose videos to ask new questions outside the scope of the original study.\
            \\n\n\
            \Databrary's unique 'upload-as-you-go' data management functionality also enables you to manage, organize, and preserve your videos and associated metadata in a secure web-based library. \
            \With this feature, you can add data and video files as soon as they are collected so that they are organized and securely backed-up. \
            \Data remains private and accessible only to your lab members and collaborators until you are ready to share with the Databrary community. \
            \Sharing is as easy as clicking a button!\
            \\n\n\
            \To help you get started, we've developed a template Databrary Release form for obtaining informed consent for sharing from participants, which can be found here: http://databrary.org/access/policies/release-template.html\n\
            \It's completely adaptable and can be added to new or existing IRB protocols. \
            \We also have lots of information and helpful tips in our User Guide: http://databrary.org/access/guide\
            \\n\n\
            \Our support team is dedicated to providing assistance to the Databrary community. \
            \Please contact us at support@databrary.org with any questions or for help getting started.\
            \\n"
          ]
      return a
  case api of
    JSON -> return $ okResponse [] $ JSON.Object $ Fold.foldMap authorizeJSON a JSON..+ ("party" JSON..= partyJSON o)
    HTML -> peeks $ otherRouteResponse [] viewAuthorize arg

deleteAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
deleteAuthorize = action DELETE (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \arg@(api, i, AuthorizeTarget app oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction =<< lookupParty oi
  let (child, parent) = if app then (p, o) else (o, p)
  _ <- removeAuthorize $ Authorize (Authorization mempty child parent) Nothing
  case api of
    JSON -> return $ okResponse [] $ JSON.object ["party" JSON..= partyJSON o]
    HTML -> peeks $ otherRouteResponse [] viewAuthorize arg

postAuthorizeNotFound :: ActionRoute (API, PartyTarget)
postAuthorizeNotFound = action POST (pathAPI </> pathPartyTarget </< "notfound") $ \(api, i) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  agent <- peeks $ fmap accountEmail . partyAccount
  (name, info) <- runForm Nothing $ liftM2 (,)
    ("name" .:> deform)
    ("info" .:> deformNonEmpty deform)
  authaddr <- peeks authorizeAddr
  sendMail authaddr
    ("Databrary authorization request from " <> partyName p)
    $ BSL.fromChunks [TE.encodeUtf8 (partyName p), " <", Fold.fold agent, "> has requested to be authorized by ", TE.encodeUtf8 name, maybe "" (\it -> " (" <> TE.encodeUtf8 it <> ")") info, ".\n"]
  return $ emptyResponse noContent204 []
