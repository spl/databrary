{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Authorize
  ( viewAuthorize
  , postAuthorize
  , deleteAuthorize
  , postAuthorizeNotFound
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when, liftM3, mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Foldable (fold)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime(..), fromGregorian, addGregorianYearsRollOver)
import Network.HTTP.Types (noContent204, StdMethod(DELETE))

import Databrary.Ops
import Databrary.Has (peek, peeks)
import qualified Databrary.JSON as JSON
import qualified Databrary.Store.Config as C
import Databrary.Service.Mail
import Databrary.Service.Messages
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

authorizeTitle :: Permission -> Messages -> T.Text
authorizeTitle site = getMessage $ C.Path ["auth", "site", BSC.pack (show site), "title"]

postAuthorize :: ActionRoute (API, PartyTarget, AuthorizeTarget)
postAuthorize = action POST (pathAPI </>> pathPartyTarget </> pathAuthorizeTarget) $ \arg@(api, i, AuthorizeTarget app oi) -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction . mfilter ((0 <) . unId . partyId . partyRow) =<< lookupParty oi
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
          ("Databrary authorization request from " <> partyName (partyRow child))
          $ BSL.fromChunks [TE.encodeUtf8 (partyName $ partyRow child), " <", fold agent, "> has requested to be authorized by ", TE.encodeUtf8 (partyName $ partyRow parent), ". \
            \To approve or reject this authorization request, go to:\n\n" ] <>
            BSB.toLazyByteString (actionURL (Just req) viewPartyEdit (TargetParty $ partyId $ partyRow parent) [("page", Just "grant")]) <> "#auth-" <> BSLC.pack (show $ partyId $ partyRow child) <> "\n\n\
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
          expires <- "expires" .:> (deformCheck "Expiration must be within two years." (all (\e -> su || e > minexp && e <= maxexp))
            =<< (<|> (su ?!> maxexp)) <$> deformNonEmpty deform)
          return $ Authorize (Authorization (Access site member) child parent) $ fmap (`UTCTime` 43210) expires
      maybe (mapM_ removeAuthorize c) changeAuthorize a
      let site = foldMap accessSite a
      when (PermissionPUBLIC < site && all ((PermissionPUBLIC >=) . accessSite) c) $ do
        sitemsg <- peeks $ authorizeTitle site
        sendMail (maybe id (:) (Right <$> partyAccount child) authaddr)
          "Databrary authorization approved"
          $ BSL.fromChunks
          [ "Dear ", TE.encodeUtf8 (partyName $ partyRow child), ",\n\n\
            \You have been authorized by ", TE.encodeUtf8 (partyName $ partyRow parent), ", as a Databrary ", TE.encodeUtf8 sitemsg, ". \
            \Your authorization allows you to access all the shared data in Databrary. \
            \Our primary goal is to inspire you to reuse shared videos on Databrary to ask new questions outside the scope of the original study. \
            \You will also find illustrative video excerpts that you can use for teaching and to learn about researchers' methods and procedures.\
            \\n\n\
            \Databrary's unique \"active curation\" functionality allows you to upload your videos as you collect them so that your data are backed up and preserved in our free, secure library, your videos are immediately available to you and your collaborators offsite, and your data are organized and ready for sharing. \
            \Your data will remain private and accessible only to your lab members and collaborators until you are ready to share with the Databrary community. \
            \When you are ready, sharing is as easy as clicking a button!\
            \\n\n\
            \To share your data, you can use our template Databrary release form for obtaining permission for sharing from your participants, which can be found here: http://databrary.org/access/policies/release-template.html\n\
            \The release form can be added to new or existing IRB protocols. \
            \It is completely adaptable and can be customized to suit your needs. \
            \We also have lots of information and helpful tips about managing and sharing your video data in our User Guide: http://databrary.org/access/guide\n\
            \As soon as your protocol is amended to allow you to share data, you can start uploading your data from each new session. \
            \Don't wait until your study is complete to upload your videos. \
            \It's much easier to upload data after each data collection while your study is in progress!\
            \\n\n\
            \We are dedicated to providing assistance to the Databrary community. \
            \Please contact us at support@databrary.org with questions or for help getting started.\
            \\n"
          ]
      return a
  case api of
    JSON -> return $ okResponse [] $ JSON.Object $ foldMap authorizeJSON a JSON..+ ("party" JSON..= partyJSON o)
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

postAuthorizeNotFound :: ActionRoute (PartyTarget)
postAuthorizeNotFound = action POST (pathJSON >/> pathPartyTarget </< "notfound") $ \i -> withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  agent <- peeks $ fmap accountEmail . partyAccount
  (name, perm, info) <- runForm Nothing $ liftM3 (,,)
    ("name" .:> deform)
    ("permission" .:> deform)
    ("info" .:> deformNonEmpty deform)
  authaddr <- peeks authorizeAddr
  title <- peeks $ authorizeTitle perm
  sendMail authaddr
    ("Databrary authorization request from " <> partyName (partyRow p))
    $ BSL.fromChunks [TE.encodeUtf8 (partyName $ partyRow p), " <", fold agent, ">", mbt (partyAffiliation $ partyRow p), " has requested to be authorized as an ", TE.encodeUtf8 title, " by ", TE.encodeUtf8 name, mbt info, ".\n"]
  return $ emptyResponse noContent204 []
  where mbt = maybe "" $ \t -> " (" <> TE.encodeUtf8 t <> ")"
