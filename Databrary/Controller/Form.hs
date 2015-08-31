{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Form
  ( FormData
  , runFormFiles
  , runForm
  , blankForm

  , emailTextForm
  , passwordForm
  , paginateForm
  , csrfForm
  ) where

import Control.Applicative (Applicative, (<$>), (<*>), (<|>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.Foldable as Fold
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Network.HTTP.Types (badRequest400)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Regex.Posix as Regex

import Databrary.Has (peeks)
import Databrary.Model.Paginate
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Service.Passwd
import Databrary.HTTP.Parse (FileContent)
import Databrary.HTTP.Form (FormData)
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Form.View (runFormView, blankFormView)
import Databrary.HTTP.Form.Errors (FormErrors)
import Databrary.Action.Response
import Databrary.Action.Types
import Databrary.Action.Form (getFormData)
import Databrary.Controller.Permission (checkVerfHeader)
import Databrary.View.Form (FormHtml)

jsonFormErrors :: FormErrors -> Response
jsonFormErrors = response badRequest400 [] . JSON.toJSON

htmlFormErrors :: (FormErrors -> Html.Html) -> FormErrors -> Response
htmlFormErrors f = response badRequest400 [] . f

handleForm :: MonadIO m => (FormErrors -> Response) -> Either FormErrors a -> m a
handleForm re = either (result . re) return

handleFormErrors :: MonadIO m => Maybe (FormErrors -> Html.Html) -> Either FormErrors a -> m a
handleFormErrors = handleForm . maybe jsonFormErrors htmlFormErrors

runFormWith :: (MonadAction q m, MonadIO m) => FormData f -> Maybe (q -> FormHtml f) -> DeformT f m a -> m a
runFormWith fd mf fa = do
  req <- ask
  let fv hv = runFormView (hv req) fd
  handleFormErrors (fv <$> mf) =<< runDeform fa fd

runFormFiles :: (MonadAction q m, MonadIO m, FileContent f) => [(BS.ByteString, Word64)] -> Maybe (q -> FormHtml f) -> DeformT f m a -> m a
runFormFiles fl mf fa = do
  fd <- getFormData fl
  runFormWith fd mf fa

runForm :: (MonadAction q m, MonadIO m) => Maybe (q -> FormHtml ()) -> DeformT () m a -> m a
runForm = runFormFiles []

blankForm :: FormHtml f -> Response
blankForm = okResponse [] . blankFormView

emailRegex :: Regex.Regex
emailRegex = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt
  ("^[-a-z0-9!#$%&'*+/=?^_`{|}~.]*@[a-z0-9][a-z0-9\\.-]*[a-z0-9]\\.[a-z][a-z\\.]*[a-z]$" :: String)

emailTextForm :: (Functor m, Monad m) => DeformT f m BS.ByteString
emailTextForm = do
  e <- deformCheck "Invalid email address" (Regex.matchTest emailRegex) =<< deform
  return $ maybe e (uncurry ((. BSC.map toLower) . (<>)) . flip BS.splitAt e) $ BSC.elemIndex '@' e

passwordForm :: (MonadIO m, MonadHasPasswd c m) => Account -> DeformT f m BS.ByteString
passwordForm acct = do
  p <- "once" .:> do
    p <- deform
    deformGuard "Password too short. Must be 7 characters." (7 <= BS.length p)
    c <- lift $ passwdCheck p (accountEmail acct) (TE.encodeUtf8 $ partyName $ accountParty acct)
    Fold.mapM_ (deformError . ("Insecure password: " <>) . TE.decodeLatin1) c
    return p
  "again" .:> do
    a <- deform
    deformGuard "Passwords do not match." (a == p)
  pw <- liftIO $ BCrypt.hashPasswordUsingPolicy passwordPolicy p
  deformMaybe' "Error processing password." pw

paginateForm :: (Applicative m, Monad m) => DeformT f m Paginate
paginateForm = Paginate
  <$> get "offset" paginateOffset
  <*> get "limit" paginateLimit
  where get t f = t .:> (deformCheck ("invalid " <> t) (\i -> i >= f minBound && i <= f maxBound) =<< deform) <|> return (f def)

csrfForm :: (MonadAction q m) => DeformT f m ()
csrfForm = do
  r <- lift checkVerfHeader
  unless r $ do
    verf <- lift $ peeks identityVerf
    "csverf" .:> maybe
      (deformError "You must be logged in to perform this request.")
      (\v -> deformGuard "Invalid form verifier. Please reload and try again." . (v ==) =<< deform)
      verf
