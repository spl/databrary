{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Zip
  ( htmlVolumeDescription
  ) where

import Control.Monad (void, unless, forM_)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.Foldable as Fold
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Format (formatTime)
import System.FilePath ((<.>))
import System.Locale (defaultTimeLocale)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Has (view)
import Databrary.Service.Messages
import Databrary.Model.Time
import Databrary.Model.Enum
import Databrary.Model.Release.Types
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Citation.Types
import Databrary.Model.Funding.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.View.Html

import {-# SOURCE #-} Databrary.Controller.Volume
import {-# SOURCE #-} Databrary.Controller.Party
import Databrary.Controller.Web

htmlTemplate :: T.Text -> H.Html -> H.Html
htmlTemplate title body = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.text title
  H.body $ body

htmlVolumeDescription :: Container -> [Citation] -> [Funding] -> [[AssetSlot]] -> [[AssetSlot]] -> AuthRequest -> H.Html
htmlVolumeDescription Container{ containerVolume = Volume{..} } cite fund _ _ req = htmlTemplate ("Databrary Volume " <> T.pack (show volumeId)) $ do
  H.h1 $
    H.a H.! (maybe (link viewVolume (HTML, volumeId)) (HA.href . byteStringValue . ("http://dx.doi.org/" <>)) volumeDOI)
      $ H.text volumeName
  H.ul $ forM_ volumeOwners $ \(i, n) ->
    H.li $
      H.a H.! link viewParty (HTML, TargetParty i)
        $ H.text n
  H.h2 "Volume description"
  Fold.mapM_ (H.p . H.text) volumeBody
  unless (null fund) $ do
    H.h3 "Funded by"
    H.dl $ forM_ fund $ \Funding{..} -> do
      H.dt $ H.text $ funderName fundingFunder
      mapM_ (H.dd . H.text) fundingAwards
  unless (null cite) $ do
    H.h3 "Related works"
    H.ul $ forM_ cite $ \Citation{..} -> H.li $
      maybe id (\u -> H.a H.! HA.href (H.toValue u)) citationURL $ do
        H.text citationHead
        Fold.forM_ citationYear $ \y ->
          " (" >> H.toMarkup (fromIntegral y :: Int) >> ")"
  H.h2 "Package information"
  H.dl $ do
    H.dt "Created"
    H.dd $ H.string $ formatTime defaultTimeLocale "%d %b %Y" volumeCreation
    H.dt "Downloaded"
    H.dd $ do
      H.string $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" (view req :: Timestamp)
      void $ " by "
      H.a H.! link viewParty (HTML, TargetParty $ view req)
        $ H.text $ partyName (view req)
  H.p $ do
    H.text $ msg "download.warning"
    void $ " For more information and terms of use see the "
    H.a H.! HA.href "http://databrary.org/access/policies/agreement.html"
      $ "Databrary Access Agreement"
    void $ "."
  H.h2 "Package contents"
  H.h3 "Legend of release levels"
  H.dl $ forM_ pgEnumValues $ \(_ :: Release, n) -> do
    H.dt $ H.string n
    H.dd $ do
      H.img H.! HA.src (builderValue $ actionURL (Just $ view req) webFile (Just $ staticPath ["icons", "release", BSC.pack $ map toLower n <.> "svg"]) [])
      H.text $ msg (fromString $ "release." ++ n ++ ".title")
      void $ ": "
      H.text $ msg (fromString $ "release." ++ n ++ ".description")
  where
  link r a = HA.href $ builderValue $ actionURL (Just $ view req) r a []
  msg m = getMessage m $ view req
