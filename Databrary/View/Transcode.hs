{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Transcode
  ( htmlTranscodes
  ) where

import Control.Monad (forM_)
import qualified Data.Foldable as Fold
import qualified Text.Blaze.Html5 as H

import Databrary.Has (view)
import Databrary.Action.Auth
import Databrary.Action
import Databrary.Model.Transcode
import Databrary.Model.Asset
import Databrary.Model.Party
import Databrary.Controller.Paths
import Databrary.View.Html
import Databrary.View.Template

import Databrary.Controller.Asset
import Databrary.Controller.Party

htmlTranscodes :: [Transcode] -> AuthRequest -> H.Html
htmlTranscodes tl req = htmlTemplate req (Just "transcodes") $ \js -> do
  H.table $ do
    H.thead $ H.tr $
      mapM_ H.th
        [ "action"
        , "id"
        , "time"
        , "owner"
        , "source"
        , "segment"
        , "options"
        , "pid"
        , "log"
        ]
    H.tbody $
      forM_ tl $ \Transcode{..} -> H.tr $ do
        H.td $ return ()
          {-
          @widget.formErrors(form)
          @if(t.process) {
            <button name='stop' value='true'>stop</button>
          } else {
            <button type='submit'>
              @if(t.fake) {
                start
              } else {
                restart
              }
            </button>
          }
          -}
        H.td $ H.a H.! actionLink viewAsset (HTML, assetId transcodeAsset) js [] $
          H.string $ show $ assetId transcodeAsset
        H.td $ Fold.foldMap (H.string . show) transcodeStart
        H.td $ do
          let p = view transcodeOwner
          H.a H.! actionLink viewParty (HTML, TargetParty (partyId p)) js [] $
            H.text $ partyName p
        H.td $ H.a H.! actionLink viewAsset (HTML, assetId transcodeOrig) js [] $
          maybe (H.string $ show $ assetId transcodeOrig) H.text (assetName transcodeOrig)
        H.td $ H.string $ show transcodeSegment
          {-
          @if(t.fake) {
            @widget.tag.inputText(form.start(), args = 'size -> 9)
            &ndash;
            @widget.tag.inputText(form.end(), args = 'size -> 9)
          } else {
            @display(t.segment)
          }
          -}
        H.td $ mapM_ ((>>) " " . H.string) transcodeOptions
        H.td $ Fold.foldMap (H.string . show) transcodeProcess
        H.td $ Fold.foldMap (H.pre . byteStringHtml) transcodeLog
