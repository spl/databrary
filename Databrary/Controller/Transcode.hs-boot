module Databrary.Controller.Transcode where

import Databrary.Model.Id.Types
import Databrary.Model.Transcode.Types
import Databrary.Action

data TranscodeAction
  = TranscodeStart
  | TranscodeStop
  | TranscodeFail
instance Show TranscodeAction

remoteTranscode :: AppRoute (Id Transcode)
postTranscode :: AppRoute (Id Transcode)
