{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Volume.Types
  ( Volume(..)
  , MonadHasVolume
  , blankVolume
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Language.Haskell.TH.Lift (deriveLift)

import Databrary.Has (makeHasRec)
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Permission.Types
import Databrary.Model.Id.Types

type instance IdType Volume = Int32

data Volume = Volume
  { volumeId :: Id Volume
  , volumeName :: T.Text
  , volumeAlias :: Maybe T.Text
  , volumeBody :: Maybe T.Text
  , volumeDOI :: Maybe BS.ByteString
  , volumeCreation :: Timestamp
  , volumePermission :: Permission
  }

instance Kinded Volume where
  kindOf _ = "volume"

makeHasRec ''Volume ['volumeId, 'volumePermission]
deriveLift ''Volume

blankVolume :: Volume
blankVolume = Volume
  { volumeId = error "blankVolume"
  , volumeName = ""
  , volumeAlias = Nothing
  , volumeBody = Nothing
  , volumeDOI = Nothing
  , volumeCreation = posixSecondsToUTCTime 1357900000
  , volumePermission = PermissionNONE
  }
