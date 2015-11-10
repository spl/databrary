{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Container.Types
  ( Container(..)
  ) where

import qualified Data.Text as T

import Databrary.Has (makeHasRec)
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Release.Types
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types

type instance IdType Container = Int32

data Container = Container
  { containerId :: Id Container
  , containerTop :: Bool
  , containerName :: Maybe T.Text
  , containerDate :: Maybe Date
  , containerRelease :: Maybe Release
  , containerVolume :: Volume
  }

instance Kinded Container where
  kindOf _ = "container"

makeHasRec ''Container ['containerId, 'containerRelease, 'containerVolume]
