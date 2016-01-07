{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.VolumeState.SQL
  ( selectVolumeState
  ) where

import Databrary.Model.SQL.Select
import Databrary.Model.VolumeState.Types

selectVolumeState :: Selector -- ^ @'Volume' -> 'VolumeState'@
selectVolumeState = selectColumns 'VolumeState "volume_state" ["key", "value", "public"]
