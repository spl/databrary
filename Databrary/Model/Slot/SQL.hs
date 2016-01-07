{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Slot.SQL
  ( selectSlotId
  , slotKeys
  ) where

import Databrary.Model.SQL.Select
import Databrary.Model.Slot.Types

selectSlotId :: String -> Selector
selectSlotId table = selectColumns 'SlotId table ["container", "segment"]

slotKeys :: String -- ^ @'Slot'@
  -> [(String, String)]
slotKeys o =
  [ ("container", "${containerId $ containerRow $ slotContainer " ++ o ++ "}")
  , ("segment", "${slotSegment " ++ o ++ "}")
  ]
