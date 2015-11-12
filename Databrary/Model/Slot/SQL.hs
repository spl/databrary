module Databrary.Model.Slot.SQL
  ( slotKeys
  ) where

slotKeys :: String -- ^ @'Slot'@
  -> [(String, String)]
slotKeys o =
  [ ("container", "${containerId $ containerRow $ slotContainer " ++ o ++ "}")
  , ("segment", "${slotSegment " ++ o ++ "}")
  ]
