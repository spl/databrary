{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}
module Databrary.Model.Format.Types
  ( Format(..)
  ) where

import qualified Data.ByteString as BS
import Data.Function (on)
import Data.Int (Int16)
import Data.Ord (comparing)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLift)

import Databrary.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types

type instance IdType Format = Int16

data Format = Format
  { formatId :: Id Format
  , formatMimeType :: BS.ByteString
  , formatExtension :: [BS.ByteString]
  , formatName :: T.Text
  }

instance Kinded Format where
  kindOf _ = "format"

instance Eq Format where
  (==) = on (==) formatId
  (/=) = on (/=) formatId

instance Ord Format where
  compare = comparing formatId

makeHasRec ''Format ['formatId]
deriveLift ''Format
