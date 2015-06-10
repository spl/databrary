{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, UndecidableInstances, StandaloneDeriving #-}
module Databrary.Model.Id.Types 
  ( IdType
  , Id(..)
  , Int32
  ) where

import Control.Applicative ((<$>))
import qualified Data.Aeson as JSON
import Data.Hashable (Hashable(..))
import Data.Int (Int32)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Database.PostgreSQL.Typed.Dynamic (PGRep)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.HTTP.Form.Deform (Deform(..))

type family IdType a
newtype Id a = Id { unId :: IdType a }

deriving instance Eq (IdType a) => Eq (Id a)
deriving instance Ord (IdType a) => Ord (Id a)
instance Hashable (IdType a) => Hashable (Id a) where
  hashWithSalt i = hashWithSalt i . unId
  hash = hash . unId

instance PGParameter t (IdType a) => PGParameter t (Id a) where
  pgEncode t (Id i) = pgEncode t i
  pgEncodeValue e t (Id i) = pgEncodeValue e t i
  pgLiteral t (Id i) = pgLiteral t i
instance PGColumn t (IdType a) => PGColumn t (Id a) where
  pgDecode t = Id . pgDecode t
  pgDecodeValue e t = Id . pgDecodeValue e t
instance (PGParameter t (IdType a), PGColumn t (IdType a), PGRep t (IdType a)) => PGRep t (Id a)

instance Show (IdType a) => Show (Id a) where
  showsPrec p (Id a) = showsPrec p a
  show (Id a) = show a

instance JSON.ToJSON (IdType a) => JSON.ToJSON (Id a) where
  toJSON (Id a) = JSON.toJSON a
instance JSON.FromJSON (IdType a) => JSON.FromJSON (Id a) where
  parseJSON = fmap Id . JSON.parseJSON

instance Deform (IdType a) => Deform (Id a) where
  deform = Id <$> deform

instance TH.Lift (IdType a) => TH.Lift (Id a) where
  lift (Id i) = TH.conE 'Id `TH.appE` TH.lift i
