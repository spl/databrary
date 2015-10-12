{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, RecordWildCards, OverlappingInstances #-}
module Databrary.Store.Config
  ( Path(..)
  , pathKey
  , keyPath
  , Value(..)
  , Config
  , configMap
  , configPath
  , load
  , Configurable(..)
  , get
  , (!)
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((*>), (<|>))
import Control.Exception (Exception, throw)
import Control.Monad ((<=<), unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable, TypeRep, typeRep)
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT

import Databrary.Ops

type Key = BS.ByteString
newtype Path = Path [Key] deriving (Monoid)

pathKey :: Path -> Key
pathKey (Path p) = BS.intercalate (BSC.singleton '.') p

keyPath :: Key -> Path
keyPath = Path . BSC.split '.'

instance Show Path where
  showsPrec p = showsPrec p . pathKey

instance IsString Path where
  fromString = keyPath . fromString

data ConfigError
  = ParseError P.ParseError
  | ValueError
    { errorPath :: Path
    , errorValue :: Value
    , errorNeeded :: TypeRep
    }
  deriving (Typeable, Show)

instance Exception ConfigError

data Value
  = Empty
  | Boolean !Bool
  | Integer !Integer
  | String !BS.ByteString
  | List [Value]
  | Sub !Config
  deriving (Typeable, Show)

type ConfigMap = HM.HashMap BS.ByteString Value

data Config = Config
  { configPath' :: ![Key]
  , configMap :: !ConfigMap
  } deriving (Typeable, Show)

configPath :: Config -> Path
configPath = Path . reverse . configPath'

type Parser = P.Parsec BSL.ByteString Config

parser :: Parser Config
parser = whiteSpace *> block *> P.eof *> P.getState where
  block = P.skipMany pair
  pair = do
    c@Config{ configMap = cm } <- P.getState
    ks <- identifier P.<?> "key"
    let k = BSC.pack ks
        k' = k : configPath' c
    P.setState =<< case HM.lookupDefault Empty k cm of
      Empty -> return $ Config k' HM.empty
      Sub kc -> return kc
      _ -> dup k'
    r <- lexeme dot *> (Nothing <$ pair) <|> rhs
    kc <- P.getState
    kv <- maybe (return $ Sub kc) (\v -> do
      unless (HM.null $ configMap kc) $ dup k'
      return v) r
    P.putState c{ configMap = HM.insert k kv cm }
  rhs =
    Nothing <$ braces block <|>
    lexeme (P.char '=') *> (Just <$> val)
  val = P.choice
    [ Boolean True <$ reserved "true"
    , Boolean False <$ reserved "false"
    , Integer <$> integer
    , String . BSC.pack <$> stringLiteral
    , List <$> brackets (commaSep val)
    ] P.<?> "value"
  PT.TokenParser{..} = PT.makeTokenParser PT.LanguageDef
    { PT.commentStart = ""
    , PT.commentEnd = ""
    , PT.commentLine = "#"
    , PT.nestedComments = False
    , PT.identStart = P.letter
    , PT.identLetter = (P.alphaNum <|> P.oneOf "-_")
    , PT.opStart = P.unexpected "operator"
    , PT.opLetter = P.unexpected "operator"
    , PT.reservedNames = []
    , PT.reservedOpNames = ["="]
    , PT.caseSensitive = True
    }
  dup = fail . ("Duplicate/conflicting key value: " ++) . show . Path . reverse

load :: FilePath -> IO Config
load f = do
  i <- BSLC.readFile f
  either (throw . ParseError) return $ P.runP parser (Config [] HM.empty) f i

lookup :: Config -> [Key] -> Value
lookup c [] = Sub c
lookup Config{ configMap = cm } [k] | Just v <- HM.lookup k cm = v
lookup Config{ configMap = cm } (k:l) | Just (Sub kc) <- HM.lookup k cm = lookup kc l
lookup _ _ = Empty

class Typeable a => Configurable a where
  config :: Value -> Maybe a

instance Configurable Value where
  config = Just

instance Configurable a => Configurable (Maybe a) where
  config Empty = Just Nothing
  config v = Just <$> config v

instance Configurable Bool where
  config (Boolean b) = Just b
  config _ = Nothing

instance Configurable Integer where
  config (Integer i) = Just i
  config _ = Nothing

instance Configurable BS.ByteString where
  config (String s) = Just s
  config _ = Nothing

instance Configurable a => Configurable [a] where
  config (List l) = mapM config l
  config _ = Nothing

instance Configurable Config where
  config (Sub c) = Just c
  config Empty = Just $ Config [] HM.empty
  config _ = Nothing

instance Configurable T.Text where
  config = rightJust . TE.decodeUtf8' <=< config

instance Configurable String where
  config v = BSC.unpack <$> config v

configBoundedInt :: forall a . (Integral a, Bounded a) => Value -> Maybe a
configBoundedInt = f <=< config where
  f i = i >= toInteger (minBound :: a) && i <= toInteger (maxBound :: a) ?> fromInteger i

instance Configurable Int where
  config = configBoundedInt

config' :: Configurable a => Path -> Value -> a
config' p v = fromMaybe (throw $ ValueError p v $ typeRep r) r where r = config v

infixl 9 !
(!) :: Configurable a => Config -> Path -> a
c ! Path p = config' (Path $ reverse (configPath' c) ++ p) $ lookup c p

get :: Configurable a => Path -> Config -> a
get = flip (!)
