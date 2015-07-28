{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.JSON
  ( module Data.Aeson
  , module Data.Aeson.Types
  , object
  , record
  , recordMap
  , (.+)
  , (.+?)
  , (.++)
  , (.!)
  , (.!?)
  , eitherJSON
  , Query
  , jsonQuery
  , escapeByteString
  , quoteByteString
  ) where

import Data.Aeson hiding (object)
import Data.Aeson.Types hiding (object)
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import Data.ByteString.Internal (c2w)
import qualified Data.Configurator.Types as C
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Traversable as Trav
import qualified Data.Vector as V
import Data.Word (Word8)
import Network.HTTP.Types (Query)

object :: [Pair] -> Object
object = HM.fromList

record :: ToJSON k => k -> [Pair] -> Object
record k = object . (("id" .= k) :)

recordMap :: [Object] -> Value
recordMap = Object . HM.fromList . map (\o -> (tt $ o HM.! "id", Object o)) where
  tt (String t) = t
  tt v = TL.toStrict $ TLB.toLazyText $ encodeToTextBuilder v

infixl 4 .+, .+?, .++
(.+) :: Object -> Pair -> Object
o .+ (k, v) = HM.insert k v o

(.+?) :: Object -> Maybe Pair -> Object
o .+? Nothing = o
o .+? Just p = o .+ p

(.++) :: Object -> [Pair] -> Object
(.++) = foldl' (.+)

(.!) :: FromJSON a => Array -> Int -> Parser a
a .! i = maybe (fail $ "index " ++ show i ++ " out of range") parseJSON $ a V.!? i

(.!?) :: FromJSON a => Array -> Int -> Parser (Maybe a)
a .!? i = Trav.mapM parseJSON $ a V.!? i

resultToEither :: Result a -> Either String a
resultToEither (Error e) = Left e
resultToEither (Success a) = Right a

eitherJSON :: FromJSON a => Value -> Either String a
eitherJSON = resultToEither . fromJSON

instance ToJSON BS.ByteString where
  toJSON = String . TE.decodeUtf8

instance ToJSON C.Value where
  toJSON (C.Bool b) = Bool b
  toJSON (C.String t) = String t
  toJSON (C.Number r) = Number $ fromRational r
  toJSON (C.List l) = Array $ V.fromList $ map toJSON l

jsonQuery :: (Monad m) => Object -> (BS.ByteString -> Maybe BS.ByteString -> m (Maybe Value)) -> Query -> m Object
jsonQuery j _ [] = return j
jsonQuery j f ((k,v):q) = do
  o <- f k v
  jsonQuery (j .+? fmap ((,) (TE.decodeLatin1 k)) o) f q

wordEscaped :: Char -> BP.BoundedPrim Word8
wordEscaped q =
  BP.condB (== c2w q) (backslash q) $
  BP.condB (== c2w '\\') (backslash '\\') $
  BP.condB (>= c2w ' ') (BP.liftFixedToBounded BP.word8) $
  BP.condB (== c2w '\n') (backslash 'n') $
  BP.condB (== c2w '\r') (backslash 'r') $
  BP.condB (== c2w '\t') (backslash 't') $
    BP.liftFixedToBounded $ (\c -> ('\\', ('u', fromIntegral c))) BP.>$< BP.char7 BP.>*< BP.char7 BP.>*< BP.word16HexFixed
  where
  backslash c = BP.liftFixedToBounded $ const ('\\', c) BP.>$< BP.char7 BP.>*< BP.char7

-- | Escape (but do not quote) a ByteString
escapeByteString :: Char -> BS.ByteString -> B.Builder
escapeByteString = BP.primMapByteStringBounded . wordEscaped

quoteByteString :: Char -> BS.ByteString -> B.Builder
quoteByteString q s = B.char7 q <> escapeByteString q s <> B.char7 q
