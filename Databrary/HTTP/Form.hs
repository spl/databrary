{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.HTTP.Form 
  ( FormKey(..)
  , FormPath
  , formPathText
  , FormData
  , FormDatum(..)
  , Form(..)
  , initForm
  , subForm
  , subForms
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.Wai.Parse (FileInfo)

import Databrary.Has (Has(..))
import Databrary.HTTP.Form.Data

data FormKey 
  = FormField !T.Text
  | FormIndex !Int
  deriving (Eq, Ord)

type FormPath = [FormKey]

formSubPath :: FormKey -> FormPath -> FormPath
formSubPath k p = p ++ [k]

instance Has BS.ByteString FormKey where
  view (FormField t) = TE.encodeUtf8 t
  view (FormIndex i) = BSC.pack $ show i

instance Has T.Text FormKey where
  view (FormField t) = t
  view (FormIndex i) = T.pack $ show i

dotsBS :: [BS.ByteString] -> BS.ByteString
dotsBS = BS.intercalate (BSC.singleton '.')

dotBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
dotBS a b
  | BS.null a = b
  | otherwise = dotsBS [a, b]

formSubBS :: FormKey -> BS.ByteString -> BS.ByteString
formSubBS k b = b `dotBS` view k

formPathText :: FormPath -> T.Text
formPathText = T.intercalate (T.singleton '.') . map view

data FormDatum
  = FormDatumNone
  | FormDatumBS !BS.ByteString
  | FormDatumJSON !JSON.Value
  deriving (Show)

instance Monoid FormDatum where
  mempty = FormDatumNone
  mappend FormDatumNone x = x
  mappend x _ = x

data Form a = Form
  { formData :: !(FormData a)
  , formPath :: FormPath
  , formPathBS :: BS.ByteString
  , formJSON :: Maybe JSON.Value
  , formDatum :: FormDatum
  , formFile :: Maybe (FileInfo a)
  }

-- makeHasRec ''Form ['formData, 'formPath, 'formPathBS, 'formDatum, 'formFile]

initForm :: FormData a -> Form a
initForm d = form where form = Form d [] "" (formDataJSON d) (getFormDatum form) Nothing

formSubJSON :: FormKey -> JSON.Value -> Maybe JSON.Value
formSubJSON k (JSON.Object o) = HM.lookup (view k) o
formSubJSON (FormIndex i) (JSON.Array a) = a V.!? i
formSubJSON _ _ = Nothing

subForm :: FormKey -> Form a -> Form a
subForm key form = form' where
  form' = form
    { formPath = formSubPath key $ formPath form
    , formPathBS = formSubBS key $ formPathBS form
    , formJSON = formSubJSON key =<< formJSON form
    , formDatum = getFormDatum form'
    , formFile = getFormFile form'
    }

formEmpty :: Form a -> Bool
formEmpty Form{ formJSON = Just _ } = False
formEmpty Form{ formPathBS = p, formData = FormData{..} } =
  me formDataQuery || me formDataPost where
  me = not . Fold.any (sk . fst) . Map.lookupGE p
  sk s = p `BS.isPrefixOf` s && (l == BS.length s || BSC.index s l == '.')
  l = BS.length p

subForms :: Form a -> [Form a]
subForms form = sf 0 where
  n | Just (JSON.Array v) <- formJSON form = V.length v
    | otherwise = 0
  sf i
    | i >= n && formEmpty el = []
    | otherwise = el : sf (succ i)
    where el = subForm (FormIndex i) form

jsonFormDatum :: Form a -> FormDatum
jsonFormDatum Form{ formJSON = j } = Fold.foldMap FormDatumJSON j

queryFormDatum :: Form a -> FormDatum
queryFormDatum Form{ formData = FormData{ formDataQuery = m }, formPathBS = p } =
  Fold.foldMap (maybe (FormDatumJSON JSON.Null) FormDatumBS) $ Map.lookup p m

postFormDatum :: Form a -> FormDatum
postFormDatum Form{ formData = FormData{ formDataPost = m }, formPathBS = p } =
  Fold.foldMap FormDatumBS $ Map.lookup p m

getFormDatum :: Form a -> FormDatum
getFormDatum form = postFormDatum form <> jsonFormDatum form <> queryFormDatum form

getFormFile :: Form a -> Maybe (FileInfo a)
getFormFile Form{ formData = FormData{ formDataFiles = f }, formPathBS = p } = Map.lookup p f
