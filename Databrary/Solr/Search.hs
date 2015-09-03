{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Solr.Search
  ( SearchType(..)
  , SearchQuery(..)
  , search
  ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldMap)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.URI (renderSimpleQuery)

import Data.ByteString.Builder.Escape (escapeLazyByteStringCharsWith, escapeTextWith)
import Databrary.Has
import qualified Databrary.JSON as J
import Databrary.HTTP.Client
import Databrary.Model.Paginate
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Solr.Service

data SearchType
  = SearchVolumes
  | SearchParties
  | SearchVolume (Id Volume)

data SearchQuery = SearchQuery
  { searchString :: Maybe T.Text
  , searchTerms :: [(T.Text, T.Text)]
  , searchType :: SearchType
  , searchPaginate :: !Paginate
  }

quoteQuery :: (Char -> String -> a -> B.Builder) -> a -> B.Builder
quoteQuery e s = B.char7 '"' <> e '\\' "\"\\" s <> B.char7 '"'

quoteBuilder :: B.Builder -> B.Builder
quoteBuilder = quoteQuery escapeLazyByteStringCharsWith . B.toLazyByteString

quoteText :: T.Text -> B.Builder
quoteText = quoteQuery escapeTextWith

defaultParams :: B.Builder
defaultParams = B.string7 "qf=\"text_en^0.6 text_gen^1.5 keyword^10 tag_name^5 party_name^4\" pf=\"tag_name^5 keyword^10 party_name^4\" ps=3"

search :: MonadSolr c m => SearchQuery -> m (Maybe J.Value)
search SearchQuery{..} = do
  req <- peeks solrRequest
  focusIO $ httpRequestJSON req
    { HC.path = HC.path req <> "search"
    , HC.queryString = renderSimpleQuery True query
    }
  where
  query = 
    [ ("q", BSL.toStrict $ B.toLazyByteString $ qt <> uw ql)
    , ("q.op", "AND")
    , ("start", BSC.pack $ show $ paginateOffset searchPaginate)
    , ("rows", BSC.pack $ show $ paginateLimit searchPaginate)
    , ("fq", "content_type:" <> ct)
    ]
  (ct, qt) = case searchType of
    SearchVolumes -> ("volume", B.string7 "{!join from=volume_id to=volume_id}")
    SearchParties -> ("party", mempty)
    SearchVolume v -> ("(-volume)", B.string7 "volume_id:" <> B.int32Dec (unId v) <> B.char7 ' ')
  ql = maybe id ((:) . bp defaultParams) searchString $ map (uncurry bt) searchTerms
  bt f = bp (B.string7 "qf=" <> quoteText f)
  bp p v = B.string7 "_query_:" <> quoteBuilder (B.string7 "{!dismax " <> p <> B.char7 '}' <> TE.encodeUtf8Builder v)
  uw [] = B.char7 '*'
  uw (t:l) = t <> foldMap (B.char7 ' ' <>) l
