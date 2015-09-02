{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Solr.Search
  ( SearchQuery(..)
  , search
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Method (methodGet)

import Databrary.Has
import qualified Databrary.JSON as J
import Databrary.HTTP.Client
import Databrary.Model.Paginate
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Solr.Service

data SearchQuery = SearchQuery
  { searchString :: Maybe T.Text
  , searchTerms :: [(T.Text, T.Text)]
  , searchVolume :: Maybe (Id Volume)
  , searchPaginate :: !Paginate
  }

quoteQuery :: T.Text -> T.Text
quoteQuery e = '"' `T.cons` T.concatMap q e `T.snoc` '"' where
  q '"' = "\\\""
  q '\\' = "\\\\"
  q c = T.singleton c

defaultParams :: T.Text
defaultParams = "qf=\"text_en^0.6 text_gen^1.5 keyword^10 tag_name^5 party_name^4\" pf=\"tag_name^5 keyword^10 party_name^4\" ps=3"

searchQuery :: SearchQuery -> T.Text
searchQuery q = T.unwords $ maybe id ((:) . bp defaultParams) (searchString q) $ map (uncurry bt) (searchTerms q) where
  bp p v = "_query_:" <> quoteQuery ("{!dismax " <> p <> "}" <> v)
  bt f = bp ("qf=" <> quoteQuery f)


data SolrQuery = SolrQuery 
  { solrQuery :: T.Text
  , solrArgs :: String
  , solrJoin :: String
  , solrPage :: !Paginate
  }

instance J.ToJSON SolrQuery where
  toJSON SolrQuery{..} = J.Object $ J.object
    [ "limit" J..= paginateLimit solrPage
    , "filter" J..= [solrJoin, solrArgs]
    , "facet" J..= J.object
      [ "content_type" J..= J.object
        [ "terms" J..= J.object
          [ "field" J..= J.String "content_type"
--        , "type" J..= J.String "terms"
          , "limit" J..= J.Number 10
          ]
        ]
      ]
    , "params" J..= J.object
      [ -- "defType" J..= J.String "edismax" ,
        "q" J..= solrQuery
--    , "q.op" J..= J.String "AND"
--    , "qf" J..= J.String "text_en^0.6 text_gen^1.5 keyword^10 tag_name^5 party_name^4"
--    , "pf" J..= J.String "tag_name^5 keyword^10 party_name^4"
--    , "ps" J..= J.Number 3
--    , "mm" J..= J.Number 75
--    , "tie" J..= J.Number 0.1
      , "group" J..= True
      , "group.field" J..= J.String "content_type"
      , "group.limit" J..= J.Number 10
      , "group.offset" J..= paginateOffset solrPage
--    , "spellcheck" J..= True
--    , "spellcheck.collate" J..= True
--    , "spellcheck.collateParam.mm" J..= J.String "100%"
      ]
    ]

submitQuery :: HC.Request -> HTTPClient -> IO (Maybe J.Value)
submitQuery q = httpRequestJSON q

generatePostReq :: SolrQuery -> Solr -> HC.Request
generatePostReq sr Solr{ solrRequest = req } = req
  { HC.path = HC.path req <> "search"
  , HC.method = methodGet
  , HC.requestBody = HC.RequestBodyLBS $ J.encode sr
  , HC.requestHeaders = ("content-type", "application/json") : HC.requestHeaders req
  }

search :: (MonadSolr c m) => SearchQuery -> m (Maybe J.Value)
search sq = do
      let query = searchQuery sq
--        (queryStr, args, join, cType) = Q.queryToString query
      -- This is hard-coded here so no one can send anything else...
      -- probably not the best.
          contentType = if isJust (searchVolume sq) then "content_type:record" else "content_type:(volume OR party)"
--        modifiedQueryStr = (if(cType == "container") then "{!join from=volume_id to=volume_id} " else "")
--                                                    ++ (if(length queryStr > 0) then queryStr else "*")
--                                                    ++ (if(cType == "container") then " OR *" else "")
          sQuery = SolrQuery
            { solrQuery = query
            , solrArgs = contentType -- ++ (if(length args > 0) then " AND " else " ") ++ args
            , solrJoin = "" -- join
            , solrPage = searchPaginate sq
            }
      request <- peeks (generatePostReq sQuery)
      liftIO $ do -- TODO REMOVE THIS
        print $ J.encode sQuery
        print contentType
        print request
      focusIO $ submitQuery request
