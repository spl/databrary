{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Search.Solr
( search
) where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Network.HTTP.Client as HC

import Databrary.Has
import qualified Databrary.JSON as J
import Databrary.HTTP.Client
import Databrary.Model.Paginate
import Databrary.Search.Service
import qualified Databrary.Search.Query as Q

data SolrQuery = SolrQuery 
  { solrQuery :: String
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
      [ "defType" J..= J.String "edismax"
      , "q" J..= solrQuery
      , "q.op" J..= J.String "AND"
      , "qf" J..= J.String "text_en^0.6 text_gen^1.5 keyword^10 tag_name^5 party_name^4"
      , "pf" J..= J.String "tag_name^5 keyword^10 party_name^4"
      , "ps" J..= J.Number 3
--    , "mm" J..= J.Number 75
      , "tie" J..= J.Number 0.1
      , "group" J..= True
      , "group.field" J..= J.String "content_type"
      , "group.limit" J..= J.Number 10
      , "group.offset" J..= paginateOffset solrPage
      , "spellcheck" J..= True
      , "spellcheck.collate" J..= True
--    , "spellcheck.collateParam.mm" J..= J.String "100%"
      ]
    ]

submitQuery :: HC.Request -> HTTPClient -> IO (Maybe J.Value)
submitQuery q = httpRequestJSON q

generatePostReq :: SolrQuery -> Solr -> HC.Request
generatePostReq sr Solr{ solrRequest = req } = req
  { HC.path = HC.path req <> "query"
  , HC.method = "POST"
  , HC.requestBody = HC.RequestBodyLBS $ J.encode sr
  , HC.requestHeaders = ("content-type", "application/json") : HC.requestHeaders req
  }

search :: (MonadSolr c m) => String -> Paginate -> m (Maybe J.Value)
search q page = do
      let query = Q.createQuery q
          (queryStr, args, join, cType) = Q.queryToString query
      -- This is hard-coded here so no one can send anything else...
      -- probably not the best.
          contentType = if cType == "container" then "content_type:record" else "content_type:(volume OR party)"
          modifiedQueryStr = (if(cType == "container") then "{!join from=volume_id to=volume_id} " else "")
                                                      ++ (if(length queryStr > 0) then queryStr else "*")
                                                      ++ (if(cType == "container") then " OR *" else "")
          sQuery = SolrQuery
            { solrQuery = modifiedQueryStr
            , solrArgs = (contentType ++ (if(length args > 0) then " AND " else " ") ++ args)
            , solrJoin = join
            , solrPage = page
            }
      request <- peeks (generatePostReq sQuery)
      liftIO $ do -- TODO REMOVE THIS
        print q
        print $ J.encode sQuery
        print contentType
        print request
      focusIO $ submitQuery request
