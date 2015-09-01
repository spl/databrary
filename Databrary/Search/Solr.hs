{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Search.Solr
( search
) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Monoid ((<>))
import qualified Network.HTTP.Client as HC

import Databrary.Has (peeks, focusIO)
import Databrary.JSON
import Databrary.HTTP.Client
import Databrary.Search.Service
import qualified Databrary.Search.Query as Q

data SolrQuery = SolrQuery {
   solrQuery :: String,
   solrArgs :: String,
   solrJoin :: String,
   solrLimit :: Int32,
   solrStart :: Int32
} deriving (Show)
instance ToJSON SolrQuery where
      toJSON SolrQuery{..} =
         Object $ object [
                  "limit" .= solrLimit,
                  "filter" .= [solrJoin, solrArgs],
                  "facet" .= object [
                    "content_type" .= object [
                        "terms" .= object [
    --                         "type" .= ("terms" :: String),
                            "field" .= ("content_type" :: String),
                            "limit" .= (10 :: Int)
                        ]
                    ]
                  ],
--                   "group" .= object [
--                     "field" .= ("content_type" :: String),
--                     "limit" .= (10 :: Int)
--                   ]
                  "params" .= object [
                        "defType" .= ("edismax" :: String),
                        "q" .= solrQuery,
                        "q.op" .= ("AND" :: String),
                        "qf" .= (volumeQf ++ (" " :: String) ++ containerQf),
                        "pf" .= (volumePf ++ (" " :: String) ++ containerPf),
                        "ps" .= (3 :: Int),
                        {- "mm" .= (75 :: Int), -}
                        "tie" .= (0.1 :: Float),
                        "group" .= ("true" :: String),
                        "group.field" .= ("content_type" :: String),
                        "group.limit" .= (10 :: Int),
                        "group.offset" .= solrStart,
                        "spellcheck" .= ("true" :: String),
                        "spellcheck.collate" .= ("true" :: String)
                        {- "spellcheck.collateParam.mm" .= ("100%" :: String) -}
                  ]
                ]

volumeQf, volumePf, containerQf, containerPf :: String
volumeQf = "text_en^0.6 text_exact^1.5 volume_keywords_ss^10.0 volume_tags_ss^5.0 party_name_s^5.0 volume_owner_names_ss^4"
volumePf = "volume_keywords_ss^10.0 volume_tags_ss^5.0 party_name_s^5.0 volume_owner_names_ss^4"
containerQf = "container_ethnicity_s^5 container_gender_s^5 container_race_s^5 container_text_t^3"
containerPf = "container_ethnicity_s^5 container_gender_s^5 container_race_s^5 container_text_t^3"

-- formQuery :: String -> SolrQuery
-- formQuery q = SolrQuery q

submitQuery :: HC.Request -> HTTPClient -> IO (Maybe Value)
submitQuery q = httpRequestJSON q

generatePostReq :: SolrQuery -> Solr -> HC.Request
generatePostReq sr Solr{ solrRequest = req } = req
  { HC.path = HC.path req <> "query"
  , HC.method = "POST"
  , HC.requestBody = HC.RequestBodyLBS $ encode sr
  , HC.requestHeaders = ("content-type", "application/json") : HC.requestHeaders req
  }

search :: (MonadSolr c m) => String -> Int32 -> Int32 -> m (Maybe Value)
search q offset limit = do
      let query = Q.createQuery q
          (queryStr, args, join, cType) = Q.queryToString query
      -- This is hard-coded here so no one can send anything else...
      -- probably not the best.
          contentType = if cType == "container" then "content_type:record" else "content_type:(volume OR party)"
          modifiedQueryStr = (if(cType == "container") then "{!join from=volume_id_i to=volume_id_i} " else "")
                                                      ++ (if(length queryStr > 0) then queryStr else "*")
                                                      ++ (if(cType == "container") then " OR *" else "")
          sQuery = SolrQuery
            { solrQuery = modifiedQueryStr
            , solrArgs = (contentType ++ (if(length args > 0) then " AND " else " ") ++ args)
            , solrJoin = join
            , solrLimit = limit
            , solrStart = offset
            }
      request <- peeks (generatePostReq sQuery)
      liftIO $ do -- TODO REMOVE THIS
        print q
        print $ encode sQuery
        print contentType
        print sQuery
        print request
      focusIO $ submitQuery request
