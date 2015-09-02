module Databrary.Solr.Query 
  ( createQuery
  , queryToString
  , Query
  ) where

import Data.List (isInfixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

-- Type for all of our documents
data QueryPart = QueryArg String String | QueryJoin String String String
                    | QuerySearchTerm String
                    | QueryType String deriving Show

data Query = Query {qJoin :: Maybe QueryPart,
                    qArguments :: Maybe [QueryPart],
                    qSearchTerms :: Maybe [QueryPart],
                    qContentType :: Maybe QueryPart} deriving Show

-- Join a bunch of query strings together with AND clauses
-- Note: has to be JOIN-MODIFIER ARGUMENT-MODIFIERS SEARCH-TERMS
createQuery :: String -> Query
createQuery x = formQuery $ parseQuery x

formQuery :: [QueryPart] -> Query
formQuery qs = Query j a t contentType
   where
      joinTest =  mapMaybe filterJoin qs
      contentTest = mapMaybe filterContentType qs
      j = if length joinTest >= 1 then Just $ head joinTest else Nothing
      a = Just $ mapMaybe filterArg qs
      t = Just $ concat $ mapMaybe filterTerm qs
      contentType = if length contentTest >= 1 then Just $ head contentTest else Nothing

filterArg :: QueryPart -> Maybe QueryPart
filterArg (QueryArg x y) = Just $ QueryArg x y
filterArg _ = Nothing


filterJoin :: QueryPart -> Maybe QueryPart
filterJoin (QueryJoin x y z) = Just $ QueryJoin x y z
filterJoin _ = Nothing

--
filterTerm :: QueryPart -> Maybe [QueryPart]
filterTerm (QuerySearchTerm x) = Just $ map QuerySearchTerm $ words x
filterTerm _ = Nothing

filterContentType :: QueryPart -> Maybe QueryPart
filterContentType (QueryType x) = Just $ QueryType x
filterContentType _ = Nothing

-- parseQueryString: We're given a query string from somewhere, figure out
-- what to do with it.
-- The format looks like "joinstuff,argstuff,free text stuff"
parseQuery :: String -> [QueryPart]
parseQuery q = map parseQueryPart x
      where
         x = splitOn "|" q

parseQueryPart :: String -> QueryPart
parseQueryPart x
   | isInfixOf "join=" x = parseQueryJoin x -- used
   | isInfixOf "arg=" x = parseQueryArg x -- used
   | isInfixOf "type=" x = parseQueryType x -- used
   | otherwise = parseTerm x -- used

-- Joins look like join=type,type and we just want the types
parseQueryJoin :: String -> QueryPart
parseQueryJoin x = QueryJoin arg1 arg2 filt
                     where
                        args = splitOn "," $ concat $ drop 1 $ splitOn "=" x
                        [arg1,arg2, filt] = args

-- Args looks like arg=argname:value
parseQueryArg :: String -> QueryPart
parseQueryArg x = QueryArg argName argVal
                     where
                        args = splitOn ":" $ concat $ drop 1 $ splitOn "=" x
                        [argName,argVal] = args

parseQueryType :: String -> QueryPart
parseQueryType q = QueryType contentType
                     where
                        x = splitOn "=" q
                        [_, contentType] = x

-- Terms just look like terms=what,ever,this,is
-- TODO add support for quoted strings
parseTerm :: String -> QueryPart
parseTerm x =  QuerySearchTerm x

joinToString :: Maybe QueryPart -> String
joinToString qp = do
      case qp of
         Nothing -> ""
         Just q -> joinToString' q

joinToString' :: QueryPart -> String
joinToString' (QueryJoin j1 j2 j3) = "{!join from=" ++ j1 ++ " to=" ++  j2 ++ "}" ++ j3 ++ " "
joinToString' _ = ""

argsToString :: Maybe [QueryPart] -> String
{-argsToString qp = intercalate " " map argsToString <$> qp-}
argsToString qp = do
      case qp of
         Nothing -> ""
         Just q -> intercalate " AND " (map argsToString' q)

argsToString' :: QueryPart -> String
argsToString' (QueryArg a1 a2) = a1 ++ ":" ++ a2
argsToString' _ = ""

termsToString :: Maybe [QueryPart] -> String
termsToString qp = do
      case qp of
         Nothing -> ""
         Just q -> intercalate " " (map termsToString' q)

termsToString' :: QueryPart -> String
termsToString' (QuerySearchTerm t1) = t1
termsToString' _ = ""

contentTypeToString :: Maybe QueryPart -> String
contentTypeToString qp = do
    case qp of
      Nothing -> ""
      Just q -> contentTypeToString' q

contentTypeToString' :: QueryPart -> String
contentTypeToString' (QueryType q) = q
contentTypeToString' _ = ""

queryToString :: Query -> (String, String, String, String)
queryToString q = (query, args, join, contentType) where
   join = joinToString (qJoin q)
   args = argsToString (qArguments q)
   query = termsToString (qSearchTerms q)
   contentType = contentTypeToString (qContentType q)

-- TODO Break query to string up into a function where the joins and args
-- are separated into their own strings, so it is Query ->
-- String,String,String
