module Databrary.Search.Query (
createQuery,
queryToString,
queryLimit,
queryStart,
Query
) where

import System.Environment
import System.IO
import System.IO.Error
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Aeson
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics


-- Some consts
containerType = "container"
recordType = "record"
volumeType = "volume"
assetType = "segment_asset"
tagType = "segment_tag"
segmentType = "segment_record"

queryPrefix = "select?q="
querySuffix = "&wt=json&indent=true"

doctypePrefix = "container_type:"
titlePrefix = "title:"
agePrefix = "age:"

-- Type for all of our documents
data DocType = Container | Record | Volume | AssetSegment | TagSegment | Segment | Unknown
{-data QueryJoin = QueryJoin {queryJoin :: String}-}
{-data QueryArg = QueryArg {queryArg :: String}-}
{-data QuerySearchTerm = QuerySearchTerm {querySearchTerm :: String}-}
data QueryPart = QueryArg String String | QueryJoin String String
                    | QuerySearchTerm String | QueryLimit Int | QueryStart Int deriving Show

data Query = Query {qJoin :: Maybe QueryPart,
                    qArguments :: Maybe [QueryPart],
                    qSearchTerms :: Maybe [QueryPart],
                    qLimit :: Maybe QueryPart,
                    qStart :: Maybe QueryPart} deriving Show

instance Show DocType where
        show Container = containerType
        show Record = recordType
        show Volume = volumeType
        show TagSegment = tagType
        show Segment = segmentType
        show AssetSegment = assetType

selectType :: DocType -> String
selectType Container = doctypePrefix ++ containerType
selectType Record = doctypePrefix ++ recordType
selectType Volume = doctypePrefix ++ volumeType
selectType AssetSegment = doctypePrefix ++ assetType
selectType TagSegment = doctypePrefix ++ tagType
selectType Segment = doctypePrefix ++ segmentType
selectType Unknown = "ERROR: Unknown type"

-- Function for generating age specific parts of the query
selectAge :: Int -> Int -> String
selectAge a b = agePrefix ++ "[" ++ show a ++ "," ++ show b ++ "]"

-- Specify which tables you're joining together
joinTypes :: DocType -> DocType -> String
joinTypes a b = "{!join from=" ++ show a ++ "_" ++ show b ++ "_id_i to=" ++ show b ++ "_id_i} "

stringToType :: String -> DocType
stringToType x
   | x == containerType = Container
   | x == recordType = Record
   | x == volumeType = Volume
   | x == assetType = AssetSegment
   | x == tagType = TagSegment
   | x == segmentType = Segment
   | otherwise = Unknown


-- Join a bunch of query strings together with AND clauses
-- Note: has to be JOIN-MODIFIER ARGUMENT-MODIFIERS SEARCH-TERMS
createQuery :: String -> Query
createQuery x = formQuery $ parseQuery x

formQuery :: [QueryPart] -> Query
formQuery qs = Query j a t limit start
   where
      joinTest =  mapMaybe filterJoin qs
      limitTest = mapMaybe filterLimit qs
      startTest = mapMaybe filterStart qs
      j = if length joinTest >= 1 then Just $ head joinTest else Nothing
      a = Just $ mapMaybe filterArg qs
      t = Just $ concat $ mapMaybe filterTerm qs
      limit = if length limitTest >= 1 then Just $ head limitTest else Nothing
      start = if length startTest >= 1 then Just $ head startTest else Nothing

filterArg :: QueryPart -> Maybe QueryPart
filterArg (QueryArg x y) = Just $ QueryArg x y
filterArg _ = Nothing


filterJoin :: QueryPart -> Maybe QueryPart
filterJoin (QueryJoin x y) = Just $ QueryJoin x y
filterJoin _ = Nothing

--
filterTerm :: QueryPart -> Maybe [QueryPart]
filterTerm (QuerySearchTerm x) = Just $ map QuerySearchTerm $ splitOn " " x
filterTerm _ = Nothing

filterLimit :: QueryPart -> Maybe QueryPart
filterLimit (QueryLimit x) = Just $ QueryLimit x
filterLimit _ = Nothing

filterStart :: QueryPart -> Maybe QueryPart
filterStart (QueryStart x) = Just $ QueryStart x
filterStart _ = Nothing

-- parseQueryString: We're given a query string from somewhere, figure out
-- what to do with it.
-- The format looks like "joinstuff,argstuff,free text stuff"
parseQuery :: String -> [QueryPart]
parseQuery q = map parseQueryPart x
      where
         x = splitOn "|" q

parseQueryPart :: String -> QueryPart
parseQueryPart x
   | isInfixOf "join=" x = parseQueryJoin x
   | isInfixOf "arg=" x = parseQueryArg x
   | isInfixOf "start=" x = parseQueryStart x
   | isInfixOf "rows=" x = parseQueryLimit x
   | otherwise = parseTerm x

-- Joins look like join=type,type and we just want the types
parseQueryJoin :: String -> QueryPart
parseQueryJoin x = QueryJoin arg1 arg2
                     where
                        args = splitOn "," $ concat $ drop 1 $ splitOn "=" x
                        [arg1,arg2] = args

-- Args looks like arg=argname:value
parseQueryArg :: String -> QueryPart
parseQueryArg x = QueryArg argName argVal
                     where
                        args = splitOn ":" $ concat $ drop 1 $ splitOn "=" x
                        [argName,argVal] = args

-- For passing in start/rows parameters to the query
parseQueryLimit :: String -> QueryPart
parseQueryLimit q = QueryLimit (read limit :: Int)
                     where
                        args = splitOn "=" q
                        [_, limit] = args

parseQueryStart :: String -> QueryPart
parseQueryStart q = QueryStart (read limit :: Int)
                     where
                        args = splitOn "=" q
                        [_, limit] = args

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
joinToString' (QueryJoin j1 j2) = "{!join from=" ++ j1 ++ " to=" ++  j2 ++ "} "
joinToString' _ = ""

argsToString :: Maybe [QueryPart] -> String
{-argsToString qp = map argsToString <$> qp-}
argsToString qp = do
      case qp of
         Nothing -> ""
         Just q -> intercalate " " (map argsToString' q)

argsToString' :: QueryPart -> String
argsToString' (QueryArg a1 a2) = a1 ++ ":" ++ a2 ++ " "
argsToString' _ = ""

termsToString :: Maybe [QueryPart] -> String
termsToString qp = do
      case qp of
         Nothing -> ""
         Just q -> intercalate " AND " (map termsToString' q)

termsToString' :: QueryPart -> String
termsToString' (QuerySearchTerm t1) = t1
termsToString' _ = ""

queryToString :: Query -> (String, String, String)
queryToString q = (query, args, join) where
   join = joinToString (qJoin q)
   args = argsToString (qArguments q)
   query = termsToString (qSearchTerms q)

-- TODO Break query to string up into a function where the joins and args
-- are separated into their own strings, so it is Query ->
-- String,String,String

queryLimit :: Query -> Int
queryLimit q = queryPartInt' $ fromMaybe (QueryLimit 10) (qLimit q)

queryPartInt' :: QueryPart -> Int
queryPartInt' (QueryLimit l) = l
queryPartInt' (QueryStart s) = s
queryPartInt' _ = 0

queryStart :: Query -> Int
queryStart q = queryPartInt' $ fromMaybe (QueryStart 0) (qStart q)
