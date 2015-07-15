module Databrary.Ingest.JSON
  ( ingestJSON
  ) where

import qualified Data.Aeson as JSON

import Databrary.Service.DB
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types

{-
loadSchema :: IO (JS.Schema ())
loadSchema = do
  schema <- getDataFileName "volume.json"
  r <- withBinaryFile schema ReadMode $ \h ->
    P.parseWith (BS.hGetSome h defaultChunkSize) JSON.json' BS.empty
  either fail (jr . JSON.fromJSON) $ P.eitherResult r
  where
  jr (JSON.Error e) = fail e
  jr (JSON.Success r) = return r
-}

ingestJSON :: (MonadDB m) => Volume -> JSON.Value -> Bool -> m [Container]
ingestJSON vol jdata overwrite = do
  -- schema <- liftIO loadSchema
  return []
