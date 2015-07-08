module Databrary.Ingest.JSON
  ( ingestJSON
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import System.IO (withBinaryFile, IOMode(ReadMode))

import Paths_databrary (getDataFileName)
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

ingestJSON :: (MonadIO m, MonadDB m) => Volume -> JSON.Value -> Bool -> m [Container]
ingestJSON vol jdata overwrite = do
  -- schema <- liftIO loadSchema
  return []
