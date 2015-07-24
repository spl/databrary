{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Probe
  ( Probe(..)
  , probeLength
  , probeFile
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((+++))
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Posix.FilePath (takeExtension)

import Databrary.Has (MonadHas, focusIO)
import Databrary.Files
import Databrary.Model.Format
import Databrary.Model.Offset
import Databrary.Store.AV

data Probe
  = ProbePlain
    { probeFormat :: Format }
  | ProbeVideo
    { probeFormat :: Format
    , probeAV :: AVProbe
    }

probeLength :: Probe -> Maybe Offset
probeLength (ProbeVideo _ av) = avProbeLength av
probeLength _ = Nothing

probeFile :: (MonadIO m, MonadHas AV c m) => BS.ByteString -> RawFilePath -> m (Either T.Text Probe)
probeFile n f = maybe 
  (return $ Left $ "unknown or unsupported format: " <> TE.decodeLatin1 (takeExtension n))
  (\fmt -> case formatTranscodable fmt of
    Nothing -> return $ Right $ ProbePlain fmt
    Just t
      | t == videoFormat -> do
        (("could not process unsupported or corrupt video file: " <>) . T.pack . avErrorString +++ ProbeVideo fmt)
          <$> focusIO (try . avProbe f)
      | otherwise -> fail "unhandled format conversion")
  $ getFormatByFilename n
