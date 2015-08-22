{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Probe
  ( Probe(..)
  , probeLength
  , probeFile
  , probeAutoPosition
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((+++))
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (diffDays)
import Data.Time.LocalTime (ZonedTime(..), LocalTime(..), timeOfDayToTime)
import System.Posix.FilePath (takeExtension)

import Databrary.Has (MonadHas, focusIO)
import Databrary.Files
import Databrary.Service.DB
import Databrary.Model.Format
import Databrary.Model.Offset
import Databrary.Model.Container.Types
import Databrary.Model.AssetSlot
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

probeAutoPosition :: MonadDB m => Container -> Probe -> m Offset
probeAutoPosition Container{ containerDate = Just d } ProbeVideo{ probeAV = AVProbe{ avProbeDate = Just (ZonedTime (LocalTime d' t) _) } }
  | dd >= -1 && dd <= 1 && dt >= negate day2 && dt <= 3*day2 = return $ diffTimeOffset dt where
  dd = diffDays d' d
  dt = (fromInteger dd)*day + timeOfDayToTime t
  day2 = 43200
  day = 2*day2
probeAutoPosition c _ = findAssetContainerEnd c

