{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.JSHint
  ( checkJSHint
  ) where

import Control.Applicative ((<$>))
import Control.Monad (mzero, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import System.FilePath (takeExtensions)
import System.Posix.FilePath (splitFileName, addExtension)
import System.Posix.IO.ByteString (openFd, OpenMode(WriteOnly), defaultFileFlags, closeFd)
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

checkJSHint :: WebGenerator
checkJSHint fo@(f, _)
  | takeExtensions (webFileRel f) == ".js" = do
    r <- fileNewer f fo
    when r $ liftIO $ do
      ht <- fmap snd <$> fileInfo h
      ft <- modificationTimestamp <$> getFileStatus f
      when (Fold.all (ft >) ht) $ do
        openFd h WriteOnly (Just 0o666) defaultFileFlags >>= closeFd -- touch
        callProcess (binDir </> "jshint") [webFileAbs f]
    return r
  | otherwise = mzero
  where
  (d, n) = splitFileName $ webFileAbsRaw f
  h = d </> ('.' `BSC.cons` n `addExtension` ".hinted")
