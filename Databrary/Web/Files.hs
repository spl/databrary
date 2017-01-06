{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Databrary.Web.Files
  ( allWebFiles
  , findWebFiles
  ) where

import Control.Exception (bracket)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Posix.Directory.ByteString (DirStream)
import qualified System.Posix.ByteString as P
import System.Posix.FilePath (takeExtensions)
import Data.DList (DList)
import qualified Data.DList as DL

import Databrary.Files
import Databrary.Web

-- | Find all* regular files in the given directory and in all subdirectories,
-- recursively.
--
-- * With the exception of files starting with '.'

findAllFilesInDir :: RawFilePath -> IO [RawFilePath]
findAllFilesInDir goalDir = do
  -- The following directory changes allow this function to work for any
  -- (accessible) directory, including '.' and '..'.
  savedFullPath <- P.getWorkingDirectory
  P.changeWorkingDirectory goalDir
  goalFullPath <- P.getWorkingDirectory
  P.changeWorkingDirectory savedFullPath
  DL.toList <$> walkDir goalFullPath
  where
    -- Walk the files in a directory with a DirStream.
    walkDir :: RawFilePath -> IO (DList RawFilePath)
    walkDir d =
      bracket (P.openDirStream d) P.closeDirStream (walkDirStream DL.empty d)
    -- Walk through a DirStream
    walkDirStream
      :: DList RawFilePath
      -> RawFilePath
      -> DirStream
      -> IO (DList RawFilePath)
    walkDirStream accumulatedFiles dir ds = do
      fileName <- P.readDirStream ds
      case B.uncons fileName of
        Nothing ->
          -- An empty file path signifies that there are no more files in the
          -- DirStream.
          return accumulatedFiles
        Just ('.', _) ->
          -- Ignore files that start with '.'.
          walkDirStream accumulatedFiles dir ds
        Just _ -> do
          let filePath = dir </> fileName
          -- For all other files, use the FileStatus.
          fs <- getFileStatus filePath
          files <- if
            | P.isDirectory fs ->
              -- For a directory, walk the subdirectory.
              walkDir filePath
            | P.isRegularFile fs || P.isSymbolicLink fs ->
              -- For a regular file or symbolic link, accumulate.
              return $ DL.singleton filePath
            | otherwise ->
              -- For any other file type, ignore.
              return DL.empty
          walkDirStream (files `DL.append` accumulatedFiles) dir ds

allWebFiles :: IO [WebFilePath]
allWebFiles = map fromRawFilePath <$> findAllFilesInDir webDirRaw

findWebFiles :: ByteString -> IO [WebFilePath]
findWebFiles ext = filter ((ext ==) . takeExtensions . webFileRelRaw) <$> allWebFiles
