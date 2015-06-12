module Databrary.Web.JSHint
  ( checkJSHint
  ) where

import Control.Monad (mzero, when)
import Control.Monad.IO.Class (liftIO)
import System.FilePath (takeExtensions)
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
    when r $
      liftIO $ callProcess (binDir </> "jshint") [webFileAbs f]
    return r
  | otherwise = mzero
