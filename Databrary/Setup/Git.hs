module Databrary.Setup.Git
  ( setGitVersion
  ) where

import Data.Version (parseVersion)
import Distribution.Simple
import Distribution.PackageDescription
import System.IO.Error (catchIOError)
import System.Process (readProcess)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readMaybe)

gitDescribe :: IO (Maybe Version)
gitDescribe = do
  s <- readProcess "git" ["describe", "--long", "--dirty", "--match", "v[0-9]*"] ""
    `catchIOError` \_ -> return ""
  return $ case [ v | (v, "\n") <- ReadP.readP_to_S (ReadP.char 'v' >> parseVersion) s ] of
    [v@(Version n (c:t))] -> Just $ maybe v (\m -> Version (n ++ replicate (2 - length n) 0 ++ [m]) t) $ readMaybe c
    _ -> Nothing

setGitVersion :: GenericPackageDescription -> IO GenericPackageDescription
setGitVersion g = maybe g s <$> gitDescribe where
  s v = g{ packageDescription = d{ package = p{ pkgVersion = v } } }
  d = packageDescription g
  p = package d

