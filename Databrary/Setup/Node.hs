module Databrary.Setup.Node
  ( nodeProgram
  , npmProgram
  , nodeUpdate
  , nodeModuleGenerate
  ) where

import Data.Char (isSpace)
import Data.List (intercalate, dropWhileEnd)
import qualified Data.Map as Map
import qualified Distribution.ModuleName as Mod
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.BuildPaths (autogenModulesDir, autogenModuleName)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, withPrograms)
import Distribution.Simple.Program
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile, rawSystemExit)
import Distribution.Text (display)
import Distribution.Verbosity (Verbosity)
import System.FilePath ((</>), (<.>), takeDirectory)

nodeProgram :: Program
nodeProgram = (simpleProgram "node")
  { programFindVersion = findProgramVersion "-v" ver
  } where
  ver ('v':s) = s
  ver _ = ""

npmProgram :: Program
npmProgram = (simpleProgram "npm")
  { programFindVersion = findProgramVersion "-v" id
  , programPostConf = \v p -> do
    bin <- dropWhileEnd isSpace `fmap` rawSystemProgramStdout v p ["bin"]
    return p
      { programProperties = Map.insert "bin" bin $ programProperties p
      }
  }

npmBin :: LocalBuildInfo -> (ConfiguredProgram, FilePath)
npmBin info = (npm, programProperties npm Map.! "bin") where
  Just npm = lookupProgram npmProgram (withPrograms info)

nodeUpdate :: Verbosity -> LocalBuildInfo -> IO ()
nodeUpdate verb info = do
  rawSystemProgram verb npm ["update"]
  rawSystemExit verb (bin </> "bower") ["update"]
  where
  (npm, bin) = npmBin info

nodeModuleGenerate :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
nodeModuleGenerate verb desc info = do
  createDirectoryIfMissingVerbose verb True $ takeDirectory file
  rewriteFile file
    $ "module " ++ display modn ++ "(binDir) where\n\
    \binDir :: FilePath\n\
    \binDir = " ++ show bin ++ "\n"
  where
  file = autogenModulesDir info </> Mod.toFilePath modn <.> "hs"
  modn = Mod.fromString $ intercalate "." $ Mod.components (autogenModuleName desc) ++ ["Node"]
  (_, bin) = npmBin info
