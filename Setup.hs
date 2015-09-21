import Control.Monad (when)
import qualified Data.Foldable as Fold
import Distribution.Compat.Environment (getEnvironment)
import Distribution.PackageDescription (PackageDescription(dataDir), FlagName(FlagName))
import Distribution.Simple
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.InstallDirs (toPathTemplate, fromPathTemplate, datadir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir), absoluteInstallDirs)
import Distribution.Simple.Setup
import Distribution.Simple.Utils (notice, rawSystemExitWithEnv, setFileExecutable)
import Distribution.Verbosity (Verbosity)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))

import Databrary.Setup.Git
import Databrary.Setup.Node

run :: Verbosity -> PackageDescription -> LocalBuildInfo -> String -> [String] -> IO ()
run verb desc lbi cmd args = do
  env <- getEnvironment
  cwd <- getCurrentDirectory
  rawSystemExitWithEnv verb (buildDir lbi </> cmd </> cmd <.> exeExtension) args
    $ (pkgPathEnvVar desc "datadir", cwd </> dataDir desc)
    : (pkgPathEnvVar desc "sysconfdir", cwd)
    : env

fixPerms :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
fixPerms desc lbi copy = do
  setFileExecutable (dir </> "transctl.sh")
  setFileExecutable (dir </> "transcode")
  where dir = datadir $ absoluteInstallDirs desc lbi copy

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPrograms = [nodeProgram, npmProgram] ++ hookedPrograms simpleUserHooks

  , confHook = \(d, i) f -> do
    d' <- setGitVersion d
    let f' | Fold.or (lookup (FlagName "devel") (configConfigurationsFlags f)) || Fold.any (not . null . fromPathTemplate) (flagToMaybe $ configProgSuffix f) = f
           | otherwise = f{ configProgSuffix = Flag $ toPathTemplate "-$version" }
    confHook simpleUserHooks (d', i) f'

  , postConf = \args flag desc lbi -> do
    postConf simpleUserHooks args flag desc lbi
    nodeUpdate (fromFlag $ configVerbosity flag) lbi

  , buildHook = \desc lbi hooks flag -> do
    let verb = fromFlag $ buildVerbosity flag
    nodeModuleGenerate verb desc lbi
    let args = buildArgs flag
        build c = buildHook simpleUserHooks desc lbi hooks flag{ buildArgs = c }
    when (null args) $ do
      build ["schemabrary"]
      run verb desc lbi "schemabrary" []
    build args

  , postBuild = \args flag desc lbi -> do
    let verb = fromFlag $ buildVerbosity flag
    notice verb "Generating web ..."
    run verb desc lbi "databrary" ["-w"]
    postBuild simpleUserHooks args flag desc lbi

  , postCopy = \args flag desc lbi -> do
    fixPerms desc lbi (fromFlag $ copyDest flag)
    postCopy simpleUserHooks args flag desc lbi
  , postInst = \args flag desc lbi -> do
    fixPerms desc lbi NoCopyDest
    postInst simpleUserHooks args flag desc lbi
  }
