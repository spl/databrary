import Control.Monad (when)
import qualified Data.Foldable as Fold
import Distribution.Compat.Environment (getEnvironment)
import Distribution.PackageDescription (PackageDescription(dataDir), FlagName(FlagName))
import Distribution.Simple
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.InstallDirs (toPathTemplate, fromPathTemplate)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExitWithEnv)
import Distribution.Verbosity (Verbosity)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))

import Databrary.Setup.Git
import Databrary.Setup.Node

run :: Verbosity -> PackageDescription -> LocalBuildInfo -> String -> [String] -> IO ()
run verb desc info cmd args = do
  env <- getEnvironment
  cwd <- getCurrentDirectory
  rawSystemExitWithEnv verb (buildDir info </> cmd </> cmd <.> exeExtension) args
    $ (pkgPathEnvVar desc "datadir", cwd </> dataDir desc)
    : (pkgPathEnvVar desc "sysconfdir", cwd)
    : env

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPrograms = [nodeProgram, npmProgram] ++ hookedPrograms simpleUserHooks

  , confHook = \(d, i) f -> do
    d' <- setGitVersion d
    let f' | Fold.or (lookup (FlagName "devel") (configConfigurationsFlags f)) || Fold.any (not . null . fromPathTemplate) (flagToMaybe $ configProgSuffix f) = f
           | otherwise = f{ configProgSuffix = Flag $ toPathTemplate "-$version" }
    confHook simpleUserHooks (d', i) f'

  , postConf = \args flag desc info -> do
    postConf simpleUserHooks args flag desc info
    nodeInstall (fromFlag $ configVerbosity flag) info

  , buildHook = \desc info hooks flag -> do
    let verb = fromFlag $ buildVerbosity flag
    nodeModuleGenerate verb desc info
    let args = buildArgs flag
        build c = buildHook simpleUserHooks desc info hooks flag{ buildArgs = c }
    when (null args) $ do
      build ["schemabrary"]
      run verb desc info "schemabrary" []
    build args

  , postBuild = \args flag desc info -> do
    run (fromFlag $ buildVerbosity flag) desc info "databrary" ["-w"]
    postBuild simpleUserHooks args flag desc info
  }
