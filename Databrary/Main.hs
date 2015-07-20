{-# LANGUAGE CPP, OverloadedStrings #-}
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (void)
#ifndef DEVEL
import Control.Monad.Reader (runReaderT)
#endif
import qualified System.Console.GetOpt as Opt
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)

#ifndef DEVEL
import Paths_databrary (getDataFileName)
import Databrary.Service.Types (serviceDB)
import Databrary.Service.DB.Schema (updateDBSchema)
#endif
import Databrary.Service.Init (loadConfig, withService)
import Databrary.Service.Periodic (forkPeriodic)
import Databrary.Web.Rules (generateWebFiles)
import Databrary.Action (runAppRoute)
import Databrary.Routes (routeMap)
import Databrary.Warp (runWarp)

data Flag
  = FlagWeb
  deriving (Eq)

opts :: [Opt.OptDescr Flag]
opts =
  [ Opt.Option "w" ["webgen"] (Opt.NoArg FlagWeb) "Generate web assets only"
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case Opt.getOpt Opt.Permute opts args of
    ([FlagWeb], [], []) -> do
      void generateWebFiles
      exitSuccess
    ([], [], []) -> return ()
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...]") opts
      exitFailure

  routes <- evaluate routeMap
  conf <- loadConfig
  withService conf $ \rc -> do
#ifndef DEVEL
    schema <- getDataFileName "schema"
    runReaderT (updateDBSchema schema) (serviceDB rc)
#endif
    void $ forkPeriodic rc
    runWarp conf rc (runAppRoute routes rc)
