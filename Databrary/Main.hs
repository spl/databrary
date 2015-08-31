{-# LANGUAGE CPP, OverloadedStrings #-}
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (void)
#ifndef DEVEL
import Control.Monad.Reader (runReaderT)
#endif
import qualified Data.Aeson.Encode as J (encodeToBuilder)
import Data.ByteString.Builder (hPutBuilder)
import qualified System.Console.GetOpt as Opt
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stdout)

#ifndef DEVEL
import Paths_databrary (getDataFileName)
import Databrary.Service.Types (serviceDB)
import Databrary.Service.DB (liftDBM)
import Databrary.Service.DB.Schema (updateDBSchema)
#endif
import Databrary.Service.Init (loadConfig, withService)
import Databrary.Service.Periodic (forkPeriodic)
import Databrary.Web.Rules (generateWebFiles)
import Databrary.Action (runActionRoute)
import Databrary.Routes (routeMap)
import Databrary.Routes.API (swagger)
import Databrary.Warp (runWarp)

data Flag
  = FlagWeb
  | FlagAPI
  deriving (Eq)

opts :: [Opt.OptDescr Flag]
opts =
  [ Opt.Option "w" ["webgen"] (Opt.NoArg FlagWeb) "Generate web assets only"
  , Opt.Option "a" ["api"] (Opt.NoArg FlagAPI) "Output Swagger API documention"
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case Opt.getOpt Opt.Permute opts args of
    ([FlagWeb], [], []) -> do
      void generateWebFiles
      exitSuccess
    ([FlagAPI], [], []) -> do
      hPutBuilder stdout $ J.encodeToBuilder swagger
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
    runReaderT (liftDBM $ updateDBSchema schema) (serviceDB rc)
#endif
    void $ forkPeriodic rc
    runWarp conf rc (runActionRoute routes rc)
