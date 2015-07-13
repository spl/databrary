{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Funding
  ( queryFunder
  , postVolumeFunding
  , deleteVolumeFunder
  ) where

import Control.Monad (liftM2)
import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE))

import Databrary.Ops
import Databrary.Has (focusIO)
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Funding
import Databrary.Model.Funding.FundRef
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Permission
import Databrary.Controller.Volume

queryFunder :: AppRoute ()
queryFunder = action GET (pathJSON </< "funder") $ \() -> withAuth $ do
  _ <- authAccount
  (q, a) <- runForm Nothing $ liftM2 (,)
    ("query" .:> (deformRequired =<< deform))
    ("all" .:> deform)
  r <- if a
    then focusIO $ searchFundRef q
    else findFunders q
  okResponse [] $ JSON.toJSON $ map funderJSON r

postVolumeFunding :: AppRoute (Id Volume, Id Funder)
postVolumeFunding = action POST (pathJSON >/> pathId </> pathId) $ \(vi, fi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  f <- maybeAction =<< lookupFunderRef fi
  a <- runForm Nothing $ do
    csrfForm
    "awards" .:> filter (not . T.null) <$> withSubDeforms deform
  let fa = Funding f a
  _ <- changeVolumeFunding v fa
  okResponse [] $ fundingJSON fa

deleteVolumeFunder :: AppRoute (Id Volume, Id Funder)
deleteVolumeFunder = action DELETE (pathJSON >/> pathId </> pathId) $ \(vi, fi) -> withAuth $ do
  guardVerfHeader
  v <- getVolume PermissionEDIT vi
  _ <- removeVolumeFunder v fi
  okResponse [] $ volumeJSON v
