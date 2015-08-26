{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Permission
  ( checkPermission
  , checkDataPermission
  , authAccount
  , checkMemberADMIN
  , checkVerfHeader
  , guardVerfHeader
  ) where

import Control.Monad (void, unless, liftM2)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Foldable as Fold

import Databrary.Has (Has, view, peek, peeks)
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.HTTP.Request
import Databrary.Action

checkPermission :: (MonadAction c m, MonadIO m, Has Permission a) => Permission -> a -> m a
checkPermission p o = do
  unless (view o >= p) $ result =<< peeks forbiddenResponse
  return o

checkDataPermission :: (MonadAction c m, MonadIO m, Has Release a, Has Permission a) => a -> m a
checkDataPermission o = do
  unless (dataPermission o > PermissionNONE) $ result =<< peeks forbiddenResponse
  return o

authAccount :: ActionM Account
authAccount = do
  ident <- peek
  case ident of
    PreIdentified -> fail "authAccount: PreIdentified"
    NotIdentified -> result =<< peeks forbiddenResponse
    Identified s -> return $ view s
    ReIdentified u -> return $ view u

checkMemberADMIN :: ActionM ()
checkMemberADMIN = do
  admin <- peeks accessMember'
  void $ checkPermission PermissionADMIN admin

checkVerfHeader :: (MonadAction q m) => m Bool
checkVerfHeader = do
  header <- peeks $ lookupRequestHeader "x-csverf"
  peeks $ Fold.or . liftM2 (==) header . identityVerf

guardVerfHeader :: ActionM ()
guardVerfHeader = do
  c <- checkVerfHeader
  unless c $ result =<< peeks forbiddenResponse
