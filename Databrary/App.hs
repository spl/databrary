{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Databrary.App 
  ( AppT
  , AppM
  , runApp
  ) where

import Databrary.Resource

type AppT = ResourceT
type AppM = AppT IO

runApp :: AppT m a -> Resource -> m a
runApp = runResource
