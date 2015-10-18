{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Invert
  ( InvertM
  , runInvert
  , give
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Cont (ContT(..))
import Data.IORef (newIORef, readIORef, writeIORef)

data InvertR b
  = InvertCont b (IO (InvertR b))
  | InvertDone

instance Functor InvertR where
  fmap f (InvertCont b c) = InvertCont (f b) (fmap f <$> c)
  fmap _ InvertDone = InvertDone

invertCont :: InvertR b -> IO (InvertR b)
invertCont (InvertCont _ c) = c
invertCont i = return i

invertValue :: InvertR b -> Maybe b
invertValue (InvertCont b _) = Just b
invertValue _ = Nothing

newtype InvertM b a = InvertM { runInvertM :: ContT (InvertR b) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

give :: b -> InvertM b ()
give b = InvertM $ ContT $ \c -> return $ InvertCont b $ c ()

-- |Convert an 'InvertM' action into an IO action that, when called repeatedly, returns 'Just' for each 'give', and 'Nothing' after the action completes.
runInvert :: InvertM b () -> IO (IO (Maybe b))
runInvert m = do
  r <- newIORef (runContT (runInvertM m) (\() -> return InvertDone))
  return $ do
    v <- join $ readIORef r
    writeIORef r $ invertCont v
    return $ invertValue v
