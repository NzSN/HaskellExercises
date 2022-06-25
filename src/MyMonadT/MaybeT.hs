{-# LANGUAGE LambdaCase, InstanceSigs #-}

module MyMonadT.MaybeT
  ( MaybeT'(..),
    lift
   ) where

import Control.Monad.Trans

newtype MaybeT' f a = MaybeT' { runMaybeT' :: f (Maybe a) }
instance Functor f => Functor (MaybeT' f) where
  fmap f (MaybeT' a) = MaybeT' $ (fmap . fmap) f a

instance Applicative f => Applicative (MaybeT' f) where
  pure x = MaybeT' (pure (pure x))
  (MaybeT' f) <*> (MaybeT' a) = MaybeT' $ (<*>) <$> f <*> a

instance Monad f => Monad (MaybeT' f) where
  return :: a -> MaybeT' f a
  return = pure

  (>>=) :: MaybeT' f a -> (a -> MaybeT' f b) -> MaybeT' f b
  (MaybeT' a) >>= f = MaybeT' $ a >>=
    \case
      Nothing -> return Nothing
      Just y -> runMaybeT' (f y)

instance MonadTrans MaybeT' where
  lift = MaybeT' . fmap Just
