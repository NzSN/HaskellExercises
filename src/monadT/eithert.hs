{-# LANGUAGE LambdaCase #-}

newtype EitherT' e m a = EitherT' { runEitherT' :: m (Either e a) }

instance Functor m => Functor (EitherT' e m) where
  fmap f (EitherT' a) = EitherT' $ (fmap . fmap) f a

instance Applicative m => Applicative (EitherT' e m) where
  pure x = EitherT' $ pure $ pure x
  (EitherT' f) <*> (EitherT' a) = EitherT' $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT' e m) where
  return = pure
  (EitherT' a) >>= f = EitherT' $ a >>= \(Right x) -> runEitherT' (f x)

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT' :: (Functor m)
             => EitherT' e m a
             -> EitherT' a m e
swapEitherT' (EitherT' a) = EitherT' $ fmap swapEither a

eitherT' :: Monad m =>
            (a -> m c)
         -> (b -> m c )
         -> EitherT' a m b
         -> m c
eitherT' fl fr (EitherT' a) = a >>=
  \case
    Left x  -> fl x
    Right y -> fr y
