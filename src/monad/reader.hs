{-# LANGUAGE InstanceSigs #-}

newtype Reader a b = Reader { runReader :: a -> b }

instance Functor (Reader a) where
  fmap f (Reader a) = Reader (f . a)

instance Applicative (Reader a) where
  pure a = Reader (const a)

  (<*>) :: Reader a (b -> c) -> Reader a b -> Reader a c
  (Reader f) <*> (Reader a) =
    -- f :: (a -> b -> c)
    -- a :: (a -> b)
    Reader $ f <*> a

instance Monad (Reader a) where
  return = pure

  (>>=) :: Reader a b -> (b -> Reader a c) -> Reader a c
  (Reader a) >>= f = Reader $ a >>= runReader . f


ask :: Reader a a
ask = Reader id

compute :: Reader String Int
compute = do
  name <- ask
  return 1

main :: IO ()
main = do
  print $ runReader compute "Ayden"
