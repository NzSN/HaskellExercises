
module MyMonadT.ReaderT
  (Reader',
   ask
  ) where

import Control.Applicative

boop :: Integer -> Integer
boop = (*2)

doop :: Integer -> Integer
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop


bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

(<||>) :: (a -> Bool)
       -> (a -> Bool)
       -> a
       -> Bool
(<||>) = liftA2 (||)

newtype Reader' r a = Reader' { runReader' :: r -> a }

ask :: Reader' a a
ask = Reader' id
