{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module MyMonad.State
  ( rollDieThreeTimes,
    rollDieThreeTimes',
    infiniteDie,
    rollsToGetN,
    Moi(..),
    fizzBuzzMain,
    get',
    put',
    modify'
  ) where


import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, state, put, get, execState)
import System.Random ( mkStdGen, Random(randomR), StdGen )


data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie go non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let sg = mkStdGen 1
      (d1, s1) = randomR (1, 6) sg
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = step 0 0
  where
    step :: Int -> Int -> StdGen -> Int
    step sum count gen
      | sum > 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in step (sum + die) (count + 1) nextGen

-- Exercises
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit = step 0 0
  where
    step sum count gen
      | sum > limit = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in step (sum + die) (count + 1) nextGen

    step :: Int -> Int -> StdGen -> Int

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged = undefined


-- Write Your Own State
newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi a) = Moi $ fmap ((,) <$> (f . fst) <*> snd) a

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (a,)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi g) <*> (Moi a) = Moi $
    \s -> let (f, s') = g s
              (a', s'') = a s'
          in (f a', s'')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi g) >>= f = Moi $ \s -> let (a, s') = g s in runMoi (f a) s'

get' :: Moi s s
get' = Moi $ \x -> (x, x)

put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

exec :: Moi s a -> s -> s
exec a = snd. runMoi a

eval :: Moi s a -> s -> a
eval a = fst . runMoi a

modify' :: (s -> s) -> Moi s ()
modify' f = Moi $ \s -> ((), f s)

-- FizzBuzz
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzBuzzMain :: IO ()
fizzBuzzMain = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
