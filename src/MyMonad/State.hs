{-# LANGUAGE InstanceSigs #-}

module MyMonad.State
  ( State',
    rollDieThreeTimes,
    rollDieThreeTimes',
    infiniteDie,
    rollsToGetN
  ) where


import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, state)
import System.Random ( mkStdGen, Random(randomR), StdGen )


newtype State' s a =
  State' { runState :: s -> (a, s) }

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
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi a) = undefined
