{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc
    ) where

import MyMonad.Reader ( compute, Reader(..) )
import MyMonad.State (
  rollDieThreeTimes,
  rollDieThreeTimes',
  infiniteDie,
  rollsToGetN,
  Moi(..))

import Control.Monad.Trans.State ( evalState )
import System.Random ( mkStdGen )

someFunc :: IO ()
someFunc = do
  print $ runReader compute "Ayden"
  print rollDieThreeTimes
  print $ evalState rollDieThreeTimes' (mkStdGen 1)
  print $ take 6 $ evalState infiniteDie (mkStdGen 1)
  print $ rollsToGetN 40 (mkStdGen 1)

  let a = Moi (1,)
  print $ runMoi a 1
