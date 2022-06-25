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
  Moi(..),
  fizzBuzzMain)

import MyMonadT.MaybeT (MaybeT'(..), lift)

import Control.Monad.Trans.State ( evalState )
import Control.Monad.Trans.Reader ( ReaderT(..), ask )
import System.Random ( mkStdGen )

someFunc :: IO ()
someFunc = do
  print rollDieThreeTimes
  print $ evalState rollDieThreeTimes' (mkStdGen 1)
  print $ take 6 $ evalState infiniteDie (mkStdGen 1)
  print $ rollsToGetN 40 (mkStdGen 1)

  let a = Moi (1,)
  print $ runMoi a 1

  fizzBuzzMain

  a <- runMaybeT' doMaybeThing
  print a

  b <- runMaybeT' $ runReaderT tryStackMore 1
  print b

  -- Try MaybeT

  where
    doMaybeThing :: MaybeT' IO String
    doMaybeThing = do
      lift $ putStrLn "Hi maybe yet"
      return "Hi"

    tryStackMore :: ReaderT Integer (MaybeT' IO) String
    tryStackMore = do
      x <- ask

      lift . lift . print $ "Deep in stack"

      if x == 1
        then return "This is 1"
        else return "Not 1"
