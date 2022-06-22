module Lib
    ( someFunc
    ) where

import MyMonad.Reader
import MyMonad.State
import Control.Monad.Trans.State
import System.Random

someFunc :: IO ()
someFunc = do
  print $ runReader compute "Ayden"
  print rollDieThreeTimes
  print $ evalState rollDieThreeTimes' (mkStdGen 1)
