module Lib
    ( someFunc
    ) where

import MyMonad.Reader ( compute, Reader(runReader) )
import MyMonad.State ( rollDieThreeTimes, rollDieThreeTimes', infiniteDie )
import Control.Monad.Trans.State ( evalState )
import System.Random ( mkStdGen )

someFunc :: IO ()
someFunc = do
  print $ runReader compute "Ayden"
  print rollDieThreeTimes
  print $ evalState rollDieThreeTimes' (mkStdGen 1)
  print $ take 6 $ evalState infiniteDie (mkStdGen 1)
