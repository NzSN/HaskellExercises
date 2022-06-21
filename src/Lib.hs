module Lib
    ( someFunc
    ) where

import MyMonad.Reader

someFunc :: IO ()
someFunc = print $ runReader compute "Ayden"
