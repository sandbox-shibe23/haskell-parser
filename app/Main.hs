module Main where

import Lib

anyChar(x:_) = x


main :: IO ()
main = do
    print $ anyChar "abc"
