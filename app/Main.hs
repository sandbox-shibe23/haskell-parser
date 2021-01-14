module Main where

import Lib

anyChar(x:xs) = (x, xs)


main :: IO ()
main = do
    let (x1, xs1) = anyChar "abc"
    let (x2, xs2) = anyChar xs1
    print [x1, x2]
