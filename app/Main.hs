module Main where

import Lib

anyChar(x:xs) = (x, xs)


main :: IO ()
main = do
    let r1 = anyChar "abc"
    let r2 = anyChar $ snd r1
    print r1
    print r2
