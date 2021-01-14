module Main where

import Lib

anyChar(x:xs) = (x, xs)

test1 xs0 =
  let (x1, xs1) = anyChar xs0
      (x2, xs2) = anyChar xs1
  in [x1, x2]


main :: IO ()
main = do
  print $ anyChar "abt"
