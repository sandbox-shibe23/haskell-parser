module Main where

import Lib
import Control.Exception

parseTest p s = do
    print $ fst $ p s
    `catch` \(SomeException e) ->
      putStr $ show e

anyChar(x:xs) = (x, xs)

test1 xs0 =
  let (x1, xs1) = anyChar xs0
      (x2, xs2) = anyChar xs1
  in ([x1, x2], xs2)

test2 xs0 =
  let (x1, xs1) = test1 xs0
      (x2, xs2) = anyChar xs1
  in (x1 ++ [x2], xs2)

main :: IO ()
main = do
  parseTest anyChar "abt"
  parseTest test1 "abt"
  parseTest test2 "abt"
  parseTest test2 "ab"
  parseTest test2 "123"
