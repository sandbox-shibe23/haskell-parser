module Main where

import Lib
import Control.Exception
import Data.Char (isDigit, isLetter)

parseTest p s = do
    print $ fst $ p s
    `catch` \(SomeException e) ->
      putStr $ show e

anyChar(x:xs) = (x, xs)
satisfy f (x:xs) | f x = (x, xs)

char c = satisfy (== c)
digit = satisfy isDigit
letter = satisfy isLetter

test3 xs0 =
  let (x1, xs1) = letter xs0
      (x2, xs2) = digit xs1
      (x3, xs3) = digit xs2
  in ([x1, x2, x3], xs3)

main :: IO ()
main = do
  parseTest test3 "abc"
  parseTest test3 "123"
  parseTest test3 "a23"
  parseTest test3 "a234"
