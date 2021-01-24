module Main where

import Lib
import Control.Exception
import Data.Char (isDigit)

parseTest p s = do
    print $ fst $ p s
    `catch` \(SomeException e) ->
      putStr $ show e

anyChar(x:xs) = (x, xs)
satisfy f (x:xs) | f x = (x, xs)

main :: IO ()
main = do
  parseTest (satisfy (=='a')) "abt"
  parseTest (satisfy (=='a')) "123"
  parseTest (satisfy isDigit) "abt"
  parseTest (satisfy isDigit) "123"
