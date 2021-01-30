module Main where

import Lib
import Control.Exception
import Control.Monad.State
import Data.Char

parseTest p s = do
    print $ evalState p s
    `catch` \(SomeException e) ->
      putStr $ show e

anyChar = state anyChar where
  anyChar (x:xs) = (x, xs)

satisfy :: (Char -> Bool) -> State String Char
satisfy f = state satisfy where
  satisfy (x:xs) | f x = (x, xs)

char c = satisfy (== c)
digit = satisfy isDigit
letter = satisfy isLetter

test1 = do
  x1 <- anyChar
  x2 <- anyChar
  return [x1, x2]

test2 = do
  x1 <- test1
  x2 <- anyChar
  return $ x1 ++ [x2]

test3 = do
  x1 <- letter
  x2 <- digit
  x3 <- digit
  return [x1, x2, x3]

main :: IO ()
main = do
    parseTest anyChar "abc"
    parseTest test1   "abc"
    parseTest test2   "abc"
    parseTest test2   "12"      -- NG
    parseTest test2   "123"
    parseTest (char 'a') "abc"
    parseTest (char 'a') "123"  -- NG
    parseTest digit  "abc"      -- NG
    parseTest digit  "123"
    parseTest letter "abc"
    parseTest letter "123"      -- NG
    parseTest test3  "abc"      -- NG
    parseTest test3  "123"      -- NG
    parseTest test3  "a23"
    parseTest test3  "a234"
