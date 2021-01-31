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

test1 = sequence [anyChar, anyChar]
test2 = (++) <$> test1 <*> sequence [anyChar]
test3 = sequence [letter, digit, digit]

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
