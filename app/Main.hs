import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.State
import Data.Char

parseTest p s = case evalStateT p s of
    Right r -> print r
    Left  e -> putStrLn $ "[" ++ show s ++ "]" ++ e

anyChar = StateT $ anyChar where
  anyChar (x:xs) = Right(x, xs)
  anyChar _      = Left "too short"

satisfy f = StateT $ satisfy where
  satisfy (x:xs) | not $ f x = Left $ "; " ++ show x
  satisfy xs                 = runStateT anyChar xs

(StateT a) <|> (StateT b) = StateT $ \s ->
    (a s) <|> (b s) where
    Left a <|> Left b = Left $ b ++ a
    Left _ <|> b      = b
    a      <|> _      = a

left = lift . Left

char c = satisfy (== c)   <|> left ("not char" ++ show c)
digit  = satisfy isDigit  <|> left "not digit"
letter = satisfy isLetter <|> left "not letter"

many p = ((:) <$> p <*> many p) <|> return [] 

test1 = sequence [anyChar, anyChar]
test2 = (++) <$> test1 <*> sequence [anyChar]
test3 = sequence [letter, digit, digit]
test4 = letter <|> digit
test5 = sequence [letter, digit, digit, digit]
test6 = sequence $ letter : replicate 3 digit
test7 = many letter
test8 = many (letter <|> digit)

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
    parseTest test4  "a"
    parseTest test4  "1"
    parseTest test4  "!"        -- NG
    parseTest test5  "a123"
    parseTest test5  "aa23"     -- NG
    parseTest test6  "a123"
    parseTest test6  "aa23"     -- NG
    parseTest test7 "abc123"
    parseTest test7 "123abc"
    parseTest test8 "abc123"
    parseTest test8 "123abc"