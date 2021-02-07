import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.State
import Data.Char

parseTest p s = case evalStateT p s of
    Right r -> print r
    Left  e -> putStrLn $ "[" ++ show s ++ "] " ++ e

anyChar = StateT $ anyChar where
    anyChar (x:xs) = Right (x, xs)
    anyChar _      = Left "too short"

satisfy f = StateT $ satisfy where
    satisfy (x:xs) | not $ f x = Left $ ": " ++ show x
    satisfy    xs              = runStateT anyChar xs

(StateT a) <|> (StateT b) = StateT $ \s ->
    (a s)  <|> (b s) where
    Left a <|> Left b = Left $ b ++ a
    Left _ <|> b      = b
    a      <|> _      = a

left = lift . Left

char c = satisfy (== c)   <|> left ("not char " ++ show c)
digit  = satisfy isDigit  <|> left "not digit"
letter = satisfy isLetter <|> left "not letter"

many p = ((:) <$> p <*> many p) <|> return []

test7 = many letter
test8 = many (letter <|> digit)

main = do
    parseTest test7 "abc123"
    parseTest test7 "123abc"
    parseTest test8 "abc123"
    parseTest test8 "123abc"
