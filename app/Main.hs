import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.State
import Data.Char

parseTest p s = case evalStateT p s of
    Right r     -> print r
    Left (e, _) -> putStrLn $ "[" ++ show s ++ "] " ++ e

anyChar = StateT $ anyChar where
    anyChar (x:xs) = Right (x, xs)
    anyChar    xs  = Left ("too short", xs)

satisfy f = StateT $ satisfy where
    satisfy (x:xs) | not $ f x = Left (": " ++ show x, x:xs)
    satisfy    xs              = runStateT anyChar xs

(StateT a) <|> (StateT b) = StateT f where
    f s0 =   (a  s0) <|> (b  s0) where
        Left (a, s1) <|> _ | s0 /= s1 = Left (     a, s1)
        Left (a, _ ) <|> Left (b, s2) = Left (b ++ a, s2)
        Left _       <|> b            = b
        a            <|> _            = a

try (StateT p) = StateT $ \s -> case p s of
  Left (e, _) -> Left (e, s)
  r           -> r

left e = StateT $ \s -> Left (e, s)

char c = satisfy (== c)   <|> left ("not char " ++ show c)
digit  = satisfy isDigit  <|> left "not digit"
letter = satisfy isLetter <|> left "not letter"

string s = sequence [char x | x <- s]

many p = ((:) <$> p <*> many p) <|> return []

test9 = sequence [char 'a', char 'b']
    <|> sequence [char 'a', char 'c']

test10 = try (sequence [char 'a', char 'b'])
         <|>  sequence [char 'a', char 'c']
test11 =      string "ab"  <|> string "ac"
test12 = try (string "ab") <|> string "ac"

main = do
    parseTest test9 "ab"
    parseTest test9 "ac"
    parseTest test10 "ab"
    parseTest test10 "ac"
    parseTest test11 "ab"
    parseTest test11 "ac"
    parseTest test12 "ab"
    parseTest test12 "ac"
