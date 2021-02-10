import Text.Parsec

expr = do
  x <- number
  char '+'
  y <- number
  return [x, y]

number = do
  x <-  many1 digit
  return (read x :: Int)

main = do
    parseTest number "123"
    parseTest expr "1+2"
