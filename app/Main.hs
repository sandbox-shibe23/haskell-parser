import Text.Parsec

expr = do
  x <- number
  xs <- many $ do
    char '+'
    number
  return $ x:xs

number = do
  x <-  many1 digit
  return (read x :: Int)

main = do
    parseTest number "123"
    parseTest expr "1+2"
    parseTest expr "123"
    parseTest expr "1+2+3"
