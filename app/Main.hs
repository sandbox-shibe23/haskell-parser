import Text.Parsec

expr = do
  x <- term
  fs <- many $ do
      char '+'
      y <- term
      return (+ y)
    <|> do
      char '-'
      y <- term
      return $ subtract y
  return $ foldl (\x f -> f x) x fs

term = do
  x <- number
  fs <- many $ do
    char '*'
    y <- number
    return $ (* y)
    <|> do
      char '/'
      y <- number
      return $ (`div` y)
  return $ foldl (\x f -> f x) x fs

number = do
  x <-  many1 digit
  return (read x :: Int)

test1 = do
    x <- letter
    digit
    return x

test2 = letter <* digit

main = do
    parseTest number "123"
    parseTest expr "1+2"
    parseTest expr "123"
    parseTest expr "1+2+3"
    parseTest expr "1-2-3"
    parseTest expr "1-2+3"
    parseTest expr "2*3+4"           -- OK
    parseTest expr "2+3*4"           -- NG
    parseTest expr "100/10/2"        -- OK
    parseTest test1 "a1"
    parseTest test2 "a1"
