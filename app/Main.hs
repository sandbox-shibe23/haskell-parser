import Text.Parsec

expr = do
  x <- number
  fs <- many $ do
    char '+'
    y <- number
    return (+ y)
    <|> do
      char '-'
      y <- number
      return $ subtract y
    <|> do
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