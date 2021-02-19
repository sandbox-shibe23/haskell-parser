import Text.Parsec

eval m fs = foldl (\x f -> f x) <$> m <*> fs
apply f m = flip f <$> m

expr = eval term $ many $
      char '+' *> apply (+) term
  <|> char '-' *> apply (-) term

term = eval number $ many $
      char '*' *> apply (*) term
  <|> char '/' *> apply div term

number = read <$> many1 digit

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
