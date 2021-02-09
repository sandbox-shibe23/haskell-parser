import Text.Parsec

number = do
  x <-  many1 digit
  return (read x :: Int)

main = do
    parseTest number "123"
