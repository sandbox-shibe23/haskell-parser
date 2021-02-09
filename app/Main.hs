import Text.Parsec

number = many1 digit

main = do
    parseTest number "123"
