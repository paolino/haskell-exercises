module Roman where

import Text.ParserCombinators.Poly.Plain

solution :: String -> Int
solution = either error id . fst . runParser parseRoman . (++ "\n")

char = satisfy . (==)

lm :: Parser Char Char -> Parser Char Int
lm  = fmap length . many1 

parseG :: Char -> Char -> Char -> Parser Char Int
parseG k m d =  
  (char m >> char k >> return 9) <|>
  (char m >> char d >> return 4) <|>
  lm (char m) <|> 
  (char d >> ((+5) <$> lm (char m))) <|> 
  (char d >> return 5) <|> 
  return 0

parse1 = parseG 'X' 'I' 'V' 
parse10 = (*10) <$> parseG 'C' 'X' 'L' 
parse100 = (*100) <$> parseG 'M' 'C' 'D' 
parse1000 = (*1000) <$> lm (char 'M') <|> return 0

parseRoman :: Parser Char Int
parseRoman = sum <$> sequence [parse1000,parse100,parse10,parse1]
