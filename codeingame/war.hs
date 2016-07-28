import Text.Parsec.Char

play [] _ xbs ybs = 1
play _ [] xbs ybs = 0
play xst@(x:xs) yst@(y:ys) xbs ybs  
    | x == y && (length xs < 3 || length ys < 3)  =  2
    | x == y = play (drop 4 xst) (drop 4 yst) (xbs ++ take 4 xst) (ybs ++ take 4 yst)
    | x < y = play xs (ys ++ (y:ybs) ++ (x:xbs)) [] []
    | otherwise = play (xs ++ (x:xbs) ++ (y:ybs)) ys [] []

parse = many1 digit <|> (char 'J' >> return 11)<|> (char 'Q' >> return 12)<|> (char 'K' >> return 13)

