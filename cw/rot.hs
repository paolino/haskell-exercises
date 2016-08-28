import Data.List


splitN :: Int -> [a] -> [[a]]
splitN n = unfoldr f where
	f [] = Nothing
	f xs = Just $ splitAt n xs
  
revRot :: [Char] -> Int -> [Char]
revRot strng sz = concatMap f . takeWhile ((==sz) . length) . splitN sz $ strng where
	f xs = let
		l = length xs
		in case even $ sum . map (^3) $ map read $ return xs of
			True -> reverse xs
			_ -> take l . drop (l- 1) . cycle $ xs
