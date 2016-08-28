isMerge :: String -> String -> String -> Bool
isMerge [] [] [] = True
isMerge xs [] ys = xs == ys
isMerge xs ys [] = xs == ys
isMerge (x:xs) (y:ys) (z:zs) 
	| x == y = isMerge xs ys (z:zs)
	| x == z = isMerge xs (y:ys) zs
	| otherwise = False
isMerge _ _ _ = False
