import Data.List


dblLinear :: Int -> Integer
dblLinear n = (tail . map snd . iterate consume $ ([1],undefined)) !! n

list = tail . map snd . iterate consume $ ([1],undefined)
consume (xs,_) = (\(x:xs) -> (2*x + 1 : 3*x + 1 : xs,x)) . map head . group . sort $ xs
