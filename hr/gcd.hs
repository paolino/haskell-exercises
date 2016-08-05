import Data.List
import Control.Arrow
import Control.Applicative
import Control.Monad

gcdC :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
gcdC xs'@((x,n):xs) ys'@((y,m):ys) 
    | x == y = (x,min n m):gcdC xs ys
    | x < y = gcdC xs ys'
    | True = gcdC xs' ys
gcdC _ _ = []

factorsout [] = []
factorsout (x:y:xs) = (x,y):factorsout xs

main = do
    n <- readLn   
    ns <- replicateM n $ factorsout <$> map read <$> words <$> getLine
    putStrLn . intercalate " " . map show $ foldr1 gcdC ns >>= \(x,n) -> [x,n]

