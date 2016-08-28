{-# language ViewPatterns #-}
import Control.Monad
import Control.Applicative
import Data.List
import Data.Either

acca (Left h) = h
acca (Right (_,h)) = h

data H = H [(Int,Int)] [(Int,Int)] deriving Show
f (acca -> H xs@((i,_):_) ms@((_,m):_)) [1,x]
    | x > m = Left $ H xs' $ x':ms
    | otherwise = Left $ H xs' ms
    where
        x' = (i + 1,x)
        xs' = x':xs
f  (acca -> H ((i,_):xs) ms'@((j,_):ms)) [2] 
    | i == j = Left $ H xs ms
    | otherwise = Left $ H xs ms'
f  (acca -> h@(H _ ((_,x):_))) [3] = Right (x,h)
f _ [1,x] = Left $ H [(0,x)] [(0,x)]
f h x = error (show (h,x))


main = do
    n <- readLn
    cs <- replicateM n (map read <$> words <$> getLine) 
    print cs
    mapM_ (print . fst) . rights $ scanl f (Left $ H [] []) $ cs
    
        
