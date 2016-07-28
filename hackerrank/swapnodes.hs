{-# language ViewPatterns, DeriveFunctor #-}
import Control.Arrow
import Data.Maybe
import Control.Monad
import Data.List

data T a = T a (T a) (T a) | L deriving (Show, Functor)

traversal :: T a -> [a]
traversal L = []
traversal (T x t1 t2) = traversal t1 ++ x : traversal t2

swapper :: (Int -> Bool) -> T a -> T a
swapper k = swap 1 where
    swap _ L = L
    swap d@(k -> True) (T x t1 t2) = T x (swap (d + 1) t2) (swap (d + 1) t1)
    swap d (T x t1 t2) = T x (swap (d + 1) t1) (swap (d + 1) t2)
        
build :: [(Int,[Int])] -> Int -> T Int
build  _ (-1) = L
build  db i = T i (build db j) (build db k) where
    [j,k] = fromJust $ lookup i db

main = do
    n <- readLn
    t <- flip build 1   <$> zip [1..] 
                        <$> map (map read . words) 
                        <$> replicateM n getLine
    nk <- readLn
    ts <-  tail <$> scanl (\t x -> swapper ((==0) . (`mod` x)) t) t 
                <$> replicateM nk readLn
    forM_ ts $ putStrLn . intercalate " " . map show . traversal
    
