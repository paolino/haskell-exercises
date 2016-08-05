{-# language ViewPatterns #-}
import Control.Applicative
import Control.Arrow

data Dir = U | L | R  deriving Show

data B = B {
    level :: Int,
    count :: Int,
    pos :: Int,
    dir :: Dir
    } deriving Show

next :: Int -> B -> [B]
next _ (B k 1 p U) = [B k h (p - 1) L, B k h (p + 1) R]
        where h = 2 ^ k 
next s (B ((== s) -> True) 1 _ _) =  []
next _ (B k 1 p lr) = [B k' h p U]
        where   k' = k - 1
                h =  2 ^ k'
next _ (B k h p L) = [B k (h - 1) (p - 1) L]
next _ (B k h p R) = [B k (h - 1) (p + 1) R]
next _ (B k h p U) = [B k (h - 1) p U]

line xs = map (\n -> if n `elem` xs then '1' else '_') [0..63]

main = do
    t <- readLn
    mapM_ putStrLn . map line  . reverse . take 100 . map (map pos)  . iterate (>>= next (5 - t)) $ [B 4 16 32 U]
