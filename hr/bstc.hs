import Data.List
import Control.Monad

dp :: Int -> [Integer]
dp k = take (k + 1) $ a where
    a = 1 : map sum (transpose t)
    t = a : zipWith (\n l -> replicate (n + 1) 0 ++ map (l *) a) [0..k] (tail a)

main = do 
    xs <- readLn >>= flip replicateM readLn 
    mapM_ (print . (`rem` (10 ^ 8 + 7)) . (dp (maximum xs) !!)) xs
