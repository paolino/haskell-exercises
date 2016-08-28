
{-# language NoMonomorphismRestriction #-}
import Control.Applicative
import Control.Monad
import System.IO
import Data.List


f :: (Int,Int) -> (Int,Int) -> (Int,Int)
f (x,y) r@(xm,ym)
    | x > y = (xm + 1,ym)
    | x < y = (xm,ym + 1)
    | otherwise = r

r :: IO [Int]
r = map read <$> words <$> getLine

main :: IO ()
main = foldr f (0,0) <$> (zip <$> r <*> r) >>= putStrLn . intercalate " " . (\(x,y) -> map show [x,y])
