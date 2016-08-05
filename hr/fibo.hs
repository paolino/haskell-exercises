{-# language NoMonomorphismRestriction #-}
import Data.List
import Data.Ord
import Control.Monad
import Control.Applicative


           
fibo = 0:1:(zipWith (+) <*> tail $ fibo)
main = do
    n <- readLn
    ws <- replicateM n $ read <$> getLine
    forM_ ws $ print . (`rem` (fibo !!)
    
