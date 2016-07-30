{-# language NoMonomorphismRestriction #-}
import Data.List
import Data.Ord
import Data.Function
import Control.Monad
import Control.Applicative
import Control.Monad.Fix

-- generic 2D building schema with dependency on left and up cell
table   :: ([t] -> [t]) -- filtering action
        -> (t -> t) -- dependency from left
        -> (t -> t) -- dependency from above
        -> t  -- cell @ (0,0)
        -> [[[t]]] -- a table with choices of t as cell
table c r f a0 = fix $ (:) (map return $ iterate r a0) . map (build c r f) where
    build c r f (u:us) = fix $ (:) (c $ map f u) . zipWith g us
    g u l = c $ (f <$> u) ++ (r <$> l)

type D = [Int] -- a dice (top,right,down,left,front,back)
d0 = [1,4,6,3,2,5] :: D -- starting dice

dleft,dabove :: (Int,D) -> (Int,D) -- possible modification tracking sum of top
dleft (m,[t,r,d,l,f,b]) = (m + l,[l,t,r,d,f,b])
dabove (m,[t,r,d,l,f,b]) = (m + b,[b,r,f,l,t,d])


-- discard dice disposition copies in favor the highest sum valued
filterS   = map (head . sortBy (flip $ comparing fst)). groupBy ((==) `on` snd) . sortBy (comparing snd)

main = do
    let f = table filterS dleft dabove (1,d0)
    n <- readLn
    replicateM_ n $ do
        [x,y] <- map read <$> words <$> getLine
        print $ maximum . map fst $ f !! (y - 1) !! (x - 1) 

