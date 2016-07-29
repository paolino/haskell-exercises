{-# language TupleSections #-}
import Control.Arrow
import Data.Tuple
import Data.List
import Data.Maybe

type P = (Int,Int)

border :: [P] -> [P]
border = (++ [(-1,-1)])

line  :: Int -> [Bool] -> [(P,P)]
line n  = (zip <*> (border . tail)) . map ((, n) . fst) . filter snd . zip [0..]

merge  :: [(P,P)] -> [(P,P)] -> [[Int]]
merge [] [] = []
merge ((p,ph):phs) ((_,pv):pvs) = ([p,ph,pv] >>= \(x,y) -> [x,y]) : merge phs pvs

solve :: [[Bool]] -> [[Int]]
solve t = merge (f id id) (f (map $ swap *** swap) transpose)
    where
        f g h = sort . g . concat .  zipWith line [0..] . h $ t

 
