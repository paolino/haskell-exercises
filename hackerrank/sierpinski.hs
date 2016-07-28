
{-# language ViewPatterns,LambdaCase #-}
import Control.Applicative
import Control.Arrow
import Data.List
import Control.Lens

data Dir = L | R deriving  Show

type D = (Dir,Int {-pos-},Int {-life-})

next (_,_,0) = [] -- end of life
next (L,n,l) = [(L, n - 1, l - 1)] --keep left
next (R,n,l) = [(R, n + 1, l - 1)] -- keep right

data C = Bl | Wh 

charC Bl = '_'
charC Wh = '1'


splitTo :: Int -> Int -> D -> D -> [D]
splitTo mx = split where
    split ((<= mx) -> True) br bl = [br,bl]
    split k br@(_,n,_) bl@(_,m,_)
        | m - n >= 2 ^ k - 1 = 
            split (k - 1) br (L, n + k2 -1, life) ++ 
            split (k - 1) (R, n + k2, life) bl
        | otherwise = split (k - 1) br bl
            where   k2 = 2 ^ (k - 1)
                    life = 2 ^ (k - 2) - 1

couples :: [t] -> [(t, t)]
couples = unfoldr f where
    f [] = Nothing
    f (x:y:rs) = Just ((x,y),rs)

colsBy2 = 6 :: Int
maxLife = 2 ^ (cols2 - 1)
maxCols =  2 ^ cols2 - 1

color :: C -> Int -> [Int] -> [C]
color c e = concat . snd . mapAccumL w c . (zipWith subtract <*> tail) . (0:) . (++ [e]) where
    w c d = (switch c, replicate d c)
    switch Bl = Wh
    switch Wh = Bl

base = [(R, 0, maxLife),(L, maxCols, maxLife )] :: [D]

recsplit n xs = couples xs >>= uncurry (splitTo (colsBy2 - n) colsBy2)

iter :: Int -> [D] -> [D]
iter n x = recsplit n $ (x >>=  next)

main = do
    n <- readLn
    mapM_ putStrLn . reverse . 
        map (map charC . color Bl maxCols . map (view _2)) . 
        take maxLife . iterate (iter n) $ recsplit n base
