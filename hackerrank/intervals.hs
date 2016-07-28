{-# language ViewPatterns #-}

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

type I = Int -- index
type L = Int -- value

data T = T (I,I) !L !T !T | Q (I,I) L L  deriving Show
type R = (I,L,L)

mkT :: [R] -> I ->  (T, L)
mkT [(x,xa,ya)] y  = (Q (x,y) xa ya, min xa ya)
mkT xs y   = let
    (lx@((x,_,_):_),rx@((hy,_,_):_)) = splitAt (length xs `div` 2) xs
    (tl,al) = mkT lx hy
    (tr,ar) = mkT rx y 
    a = min al ar
    in (T (x, y) a tl tr, a)

query :: L ->  T -> (I,I) -> Maybe L
query z _ (0,0) = Just z
query z t i = query' t i where
    query' (Q (x',y') xa ya) (x,y)
        | y <= x' || x > y' = Nothing
        | x == y' = Just ya
        | y >= y' = Just $ min xa ya
        | otherwise = Just xa
    query' (T i@(x',y') a l r) j@(x,y)
        | y <= x' || x > y' = Nothing
        | x <= x' && y >= y' = Just a
        | otherwise =   let 
            l' = query' l j 
            r' = query' r j 
            in (min <$> l' <*> r') <|> l' <|> r'

points :: [(I, I)] -> [I]
points = map head . group . sort . concatMap (\(x, y) -> [x, y])

mkData :: [L] -> [(I, I)] -> ([R], I)
mkData ds (points -> (x:xs)) = let
    ((l, _), rs) = mapAccumL f (x, ds) xs
    f (x, as) y = let 
        (ls, r : rs) = splitAt (y - x) as
        in ((y, r:rs), (x, foldr1 min ls, r))
    in (rs, l)

main = do
    let r =  map read <$> words <$> getLine
    [n,t] <- r
    xs <- r
    ls <- replicateM t ((\[x,y] -> (x,y)) <$> r)
    let q = query (head xs) (fst . uncurry mkT . mkData xs $ (0,n-1) : ls)
    forM_ ls $ print . fromJust . q


