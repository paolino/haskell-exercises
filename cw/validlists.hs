import Control.Arrow
import Data.Ord
import Data.List
import Data.Char
import Data.Monoid
import qualified Data.Map as M

data R  = F | S | B deriving (Eq,Ord)

compareM :: (Ord a , Ord b, Ord c) => (a,(b,c)) -> (a,(b,c)) -> Ordering
compareM = flip (comparing $ snd . snd) <> comparing (fst . snd) <> comparing fst 

merge :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,(R,Int))]
merge xs ys = sortBy compareM  . filter  ((>1) . snd . snd) . M.assocs 
    $ M.unionWith f (fmap ((,) F) $ M.fromList xs)
                    (fmap ((,) S) $ M.fromList ys)
    where 
  f (F,xn) (S,yn) | xn == yn    = (B,xn)
                  | xn > yn     = (F,xn)
                  | otherwise   = (S,yn)
       
stat :: Ord a => [a] -> [(a,Int)]
stat =  map (head &&& length) . group . sortBy (flip compare) 

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = intercalate "/" . map showM $ merge (f s1) (f s2) where
    f = stat . filter isLower
    showM (c,(r,n)) = showR r ++ ":" ++ replicate n c
    showR F = "1"
    showR S = "2"
    showR B = "="
