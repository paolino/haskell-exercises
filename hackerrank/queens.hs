{-# language ViewPatterns, FlexibleContexts #-}
import Data.Array 
import Data.Hashable
import qualified Data.Set as S
import Data.List
import Control.Monad.State
import Control.Monad.List
import Control.Monad.Trans

l n = range ((0,0),(n-1,n-1))
board n = listArray ((0,0),(n-1,n-1)) $ repeat  False

pop n qs = [q | q <- l n, all not $ [check q q' | q' <- qs]]

check (i,j) (i',j') = 
    i == i' ||
    j == j' 
    ||
    abs (j - j') == abs (i - i') 
    ||
    abs (j - j') == 1 && abs (i - i') == 2 ||
    abs (j - j') == 2 && abs (i - i') == 1


populate n = let
    t' s s' n = do
        r <- gets $ S.member (hash s') 
        if r then ListT $ return []
        else t s s' n
    t s s' ((== 0) -> True) = ListT $ return [s']
    t [] _ _ = ListT $ return []
    t s s' n = do
        z <- ListT (return s) >>= \x -> t' (filter (not . check x) s) (sort $ x:s') (n - 1) 
        
        modify $ S.insert (hash s')
        return z
    in  t' (l n) [] n
