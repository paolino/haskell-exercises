 {-# language ViewPatterns #-}
import Data.List
import System.IO
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set  as S

mkM' ::([(N,N)] , M.Map N [N],S.Set N) -> M.Map N [N]
mkM' ([],m,_) = m
mkM' (xs,m,s)  = mkM' $ foldr f ([],m,s) xs where
    f (i,j) (rs,m,s) = 
        if S.member j s || M.member j m then  g j i else 
        if S.member i s || M.member i m then  g i j else
            ((i,j):rs,m,s)            
            where
        g i j = (rs,M.insertWith (++) i [j] m, S.insert j s)

mkM :: [(N,N)] -> M.Map N [N]
mkM ((i,j):xs) = mkM' (xs,M.singleton i [j],S.singleton j)

type N = Int
type D = Int

data T a = T [(a, T a)] deriving Show

other n (i,j) 
    | i == n = j
    | otherwise = i

mkT :: N -> M.Map N [N] -> T ()
mkT n ns = T $ zip (repeat ()) $ map (flip mkT ns) (M.findWithDefault [] n ns)

count :: T () -> (Int, T Int)
count (T []) = (1, T [])
count (T ts) = let
    ts' = map (count . snd) ts
    r = maximum (map fst ts') + 1
    in (r, T ts')

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    n <- readLn
    
    ns@((i,_):_) <- replicateM n $ (\[x,y] -> (x,y)) <$> map read <$> words <$> getLine
    print $ mkM ns
    
    let val (T ts) = (`div` 2) . (+1) . sum $ take 2 $ sortBy (flip compare) $ 0:map fst ts
  
    let r = snd .count $  mkT i $ mkM ns

    print (val r)

