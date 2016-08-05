import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Ord
import Data.Function 


type R = (Int,Int)

mkM :: [R] -> (M.Map Int [Int], S.Set Int,S.Set Int)
mkM = foldr f (M.empty,S.empty,S.empty) . map (fst . head &&& map snd) . groupBy ((==) `on` fst) . sort where
    f (x,ys) (m,sk,sd)  = (M.insert x ys m, S.insert x sk, foldr S.insert sd ys)

longest m i 
    | i `M.member` m = (1+) . maximum $ map (longest m) $ m M.! i
    | otherwise = 1

main = do
    n <- readLn
    xs <- replicateM n $ (\[x,y] -> (x,y)) <$> map read <$> words <$> getLine
    let (m,sk,sd) = mkM xs
        s = S.toList $ sk S.\\ sd
    print . maximum . map (longest m) $ s
    
