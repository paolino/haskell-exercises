import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Control.Arrow 
import Control.Monad
import Control.Applicative

load :: [(Int,Int)] -> M.Map Int (S.Set Int)
load = foldr f M.empty where
    f (x,y) = M.insertWith S.union x (S.singleton y) . M.insertWith S.union y (S.singleton x)


circle :: M.Map Int (S.Set Int) -> Int -> S.Set Int
circle m = snd . circle' S.empty where
    circle' rem i 
        | i `S.member` rem = (rem,S.empty)
        | otherwise =   second (S.insert i . S.unions) . 
                        mapAccumL circle' (S.insert i rem) $  
                        S.elems $ m  M.! i 
circles :: M.Map Int (S.Set Int) -> [S.Set Int]
circles m = filter (not . S.null) . snd . mapAccumL circles' S.empty $ M.keys m where
    circles' rem i 
        | i `S.member` rem = (rem,S.empty) 
        | otherwise = S.union rem &&& id $ circle m i

main = do
    ns <- map (\x -> (x,x)) <$> (\x -> [1 .. x]) <$> readLn
    np <- readLn
    bs <- (++ ns) <$> map ((\[x,y] -> (x,y)) . map read . words) <$> replicateM np getLine
    print $ sum . map (bus . S.size) . circles . load $ bs

bus n = head $ dropWhile ((<n) . (^2)) $ [1..]


