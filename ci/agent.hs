import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Ord
import System.IO
import Control.Monad
import Control.Applicative
import Data.Function


type N = Int
type L = (Int,Int)

type P = [Int]

links :: P -> [L]
links = map order . (zip <*> tail) where
    order t@(x,y)   | x > y = (y,x)
                    | otherwise = t

type G = Int -> [Int]

both t@(x,y) = [t,(y,x)]

mkGraph :: [N] -> [L] -> G
mkGraph ns = (M.!) . foldr (\(k,a) -> M.insertWith (++) k [a]) boot . concatMap both 
    where boot = foldr (\k -> M.insert k []) M.empty $ ns
  
reach :: G -> S.Set N -> N -> [P]
reach g os x = reach' os S.empty  [[x]] where
    reach' _ _ [] = []
    reach' os s ((x:xs):zs)  
        | x `S.member` os = reverse (x:xs) : reach' (S.delete x os) (S.insert x s) zs
        | x `S.member` s = reach' os s zs
        | otherwise = reach' os (S.insert x s)  $ zs ++ map (flip (:) $ x : xs) (g x)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    [n,l,e] <- map read <$> words <$> getLine
    
    g <- fmap (mkGraph [0..n-1]) . replicateM l $ do
        [p1,p2] <- map read <$> words <$> getLine
        return (p1,p2)
    os <- fmap S.fromList . replicateM e $ read <$> getLine
    loop g os

loop :: G -> S.Set N -> IO ()
loop g os = do
    z@(ps:_) <- groupBy ((==) `on` length) <$> sortBy (comparing length) <$> reach g os <$> read <$> getLine
    let (n1,n2) =  head . maximumBy (comparing length) . group . sort $ ps >>= links
    putStrLn $ show n1 ++ " " ++ show n2
    
    loop g os
