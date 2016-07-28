
{-# language ScopedTypeVariables,ViewPatterns #-}
import System.IO
import Control.Monad
import Control.Applicative
import Data.List
import Data.Ord
import Data.Function
import Control.Arrow hiding (loop)

-- decision by a transfer function and a tower of choices
decision :: (Ord c, Num c) => (a -> a -> c) ->  [[a]] -> [a]
decision eval = fst . head . sortBy (comparing snd) . map (id &&& cost eval) . sequence where
    cost :: Num c => (a -> a -> c) -> [a] -> c
    cost eval (x:xs) = sum . snd $ mapAccumL f x xs where
        f ed' ed = (ed, eval ed' ed)

-- versus aware enumarator
renum a b = enumFromThenTo  a (a + (signum $ b - a)) b

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    [_,_,_,ef,_,_,_,n] <- map read <$> words <$> getLine
    
    es <- map (map snd) <$> groupBy ((==) `on` fst) <$> sort <$> (replicateM n $ (\[x,y]-> (x,y)) <$> map read <$> words <$> getLine)
    
    loop $ Left (es ++ [[ef]])

type E = Int -- elevator position
type D = Bool -- direction
type C = Int -- path cost
type L = Int -- level
type P = Int -- position

loop :: Either [[E]] [(L,P)] -> IO ()
loop mp = do
    [read -> f,read -> x,_] <- words <$> getLine
    p <- case mp of 
        Left es ->  do -- boot turn, select the cheapest path
            return $ mkPath $ [x] : es
        Right p -> return p -- other turns
    putStrLn $ case (f,x) `elem` p of
        True -> "WAIT"
        False -> "BLOCK"
    loop $ Right p

mkPath :: [[E]] -> [(L,P)]
mkPath ([e]:es) = let
    eds = [(e,True)] : map (\e -> (,) <$> e <*> [True,False]) es
    in path . map fst . decision elev $ eds
    
elev ::  (E,D) -> (E,D) -> C
elev (e',d') (e,d) =  f (e - e',d) where
    f (g,d) 
        | (d'== d) = g
        | otherwise = g + 3

path :: [E] -> [(L,P)]
path xs = zip [0..] (zip <*> tail $ xs) >>= \(l,(e',e)) -> zip (repeat l) (renum e' e)
   
