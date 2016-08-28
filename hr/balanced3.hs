     

     


-- Enter your code here. Read input from STDIN. Print output to STDOUT
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.List
import Control.Applicative

sols :: Int -> Sols
sols x = let
    x2 = x `div` 2
    x3 = x `div` 3
    in M.fromList $ do
        y <- [x `div` 3 .. x `div` 2]
        let w = x - 2 * y
        guard $ w < y
        guard $ w > 0
        [(w,y),(y,w)]

type Index = Int
type Value = Int
type Count = Int

type Stat = M.Map Value Count
type Sols = M.Map Value Value
type Reverse = M.Map Index (Value, Maybe Index)

path :: Reverse -> Maybe Index -> [Value]
path m  = unfoldr f where
    f Nothing = Nothing
    f (Just i) = Just $ m M.! i

data Env = Env Sols Int Stat Reverse

subtractWithZero x y = case y -x of
    0 -> Nothing
    x -> Just x

correct :: Env -> (Value, Maybe Index) -> Stat -> Stat
correct (Env _ _ w rev) (v,mk) dw = let
    vs = path rev mk
    vs' = map (subtract v) vs
    w' = M.unionWith (+) w $ M.fromList (zip vs' $ repeat 1)
    dw' = M.unionWith (+) dw $ M.fromList (zip vs $ repeat 1)
    in M.differenceWith subtractWithZero w' dw' 

tr k x = trace (show (k,x)) x
logic :: Env -> Index -> Stat -> [Maybe Value] -> Maybe Value
logic e@(Env cs t w rev) k w' ks = let 
        r@(v,mf) = rev M.! k
        rs = (:ks) $ do
                    v' <-  M.lookup v cs
                    _ <- M.lookup v' (correct e r w')
                    return $  abs $ v' - v
        in case catMaybes (tr k rs) of
            [] -> Nothing
            rs -> Just $ minimum rs

mixer :: Index -> [(Value, Stat)] -> (Value, Stat)
mixer k xs = let 
    (ss,rs) = unzip xs
    s = k + sum ss
    in (s,M.unionsWith (+) $ M.singleton s 1 : rs)

data T = T Index [T] deriving Show

type Values = M.Map Index Value

analize :: Values -> Maybe Index -> Reverse -> T -> (Reverse ,(Value, Stat))
analize m mf rev (T k ts) = let 
    (rev',vs) = mapAccumL (analize m (Just k))  rev ts  
    g@(v,_) = mixer (m M.! k) vs
    in (M.insert k (v,mf) rev',g)

check ::  (Index -> Stat -> [a] -> a) -> T -> (a, (Value, Stat))
check f (T k ts) = let
    (rs,zs) = unzip $ map (check f) ts
    (s,z) = mixer k zs
    in (f k z rs, (s,z))

checkR :: Values -> T -> Maybe Int
checkR vs tr = let
    (rev,(t,w)) = analize vs Nothing M.empty tr
    env = Env (sols t) t w rev
    in fst $ check (logic env) tr

type Edge = (Int,Int) 
parseT :: [Edge] -> Int -> ([Edge], T)
parseT ls i = let
    (es,ls') = partition ((==) i . fst) ls
    (es',ls'') = partition ((==) i . snd) ls'
    (r,ts) = mapAccumL parseT ls'' $ map snd es ++ map fst es'
    in (r,T i ts)

parseValues :: [Value] -> Values
parseValues = M.fromList . zip [1..]

solve :: [Edge] -> [Value] -> Maybe Value
solve es vs = checkR (parseValues vs) (snd $ parseT es 1)

main = do
    n <- readLn
    rs <- replicateM n $ do
        t <- readLn
        ws <- map read <$> words <$> getLine
        ls <- replicateM (t -1) $ (\[x,y] -> (read x, read y)) <$> words <$> getLine
        let (_,t) = parseT ls 1

        let v = parseValues ws
        print $ analize v Nothing M.empty t
        return $ solve ls ws
    forM_ rs $ \r -> case r of
        Just v -> print v
        Nothing -> print (-1)
    
