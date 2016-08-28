import Data.Maybe
import Control.Monad
import Data.List
import Control.Arrow
import Control.Applicative
import Data.Function
import Data.Ord
import Debug.Trace
import qualified Data.IntMap.Strict as M

data T = T Int [T] deriving Show

type P = (Int,[Int])

paths :: T -> [P]
paths (T x ts) = (x,[]) : (zip [0..] ts >>= \(i,t) -> second (i:) <$> paths t)

data E = E {
    top :: Int,
    tree :: T
    } deriving Show

makeE :: T -> E
makeE = uncurry E . sumT where
    sumT :: T -> (Int,T)
    sumT (T x []) = (x,T x [])
    sumT (T x ts) = let
        (ss,ts') = unzip . map sumT $ ts
        s = x + sum ss
        in (s,T s ts')

data S = S {
    coups :: [(Int,Int)],
    smalls :: [P],
    bigs :: [[P]]
    } deriving Show

cops g x 
    | x >= g `div` 3 && g - 2 * x >= 0 = [(x,g - 2*x)]
    | x < g `div` 3 && even (g - x) = [((g - x) `div` 2, x)]
    | True = []

makeS :: E  -> S
makeS (E g t) = let
    xs = paths t
    vs = map head . group . sortBy (comparing $ \(x,y) -> x - y) $ map fst xs >>= cops g
    (ss,bs) = partition ((< g `div` 3) . fst) $ sortBy (comparing fst) xs
    in S vs (reverse ss) $ 
            groupBy ((==) `on` fst) . takeWhile ((<= g `div` 2) . fst) $ bs

step :: E -> S -> Either (Maybe Int) S
step  _ (S [] _ _) = Left Nothing
step (E g t) (S ((b,s):cs) ss bs) = let 
    success = Left $ Just (b - s)
    (hss, ss') = partition ((== s) . fst) . dropWhile ((> s) . fst) $ ss
    (hbs, bs') = partition ((== b) . fst . head) . dropWhile ((< b) . fst . head) $ bs
    fail = Right $ S cs ss' bs'
    in case hbs of
        [] -> case hss of -- can't cut a big one
            [] -> fail -- same for small
            hss -> case any (checkPath t (== s + b) (< b + s)) $ map snd hss of
                True -> success -- a small with a big father
                False -> fail -- a small with no big father
        [[x]] -> case hss of
                    [] -> case checkPath t ((||) <$> (== b + b) <*> (== b + s)) (< b + s) $ snd x of
                        True -> success -- a big with a small or big father
                        False -> fail -- a big with no correct father
                    hss -> case filter (any id . zipWith (/=) (snd x) . snd) hss of
                        [] -> fail
                        _ -> success
        _ -> success -- two bigs

checkPath :: T -> (Int -> Bool) -> (Int -> Bool) -> [Int] -> Bool
checkPath t f _ [] = False
checkPath (T x ts) f g (k:ks) 
    | g x = False
    | True = f x || checkPath (ts !! k) f g ks

solve :: E -> Maybe Int
solve e = fix (\s -> either id s . step e) $ makeS e 

mkMap :: [(Int,Int)] -> M.IntMap  [Int]
mkMap xs = M.fromList . map (fst . head &&& map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst) $ xs ++ map swap xs where
    swap (x,y) = (y,x)

parseT :: M.IntMap [Int]  -> M.IntMap Int  -> Maybe Int -> Int ->  T
parseT ls ws mj i 
    | M.null ls = T (ws M.! i) []
    | otherwise = T (ws M.! i) $ map (parseT ls ws  (Just i)) $ maybe id delete mj $ ls M.! i

z k = maybe (error (show k)) id . M.lookup k
main = do
    n <- readLn
    ts <- replicateM n $ do
        t <- readLn
        ws <- map read <$> words <$> getLine
        ls <- replicateM (t - 1) $ (\[x,y] -> (read x, read y)) <$> words <$> getLine
        return $ parseT (mkMap ls) (M.fromList $ zip [1..] ws) Nothing 1
    forM_ ts $ \tr -> do
        case solve (makeE tr) of
            Just v -> print v
            Nothing -> print (-1)
    
