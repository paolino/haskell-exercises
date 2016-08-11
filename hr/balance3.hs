import Data.Maybe
import Control.Monad
import Data.List
import Control.Arrow
import Data.Function
import Data.Ord
import Debug.Trace

data T = T Int [T] deriving Show

sumT :: T -> (Int,T)
sumT (T x []) = (x,T x [])
sumT (T x ts) = let
	(ss,ts') = unzip . map sumT $ ts
	s = x + sum ss
	in (s,T s ts')

type P = (Int,[Int])

paths :: T -> [P]
paths (T l []) = [(l,[])]
paths (T x ts) = (x,[]) :( zip [0..] ts >>= \(i,t) ->  (second (i:) <$> paths t))

prepare :: Int -> [P] -> [[P]]
prepare g = 	groupBy ((==) `on` fst) . 
		takeWhile ((<= g `div` 2) . fst) . 
		dropWhile ((< g `div` 3) . fst) .
		sortBy (comparing fst) 

sd (v,[]) _ = False
sd (v,k:ks) (T w ts) 
	| w == 2 * v = True
	| w < 2 * v = False
	| otherwise = sd (v,ks) $ ts !! k

solve :: T -> Maybe Int
solve t = let
	(g,t') = sumT t
	solve' []  = Nothing
	solve' ([x@(v,_)]:xs) 
		| sd x t'= Just $ 3*v - g
		| otherwise = solve' xs
	solve' (((v,_):_):_) = Just $ 3*v - g
	in solve' . prepare g . paths $ t'
	
parseT :: ([(Int,Int)],[(Int,Int)]) -> Int -> (([(Int,Int)],[(Int,Int)]), T)
parseT (ls,ws) i = let
	(es,ls') = partition ((==) i . fst) ls
	(es',ls'') = partition ((==) i . snd) ls'
	([w],ws') = partition ((== i) . fst) ws
	(r,ts) = mapAccumL parseT (ls'',ws') $ map snd es ++ map fst es'
	in (r,T (snd w) ts)

main = do
	n <- readLn
	ts <- replicateM n $ do
		t <- readLn
		ws <- map read <$> words <$> getLine
		ls <- replicateM (t -1) $ (\[x,y] -> (read x, read y)) <$> words <$> getLine
		return $ snd . parseT (ls, zip [1..] ws) $ 1
	forM_ ts $ \tr -> do
		print tr
		case solve tr of
			Just v -> print v
			Nothing -> print (-1)

	
