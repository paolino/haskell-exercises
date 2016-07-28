{-# language LambdaCase #-}
import Data.List
import Control.Monad
import Control.Arrow

newtype C a = C (a -> Maybe (C a))

fill :: Eq a => [a] -> C a -> [[a]] 
fill [] _ = return []
fill xs (C c) = do
    x <- xs
    (x:) <$> maybe [] (fill $ delete x xs) (c x)

crossCheck :: [(Int,Int,Int,Int)] -> [String] -> Int -> String -> Bool
crossCheck dbc dbs n x = all good dbc where
    check (w1,p1,w2,p2) 
        | n == w1 && w2 < length dbs =  dbs !! w2 !! p2 == x !! p1
        | otherwise = True
    good r@(w1,p1,w2,p2) = check r && check (w2,p2,w1,p1)

newtype Rem = Rem (Int -> String -> Maybe Rem)

mkRem :: [(Int,Int,Int,Int)] -> Rem
mkRem xs = let
    check ys n x = if crossCheck xs ys n x 
        then Just $ make (ys ++ [x]) else Nothing
    make ys = ys `seq` Rem $ check ys
    in make []

crossfit :: [Int] -> [(Int,Int,Int,Int)] -> C String
crossfit ls dbc = let
    good (Rem f) (n:ns) i x = do
        guard $ length x == n
        f' <- f i x 
        return $ make f' ns (i + 1)
    make f ns i = C $ good f ns i
    in make (mkRem dbc) ls 0

type P = (Int,Int)

mkDbcE :: [P] -> [P] -> Maybe (Int,Int)
mkDbcE xs ys = lookup True $ do
    (i,x) <- zip [0..] xs
    (j,(x',y')) <- zip [0..] ys
    return $ (x == (y',x'),(i,j))

mkDbc :: [(Int,[P])] -> [(Int,[P])] -> [(Int,Int,Int,Int)]
mkDbc xs ys = do
    (i,x) <- xs
    (j,y) <- ys
    case mkDbcE x y of
        Nothing -> []
        Just (px,py) -> return $ (i,px,j ,py)

collect :: Int -> String -> [[P]]
collect k = (\(x,_,_) -> extract x) . foldl f ([],False,0) where
    f (xs,_,n) '+' = (xs,False,n+1)
    f (xs,False,n) '-' = ([n]:xs,True,n+1)
    f (ns:xs,True,n) '-' = ((n:ns):xs,True,n+1)
    f x c = x
    extract = filter ((>1). length) . map (map ((,) k) . reverse)

reverseDB :: [(Int,[P])] -> [String] -> P -> Char
reverseDB xs ss p = let
    Just (n,ps) = find (elem p . snd) xs
    Just j = findIndex (==p) ps
    in ss !! n !! j

parse :: [String] -> ([Int], [(Int,Int,Int,Int)], [String] -> [String])
parse ss = let
    hps = zip [0..] ss >>= uncurry collect
    vps = zip [0..] (transpose ss) >>= uncurry collect
    (hps',vps')  = splitAt (length hps) $ zip [0..] $ hps ++ vps
    db0 = hps' ++ map (second (map swap)) vps'
    
    in (map (length . snd) hps' ++ map (length .snd) vps' ,
        mkDbc hps' vps', 
        rebuild ss . reverseDB (hps' ++ map (second (map swap)) vps') 
        )

swap (x,y) = (y,x) 
  
rebuildL :: (P -> Char) -> Int -> String -> String
rebuildL rdb n  = zipWith f [0..] where
    f m '+' = '+'
    f m '-' = rdb (n,m)

rebuild ::  [String] -> (P -> Char) -> [String]
rebuild os f = zipWith (rebuildL f) [0..] os

main = do
    (ls,db,rdb) <- parse <$> replicateM 10 getLine
    ss <- words <$> map (\case {';' -> ' ';x -> x}) <$> getLine
    mapM_ putStrLn $ rdb (head $ fill ss (crossfit ls db))

