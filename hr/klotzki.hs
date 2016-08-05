{-# LANGUAGE DeriveGeneric #-}
import Data.Hashable
import Data.List
import Control.Arrow
import GHC.Generics
import qualified Data.HashSet as H

type P = (Int,Int)

type F = [P]

data Board = Board (Char,[F]) [P] deriving Generic

data Di = U | D | L | R deriving Enum

to L (x,y) = (x - 1,y)
to R (x,y) = (x + 1,y)
to U (x,y) = (x,y - 1)
to D (x,y) = (x,y + 1)

next p q = p `elem` map (($q) . to) [U ..R]

-- instance Hashable F
parse :: Array P Char -> Board
parse = foldr f (Board [] []) . assocs where
    f (p,'.') (Board fs ls) = Board fs (p:ls)
    f (p,c) (Board fs ls) = Board fs' ls where
        fs' = let
            ps = fromJust $ lookup c fs
            in (c,p:ps): filter (/= c) fs


data PF = PF F P deriving (Ord,Eq,Show,Generic)
instance Hashable PF

data Move = Move PF [P] deriving (Eq,Ord,Show,Generic)


t ==> c = if t then [c] else []
infixl 1 ==> 

cb :: (a , a) -> [a -> a -> b] -> [b]
cb (x,y) fs = [(x,y),(y,x)] >>= \t -> fs  >>= return . ($t) . uncurry

move :: Move -> [Move]
move (Move (PF L p) [q1,q2]) = let
    c1 q1 q2 = next q1 p ==> Move (PF L q1) [p,q2]
    c2 q1 q2 = next q1 p && next q2 q1 ==> Move (PF L q2) [p,q1]
    in concat $ cb (q1,q2) [c1,c2]

move (Move (PF Q p) [q1,q2]) = let
    cu q1 q2 = q1 == u p && q2 == r q1 ==> Move (PF Q (u p)) [d p, (d . r)  p]     
    cd q1 q2 = q1 == (d . d) p && q2 == r q1 ==> Move (PF Q (d p)) [p, r p]     
    cl q1 q2 = q1 == l p && q2 == d q1 ==> Move (PF Q (l p)) [r p, (d . r) p]     
    cr q1 q2 = q1 == (r . r) p && q2 == d q1 ==> Move (PF Q (r p)) [p, d p]     
    in concat $ cb (q1,q2)  [ cu, cd , cl , cr ]

move (Move (PF V p) [q1,q2]) = let 
    cl q1 q2 = q1 == l p && q2 == d q1 ==> Move (PF V (l p)) [p, d p]     
    cr q1 q2 = q1 == r p && q2 == d q1 ==> Move (PF V (r p)) [p, d p]     
    cu q1 q2 = q1 == u p ==> Move (PF V q1) [d p, q2]     
    cuu q1 q2 = q1 == u p && q2 == u q1 ==> Move (PF V q2) [p, d p]     
    cd q1 q2 = q1 == d (d p) ==> Move (PF V (d p)) [p,q2]
    cdd q1 q2 = q1 == d (d p) && q2 == d q1 ==> Move (PF V q1) [p, d p]
    in concat $ cb (q1,q2) $ [cu, cuu, cd, cdd, cl, cr]


move (Move (PF H p) [q1,q2]) = let 
    cu q1 q2 = q1 == u p && q2 == r q1 ==> Move (PF H q1) [p, r p]     
    cd q1 q2 = q1 == d p && q2 == r q1 ==> Move (PF H q1) [p, r p]     
    cl q1 q2 = q1 == l p ==> Move (PF H q1) [r p, q2]     
    cll q1 q2 = q1 == l p && q2 == l q1 ==> Move (PF H q2) [p, r p]     
    cr q1 q2 = q1 == r (r p) ==> Move (PF H (r p)) [p,q2]
    crr q1 q2 = q1 == r (r p) && q2 == r q1 ==> Move (PF H q1) [p, r p]
    in concat $ cb (q1,q2) $ [cl,cll,cr,crr,cu, cd]


through :: [a] -> [(a , a -> [a])]
through = through' id where
    through' f [x] = [(x,f . return)]
    through' f (x:xs) = (x, \x' -> f $ x':xs) : through' (f . (x:)) xs

data Board = Board [PF] [P] deriving (Eq,Ord,Show,Generic)
instance Hashable Board
norm (Board pfs ls) = Board (sort pfs) (sort ls)

moves :: Board -> [Board]
moves (Board pfs ls) = map head . group . sort . map norm $ through pfs >>= \(pf,back) -> map (\(Move pf ls) -> Board (back pf) ls)  (move $ Move pf ls)

moves' (Board pfs ls) = pfs >>= \pf -> move $ Move pf ls

algo0 :: Board -> [[Board]]
algo0 = iterate (>>= moves) . return

algo1 :: Board -> [[Board]]
algo1 = algo' H.empty . return where
    algo' rs bs = let
        ys = filter (\s -> not (s `H.member` rs) && not (symmetric s `H.member` rs)) $ bs >>= moves
        in ys : algo' (foldr H.insert rs ys) ys

close (Board pfs _) = PF Q  (1,3) `elem` pfs
s1 = norm $ Board 
    [   PF L (0,0)
    ,   PF L (0,1)
    ,   PF L (3,0)
    ,   PF L (3,1)
    ,   PF L (1,2)
    ,   PF L (2,2)
    ,   PF L (1,3)
    ,   PF L (2,3)
    ,   PF L (0,4)
    ,   PF L (3,4)
    ,   PF Q (1,0)
    ,   PF V (0,2)
    ,   PF V (3,2)
    ] [(1,4),(2,4)]
s3 = norm $ Board 
    [   PF L (0,0)
    ,   PF L (3,0)
    ,   PF L (3,1)
    ,   PF L (1,2)
    ,   PF L (2,2)
    ,   PF L (3,2)
    ,   PF L (1,3)
    ,   PF L (2,3)
    ,   PF Q (1,0)
    ,   PF V (0,1)
    ,   PF V (0,3)
    ,   PF V (3,3)
    ] [(1,4),(2,4)]
s32 = norm $ Board 
    [   PF L (0,0)
    ,   PF L (3,0)
    ,   PF L (0,1)
    ,   PF L (3,1)
    ,   PF H (1,2)
    ,   PF V (0,3)
    ,   PF V (1,3)
    ,   PF V (2,3)
    ,   PF V (3,3)
    ,   PF Q (1,0)
    ] [(0,2),(3,2)]
s50 = norm $ Board 
   [    PF L (0,0) 
   ,    PF L (3,0) 
   ,    PF V (0,1) 
   ,    PF V (3,1) 
   ,    PF H (1,2) 
   ,    PF H (1,3) 
   ,    PF H (1,4) 
   ,    PF L (0,3) 
   ,    PF L (3,3) 
   ,    PF Q (1,0)
   ] [(0,4),(3,4)]

c = (2,2)
l0 = Board [PF L c] 
v0 = Board [PF V c] 
h0 = Board [PF H c] 

main = mapM_ print $ zip [1..] . map length  . takeWhile (all $ not . close) $ algo1 s50

busy (PF L p) = [p]
busy (PF Q p) = [p, r p ,d p ,d . r $ p]
busy (PF V p) = [p,d p]
busy (PF H p) = [p,r p]

valid (Board pfs ls) = sort (ls ++ (pfs >>= busy)) == ((,) <$> [0..3] <*> [0..4])

data TS = Fl | Bi

sy Fl (x,y) = (3 - x,y)
sy Bi (x,y) = (2 - x,y)

sym (PF L p) = PF L (sy Fl p)
sym (PF V p) = PF V (sy Fl p)
sym (PF H p) = PF H (sy Bi p)
sym (PF Q p) = PF Q (sy Bi p)

symmetric (Board pfs ls) = Board (map sym pfs) (map (sy Fl) ls)
