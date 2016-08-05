{-# language ViewPatterns #-}

import System.IO
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.Set as S
import Data.List

data C = J | Sl | Sp | W deriving (Enum,Show,Eq)
type P = Int
type V = Int

data S = S Int Int deriving (Show, Eq, Ord)

effect :: C -> S -> S
effect J (S s v) = S (s + v) v
effect W (S s v) = S (s + v) v
effect Sp (S s v) = S (s + v + 1) (v + 1)
effect Sl (S s v) = S (s + v - 1) (v - 1)

type Sol  = ([C],S)

data Env = Env P P P

win :: Env -> S -> Bool
valid  :: Env -> S -> C -> Bool

win (Env r g p) (S x v) = v == 0 && x >= r + g && x < r + g + p

valid (Env r g p) (S x v) ((/=) J -> c) = 
    v >= 0 && 
    (       (c && x < r) || 
        (   x >= r + g && x < r + g + p
        )
    )

mjudge zs e s cs = catMaybes . map judge 
    where
    judge j =   if valid e s' j && not (s' `S.member` zs) 
                then Just  (j:cs,s') 
                else Nothing
        where s' = effect j s

move :: S.Set S -> Env -> Sol -> [Sol]
move z e ([],s) = mjudge z e s [] [Sl,W,Sp,J]
move z e (cs@(elem J -> True), s) = mjudge z e s cs [Sl]
move z e (Sl:cs, s) = mjudge z e s (Sl:cs) [Sl,W,Sp,J]
move z e (W:cs, s) = mjudge z e s (W:cs) [W,Sp,J]
move z e (Sp:cs, s) = mjudge z e s (Sp:cs) [Sp,W,J]

produce e x = produce' S.empty $ [([],x)] where
    produce' _ [] = [] -- end of search
    produce' zs ns = let
        ns' = ns >>= move zs e
        in ns' ++ produce' (foldr S.insert zs $ map snd ns') ns' 

solution e x = reverse <$> fst <$> find (win e . snd) (produce e x)

what W = "WAIT"
what Sl = "SLOW"
what Sp = "SPEED"
what J = "JUMP"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [r,g,p] <- replicateM 3 (read <$> getLine) 
    let e = Env r g p
    [v,x] <- replicateM 2 (read <$> getLine)
    let s = fromJust $ solution e (S x v)
    forM_ s $ \w -> do
        putStrLn $ what w
        replicateM 2 getLine
        


