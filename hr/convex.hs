{-# language ViewPatterns #-}
{- perimeter of convex hull of points -}
import Data.List 

type P = (Double,Double)

(.-.) :: P -> P -> P
(x1, y1) .-. (x2, y2) = (x1 - x2, y1 - y2)

(.*.) :: P -> P -> Double
(x1, y1) .*. (x2, y2) = x1 * y2 - x2 * y1

data T = CW | CCW deriving Show

turn :: P -> P -> P -> T
turn p1 p2 p3  = case (p2 .-. p1) .*. (p3 .-. p1) `compare` 0 of
    GT -> CCW
    _ -> CW

chain :: [P] -> [P]
chain = chain' []
  where
    chain' pss [] = pss -- spit the output
    chain' pss xss@(x:xs) =
        case pss of
            p:dps@(pp:ps) -> case turn pp p x of
                CW -> chain' dps xss -- drop an output
                CCW -> passthrough
            _ -> passthrough
        where passthrough = chain' (x:pss) xs

hull :: [P] -> [P]
hull [] = []
hull [p] = [p]
hull (sort -> ss) = tail (chain ss) ++ tail (chain $ reverse ss)

perimeter :: [P] -> Double
perimeter = sum . (zipWith dist  <*> shift)
    where 
    shift = flip (++) <$> return <$> head <*> tail
    dist (x,y) ((-) x -> dx, (-) y -> dy) = sqrt $ dx ^ 2 + dy ^ 2



