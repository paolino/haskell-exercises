{-# language ViewPatterns #-}
import System.IO
import Control.Monad
import Data.Array
import Data.List
import Data.Maybe
import Control.Applicative

type P = (Int,Int)
type A = Array P Char
data D = S | E | N | W deriving (Eq, Show)

move S (i,j) = (i,j+1)
move N (i,j) = (i,j-1)
move W (i,j) = (i-1,j)
move E (i,j) = (i+1,j)

barrier = flip elem "X#"

next :: [D] -> A -> P -> D
next ds a p = snd . head . filter (not . barrier . fst)
    . map (\d -> (a ! move d p, d)) $ ds

start :: A -> P
start = fst . fromJust . find ((== '@') . snd) . assocs

tele :: A -> P -> P
tele x = case filter ((== 'T') . snd) . assocs $ x of
    [] -> error "no tele"
    s -> \z -> head $ filter (/= z) $ map fst s

data State = State {
    a :: A,
    b :: Bool,
    to :: [D],
    w :: D,
    p :: P
    } deriving (Show,Eq)

data Future = Future State | Back D | End 

play :: State -> Future
play (State a b to w p) = let
    p' = move w p
    in case a ! p' of
        ' ' -> Future $ State a b to w p' 
        '$' -> End 
        '#' -> Back $ next to a p
        'X' -> if b then Future $ State (a // [(p',' ')]) b to w p'
               else Back $ next to a p
        'S' -> Future $ State a b to S p'
        'E' -> Future $ State a b to E p'
        'N' -> Future $ State a b to N p'
        'W' -> Future $ State a b to W p'
        'I' -> Future $ State a b (reverse to) w p'
        'B' -> Future $ State a (not b) to w p'
        'T' -> Future $ State a b to w (tele a p')

run ss ((`elem` ss) -> True) = Nothing 
run ss s@(State a b to d p) = case play s of
    Future s' -> (d :) <$> run (s:ss) s'
    Back d' -> run ss $ State a b to d' p
    End -> Just [d]

display Nothing = putStrLn "LOOP"
display (Just xs) = mapM_ (putStrLn . f) $ xs where
    f S = "SOUTH"
    f E = "EAST"
    f N = "NORTH"
    f W = "WEST"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [l,c] <- map read <$> words <$> getLine
    a <- listArray ((1,1),(c,l)) <$> concat <$> transpose <$> replicateM l getLine
    display . run [] $ State a False [S,E,N,W] S (start a)
