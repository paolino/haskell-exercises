import qualified Data.Map as M
import Control.Monad
import Data.List
import Data.Maybe 
import Data.Ord

points = [("eaionrtlsu",1),
            ("dg",2),
            ("bcmp",3),
            ("fhvwy",4),
            ("k",5),
            ("jx",8),
            ("qz",9)
            ]
compute :: String -> Int
compute = sum . map f where
    f x = snd . fromJust $ find (elem x . fst) points

data T = T (M.Map Char T) | E deriving Show

merge :: T -> T -> T
merge E t = t
merge t E = t
merge (T t') (T t'') = T $ M.unionWith merge t' t''

fromString :: String -> T 
fromString [] = E
fromString (c:cs) = T (M.singleton c $ fromString cs)

insertT :: String -> T -> T
insertT = merge . fromString

match ys xs E = [reverse xs]
match ys xs (T m) = do 
    y <- ys
    guard $ y `M.member` m
    match (delete y ys) (y:xs) (m M.! y)

main = do
    n <- readLn
    ws <- replicateM n getLine
    ls <- getLine
    putStrLn $ head . intersect ws . head . groupBy ((==) `on` compute). sortBy (flip $ comparing compute) . sort . match ls [] $ foldr insertT E ws 
