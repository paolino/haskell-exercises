import Data.List
import Data.Ord
import Control.Monad
import Control.Applicative
import Data.Maybe

type S = [Int]
type P = Int -- length of bars

enumerate :: P -> [S]
enumerate n = sortBy (comparing sum) $ sequence [[1..n],[0..n],[0..n]]

realize :: P -> S -> [S -> S]
realize n = zipWith ((zipWith min .) . keep n) [0..] where
    keep n y x = replicate y n ++ repeat x

win :: P -> [S] -> S -> Bool
win n losers x = any id $ [r x == l | l <- losers, r <- realize n l]

losers :: P -> S -> [S]
losers n z = (z :) . catMaybes . snd . mapAccumL f [z] $ enumerate n where
        f ls s 
            | win n ls s = (ls,Nothing)
            | otherwise = (s:ls,Just s)
                  
main = do 
    n   <- readLn
    replicateM n $ do
        w <- map read <$> words <$> getLine
        putStrLn $ if w `elem` losers 25 [1,0,0] then "LOSE" else "WIN"
    
