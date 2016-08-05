import Control.Monad
import Control.Applicative

check :: [Int] -> Bool 
check [] = True
check (x:xs) = let
    (ls,rs) = break (> x) xs
    in all (> x) rs && check ls && check rs

conv True = "YES"
conv False = "NO"

main = do
    n <- readLn
    replicateM_ n $ do
        _ <- getLine
        check <$> map read . words <$> getLine >>= putStrLn . conv
    
