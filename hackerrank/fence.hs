import Data.List
import Control.Applicative

normalize n xs = let m' = minimum xs in 
     (n + m',map (subtract m') xs)

pick :: (Int,[Int]) -> [(Int,[Int])]
pick (n,xs) = map (normalize n). filter ((>0) . head) . groupBy (((>0) .) . (*)) $ xs

main = do
    _ <- getLine

    xs <-  map read <$> words <$> getLine 
    print . maximum . map (\(n,xs) -> n * length xs) . concat $ takeWhile (not.null) $ iterate (>>= pick) $ pick (normalize 0 xs) 


