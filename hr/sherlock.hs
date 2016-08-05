import Control.Monad.Fix 
import Control.Monad
import Control.Applicative

table :: [[([Int],[Int])]]
table = fix $ (:) (repeat (1:repeat 0,repeat 0)) . map g where
    g t = fix $ (:) (repeat 0 ,1:repeat 0) . zipWith f (tail t) 
    f (l',u') (l,u) = (zipWith (+) l (0:u), zipWith (+) (0:l') u')

main = do
    n <- readLn
    xs <- replicateM n $ map read <$> words <$> getLine
    forM_ xs $ \[r,c,l] -> print $ 
        sum . take (l + 1) . uncurry (zipWith (+)) 
            $ table !! (r - 1) !! (c - 1)
