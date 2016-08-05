import Data.List
import Data.Ord
import Data.Function
import Control.Monad
import Control.Applicative


perimeter zs@(x:xs) = sum . map (uncurry dist) . (zip <*> ((++ [x]) . tail)) $ zs

dist (x,y) (x',y') = sqrt $ d x x' + d y y' where
    d x x' = (x - x') ^ 2

mkT [x,y] = (x::Double,y)

area zs@(x:xs) = abs . (/2) . sum . map (uncurry cross) . (zip <*> ((++ [x]) . tail)) $ zs
    where cross (x,y) (x',y') = (x'+x) * (y'-y)
main = do
        t <- readLn
        z <- area <$> map (mkT . map read . words) <$> replicateM t getLine     
        print z
