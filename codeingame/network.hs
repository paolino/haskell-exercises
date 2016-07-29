{-# language ViewPatterns #-}

import Data.List


dist (unzip &&& length -> ((xs,ys),l)) = sum (map (abs . subtract median) ys) + s where
    median = sort ys !! (l `div` 2)
    s = mx - mn  where (mn,mx) = minmax xs
    minmax (x:xs) = foldl' f (x,x) xs where
        f (mn,mx) x = (min x mn,max x mx)

