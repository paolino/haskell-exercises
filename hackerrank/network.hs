import Data.List

inter xs xs' 
            | mx > mn' ||  mx' > mn = 0
            | otherwise = max (mn' - mx) (mn - mx')
            where
                (mn,mx) = minmax xs
                (mn',mx') = minmax xs'
                
minmax (x:xs) = foldl' f (x,x) xs where
    f (mn,mx) x = (min x mn,max x mx)
