{-# language ViewPatterns #-}

type N = Int
type D = Int

data T a = T N [(a, T)]

other n (i,j) 
    | i == n = j
    | otherwise = i

mkT :: N -> [(N,N)] -> T ()
mkT n ns = let
    (map (other n) -> ms,ns') = partition (\(i,j) -> i==n || j == n) ns
    in T n . zip (repeat ()) $ foldr mkT ns' ms

inter xs xs' 
            | mx > mn' ||  mx' > mn = 0
            | otherwise = max (mn' - mx) (mn - mx')
            where
                (mn,mx) = minmax xs
                (mn',mx') = minmax xs'
                
minmax (x:xs) = foldl' f (x,x) xs where
    f (mn,mx) x = (min x mn,max x mx)
