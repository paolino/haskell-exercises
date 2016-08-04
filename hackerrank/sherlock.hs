t :: [[([Int],[Int])]]
t = repeat (1:repeat 0,repeat 0) : map g t

g :: [([Int],[Int])] ->  [([Int],[Int])]
g t = let
    li = (repeat 0 ,1:repeat 0) : zipWith f li t 
    f (l,u) (l',u') = (zipWith (+) l (0:u), zipWith (+) (0:l') u')
    in li
