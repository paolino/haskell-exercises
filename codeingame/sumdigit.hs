import Data.List 

digits = unfoldr f where
    f 0 = Nothing
    f n = Just (r,d) where
        (d,r) = n `divMod` 10

super [n] = n
super ns = super . digits . sum $ ns

superdigit = super . digits

 
