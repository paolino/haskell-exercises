import Data.List

descendingOrder :: Integer -> Integer
descendingOrder = fromDigits . sort . toDigits

toDigits = unfoldr f where
    f 0 = Nothing
    f n = Just (swap $ n `divMod` 10)
    swap (x,y) = (y,x)
  
fromDigits = foldr (\n m -> m * 10  + n) 0
