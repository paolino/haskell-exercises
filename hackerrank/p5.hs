import Data.Array

newtype Query = Query (Int -> (Int,Query))
mkQuery = let   
    response a = ((a !), make a)
    make a = Query $ response (listArray (0,n) $ map   

pn = inside where
    inside :: Int -> Int
    inside 1 = 1
    inside n = if n > k pn (n - 1) + 2*n + (n - 2)

pna = (map inside [0..] !!) where
    inside :: Int -> Int
    inside 1 = 1
    inside n = pna (n - 1) + 2*n + (n - 2)

main = do
    (t:ns) <- map read <$> lines <$> getContents
    mapM_ (print . pn
