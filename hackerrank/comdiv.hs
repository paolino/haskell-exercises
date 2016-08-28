import Control.Monad.Fix

gcd' x y = last . takeWhile (>0) . fix $ (x:) . (y:) . (zipWith rem <*> tail)

