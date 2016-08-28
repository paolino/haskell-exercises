import Control.Monad.Fix
import Control.Applicative

fact :: Int -> Integer
fact n = (!!n) . fix $ ((1:) . (1:). zipWith (*) [2..] . tail)

main = map read <$> words <$> getLine >>= \[m,n,c] -> print (fact $ m + n - c -1)
