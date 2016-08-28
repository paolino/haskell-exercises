import Data.List
import Control.Arrow

primes= sieve $ 2:[3,5..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]

factor 1 = Nothing
factor n = (\t -> (t,n `div` t)) <$> find ((==0) . rem n) primes 

factors = map (head &&& length) . group . unfoldr factor


