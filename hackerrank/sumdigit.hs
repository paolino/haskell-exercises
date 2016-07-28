import Data.List 
import Control.Applicative

digits = unfoldr f where
    f 0 = Nothing
    f n = Just . swap $ n `divMod` 10

super [n] = n
super ns = super . digits . sum $ ns

superdigit = super . digits

main = do
    [n,k] <- map read <$> words <$> getLine
    print $ superdigit (superdigit n * k) 

swap (a,b) = (b,a)
