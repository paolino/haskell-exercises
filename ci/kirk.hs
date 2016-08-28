import Data.Array
import Control.Arrow
import Data.List


parse :: Int -> String -> Array (Int,Int) Char
parse n = lines >>> transpose >>> concat >>> listArray ((1,1),(n,n))
