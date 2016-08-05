
import Data.Char
import Numeric
import Data.List
import Control.Arrow


 
base2 c =  (showIntAtBase 2 intToDigit $ ord c) ""

encode = (>>= \(l,n) -> (if l == '0'  then "0 " else "00 ") ++ replicate n '0' ++ " ") . map (head &&& length) . group 
