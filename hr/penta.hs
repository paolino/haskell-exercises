import Control.Monad
import Data.List
import qualified Data.Map as M

penta = a where
    a = 1 : zipWith (+) a [4,7..]

main = do
    xs <- readLn >>= flip replicateM readLn 
    
    let m = M.fromList $ zip [1..maximum xs] penta
    
    mapM_ (print . (M.!) m) xs


