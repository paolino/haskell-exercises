import Control.Applicative
import Data.Array
import Control.Monad
import qualified Data.ByteString.Char8 as B
-- import Data.Attoparsec.ByteString


negMod x k = ((x `mod` k) + k) `mod` k
main = do
    [n,k,q] <- map read <$> words <$> getLine
    xs <- listArray (0,n - 1) <$> map (read :: String -> Int) <$> words <$> B.unpack <$> B.getLine
    replicateM q $ do
        l <- subtract k <$> readLn
        print $ xs ! (l `negMod` n)
