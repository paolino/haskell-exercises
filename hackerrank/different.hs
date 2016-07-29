{-# language OverloadedLists #-}
import Control.Monad
import System.IO
import Control.Applicative
import qualified Data.Vector as V
import Data.List as L


different mn mk = let
    t = L.replicate mn 1 : (L.zipWith (\n l -> L.replicate n 0 L.++ f n l) [1..mk] t)
    f n l = (:) 1 $ L.zipWith (+) (f n l) (L.drop n l)
    in \n k -> t L.!! k L.!! n


main = do
    ws <- readLn >>= flip replicateM (map read <$> words <$> getLine)
    let r = uncurry different $(\[mn,mk] -> (mn,mk)) . map maximum $ transpose ws
    forM_ ws $ \[n,k] ->  print $ (`rem` (10 ^ 8 + 7)). r n $ k
