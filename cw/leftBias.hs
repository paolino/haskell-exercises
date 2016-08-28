
import Data.Monoid
import Data.List
import Data.Ord
import Control.Exception

leftBias g = g  . (fmap . fmap) (`mappend` GT)

assert' = flip assert $ return ()

main = do
    assert'  $ maximumBy (comparing length) ["ciao","wave"] == "wave"
    assert' $ leftBias maximumBy (comparing length) ["ciao","wave"] == "ciao"
    putStrLn "ok"
