import System.IO
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Control.Arrow
import Control.Applicative
import Data.Ord

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering     
    -- x <- getLine
    -- ts <- words <$> getLine
    -- [] <- map read <$> words <$> getLine
    -- n <- readLn
    
    ls <- lines <$> getContents
    putStrLn $ intercalate " " . return $ zip el ls >>= \(ns,l) -> ns >>= \n -> return (l !! n)
    -- putStrLn 
    return ()
    
    
    
