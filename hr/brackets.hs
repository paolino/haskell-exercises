{-# language ViewPatterns #-}
import Control.Monad


close '(' = ')'
close '[' = ']'
close '{' = '}'
f Nothing _ = Nothing
f (Just xs) c@((`elem` "{[(") -> True)  = Just $ c : xs
f (Just []) ((`elem` ")]}") -> True) = Nothing
f (Just (x:xs)) c@((`elem` ")]}") -> True) 
    | c == close x = Just xs
    | otherwise = Nothing
f x y = x

g = (== ok) . fmap last . sequence . scanl f ok where
    ok = Just ""

main = do
    n <- readLn
    xs <- replicateM n getLine
    forM_ xs $ \x -> putStrLn $ case g x of
        True -> "YES"
        _ -> "NO"
