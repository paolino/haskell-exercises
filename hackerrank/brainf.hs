{-# language TemplateHaskell, ViewPatterns #-}
import Control.Monad
import Data.Array
import Control.Lens
import Control.Lens.TH
import Data.Char
import Control.Applicative

matchR,matchL  :: Array Int Char -> Int -> Int
matchR a = matchR' 0 where
    matchR' k n = case a ! n of
        ']' -> case k of
            0 -> n
            k -> matchR' (k - 1) (n + 1)
        '[' -> matchR' (k + 1) (n + 1)
        _ -> matchR' k (n + 1)

matchL a = matchL' 0 where
    matchL' k n = case a ! n of
        '[' -> case k of
            0 -> n
            k -> matchL' (k - 1) (n - 1)
        ']' -> matchL' (k + 1) (n - 1)
        _ -> matchL' k (n - 1)

data Z = Z [Int] Int [Int]

right   (Z xs x (y:ys)) = Z (x:xs) y ys
left    (Z (x:xs) y ys) = Z xs x (y:ys)
inc     (Z xs z ys) = Z xs ((z + 1) `rem` 256) ys
dec     (Z xs z ys) = Z xs ((z + 255) `rem` 256) ys
put z   (Z xs _ ys) = Z xs z ys
value   (Z xs z ys) = z

data State = State 
    {   _inp :: String
    ,   _out :: String
    ,   _ip :: Int
    ,   _datas :: Z
    ,   _count :: Int
    }

makeLenses ''State

mv :: State -> State
mv = over ip (+1) 

incp :: State -> Maybe State
incp = Just . over count (+1)

execute :: Int -> Array Int Char -> State -> Maybe State
execute l _ ((>=l) . view count -> True) = Nothing
execute _ a s = incp $ case a ! view ip s of
    '>' -> mv . over datas right $ s
    '<' -> mv . over datas left $ s
    '+' -> mv . over datas inc $ s
    '-' -> mv . over datas dec $ s
    '.' -> mv . over out ((chr . value . view datas $ s):) $ s
    ',' -> let r:rs = s ^. inp
           in mv . over datas (put $ ord r) . set inp rs $ s
    '[' -> case value (s ^. datas) of
                0 -> over ip (matchR a . (+1)) s
                _ -> mv s
    ']' -> case value (s ^. datas) of
                0 -> mv s
                _ -> over ip (matchL a . (subtract 1)) s

run :: Int -> Array Int Char -> State -> Either String String
run l a s
   | inRange (bounds a) (s ^. ip) = maybe (Left output) (run l a) $ execute l a s
   | otherwise = Right output 
    where   output = reverse $ s ^. out

lA xs = listArray (0,length xs - 1) xs
isBf = (`elem` "><-+,.[]")
z0 = Z [] 0 (repeat 0)
s0 inp = State inp "" 0 z0 0
exceed output = unlines [output,"PROCESS TIME OUT. KILLED!!!"]

main = do
    [_,np] <-  map read <$> words <$> getLine
    inp <- getLine
    prg <- lA <$> filter isBf <$> unlines <$> replicateM np getLine
    putStrLn . either exceed id $ run 100000 prg (s0 inp)
