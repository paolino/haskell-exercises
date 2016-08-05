{-# language ViewPatterns #-}
import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Applicative
import Data.Maybe

data T  = T Int [T] deriving (Show, Eq)
data Z  = Z T [C] deriving (Show)
data C  = C [T] Int [T] deriving (Show)

mkZ :: Int -> Z 
mkZ x = Z (T x []) []

focus :: Z -> Int
focus (Z (T x _) _) = x

type M = Z -> Maybe Z

down,right,up,delete :: M

insertL,insertR :: Int -> M

down (Z (T n []) bcs) = Nothing
down (Z (T n (c:cs)) bcs) = Just $ Z c (C [] n cs : bcs)

right (Z _ []) = Nothing
right (Z t (C ls n [] : bcs)) = Nothing
right (Z t (C ls n (r:rs) : bcs)) = Just $ Z r (C (t : ls) n rs : bcs)

left (Z _ []) = Nothing
left (Z t (C [] n rs : bcs)) = Nothing
left (Z t (C (l:ls) n rs : bcs)) = Just $ Z l (C ls n (t:rs) : bcs)

up (Z _ []) = Nothing
up (Z t (C ls n rs:tcs)) = Just $ Z (T n (reverse ls ++  t : rs)) tcs where

delete (Z t []) = Nothing
delete (Z t (C ls n rs:tcs)) = Just $ Z (T n (reverse ls ++ rs)) tcs 

insertL _ (Z t []) = Nothing
insertL x (Z t (C ls n rs:bcs)) = Just $ Z t (C (T x [] : ls) n rs : bcs)

insertR _ (Z t []) = Nothing
insertR x (Z t (C ls n rs:bcs)) = Just $ Z t (C ls n (T x [] : rs) : bcs)

insertC x (Z (T y []) cs) = Just $ Z (T y [T x []]) cs
insertC x t = down t >>= insertL x >>= up

change x (Z (T _ rs) bcs) = Z (T x rs) bcs

multi op n = (!! n) . iterate (>>= op) . Just
child n t = down t >>= multi right (n - 1)

parse =  
    pUp <|> pLeft <|> pRight <|>
    pDelete <|> pInsertR <|> pInsertL <|> pInsertC <|> pChange <|>
    pVisit <|> pPrint 
        where
        pUp = stringSP "visit parent" >> return (Just up)
        pLeft = stringSP "visit left" >> return (Just left)
        pRight = stringSP "visit right" >> return (Just right)
        pDelete = stringSP "delete" >> return (Just delete)
        pInsertR = stringSP "insert right" >> Just <$> insertR <$> readsP
        pInsertL = stringSP "insert left" >> Just <$> insertL <$> readsP
        pInsertC = stringSP "insert child" >> Just <$> insertC <$> readsP
        pChange = stringSP  "change" >> Just <$> fmap Just <$> change <$> readsP
        pVisit = stringSP "visit child" >> Just <$> child <$> readsP
        pPrint = stringSP "print" >> return Nothing
        stringSP x = string x >> skipSpaces
        readsP = readS_to_P reads

top z@(up -> Nothing) = z
top z = top $ fromJust (up z)

main = do
    let f _ 0 = return ()
        f z n = do
            (c,_):_ <- readP_to_S parse <$> getLine
            z' <- case c of 
                Nothing -> print (focus z) >> return z
                Just c -> return $ fromJust $ c z
            f z' (n - 1)
    readLn >>= f (mkZ 0)

