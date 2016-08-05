{-# language DeriveFunctor, ViewPatterns #-}
import Data.Attoparsec.Text
import Data.Ratio
import Control.Applicative
import Data.Text (pack)
import Data.List
import Data.Ord
import Data.Function
import Control.Monad
import Data.Maybe
import Control.Arrow

mSepBy1 :: Parser a -> Parser (a -> a) -> (a -> a) -> Parser [a]
mSepBy1 f gs z = do
    x <- z <$> f 
    (x:) <$> option [] (gs >>= \g -> mSepBy1 f gs g <|> return [])

groupOp :: ([a] -> b) -> Parser a -> Parser (a -> a)  -> Parser b
groupOp w f z  = fmap w $ do
    x <- f
    s <- z
    xs <- mSepBy1 f z s
    return $ x:xs

parens f = do
    char '('
    x <- f
    char ')'
    return x

expon = char '^' >> decimal

num :: Parser Rational
num = fmap (%1) $ do
    b <- decimal <|> parens (signed decimal)
    e <- option 1 expon 
    return $ b ^ e

data E a
    = T {constant :: a, expo :: Int}
    | Plus [E a]
    | Mult [E a] 
    | Negate (E a)
    | Recip (E a)
    deriving (Eq,Show,Functor)

isT (T _ _) = True
isT _ = False

numterm = do 
    x <- num 
    n <- option 0 $ do
        char 'x'
        option 1 expon
    return $ T x n

term = (<|> numterm) $ do
    x <- option 1 num
    n <- char 'x' >> option 1 expon
    return $ T x n

plusR = plus $ multR <|>   parens parseE' <|> term where
    plus f = groupOp Plus f $  id <$ char '+' <|> Negate <$ char '-'

multR = mult $ parens parseE' <|> term where
    mult f = groupOp Mult f $ id <$ char '*' <|> Recip <$ char '/' <|> id <$ return ()

parseE' :: Parser (E Rational)
parseE' =  plusR <|> multR <|> parens parseE' <|> term

parseE = maybeResult . parse parseE' . pack . (++ "\n") . checkNegStart. Prelude.filter ((/=) ' ') where
    checkNegStart ('-':xs) = '0':'-':xs
    checkNegStart xs = xs

simply t@(T k n) = T k n

simply (Plus [x]) = simply x
simply (Plus (T 0 _:rs)) = simply $ Plus rs
simply (Plus (Plus es:rs)) = simply . Plus $ rs ++ es
simply (Plus es@(all isT -> True)) = let
    ts = groupBy ((==) `on` expo). sortBy (comparing expo) $ es
    f = foldr1 (\(T k1 _) (T k2 n) -> T (k1 + k2) n) 
    in case map f ts of
        [x] -> x
        xs -> Plus xs
simply (Plus (e:es)) = simply . Plus . map simply $ es ++ [e]

simply (Mult [x]) = simply x
simply (Mult (T 1 0:rs)) = simply $ Mult rs
simply (Mult (Plus es1:Plus es2:rs)) = simply $ Mult (Plus [Mult [e1,e2] | e1 <- es1 , e2 <- es2]:rs)
simply (Mult (t@(T k n):Plus es:rs)) = simply $ Mult (Plus (map (Mult . (t:) . return) es):rs)
simply (Mult ms@(all isT -> True)) = T (product $ map constant ms) (sum $ map expo ms)
simply (Mult (e:es)) = simply . Mult . map simply $ es ++ [e]

simply (Negate m) = negate <$> simply m

simply (Recip m) = recip <$> simply m

realize (x: (filter ((/=0) . fst) -> xs)) = flip (foldl g) xs $ f x where
    f (-1,n) = "-" ++ n
    f (0,n) = if null xs then "0" else ""
    f (_,n) = n
    g z (-1,n) = z ++ " - " ++ n
    g z (0,_) = z
    g z (1,n)  = z ++ " + " ++ n
   
pprint (Plus xs) =  realize . map f . sortBy (flip $ comparing expo) $ xs   where
    f (T k@(abs -> 1)  0) = second (++ "1") $ z k
    f (T 0 _) = (0,"")
    f (T k 0) = z k
    f (T k 1) = second (++ "x") $ z k 
    f (T k n) = second (++ ("x^" ++ show n)) $ z k
    z (numerator -> n) 
        | abs n == 1 = (signum n, "")
        | otherwise = (signum n, show (abs n))

pprint x = pprint (Plus [x])

main = do
    n <- readLn
    xs <- replicateM n getLine
    forM_ xs $ putStrLn . fromJust . fmap (pprint . simply) . parseE

