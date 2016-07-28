{-# language ViewPatterns #-}

import Prelude hiding (Left,Right)
import System.IO
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Array
import Control.Arrow  hiding (loop)
import Data.List

data M = Z | O | Tw | Th | Fo | Fi | Si | Se | Ei | N | Te | El | Twe | Thi deriving (Enum,Show)

data Ex = Top | Left | Right | Down deriving (Read,Eq,Show)

opposite Top = Down
opposite Down = Top
opposite Left = Right
opposite Right = Left 

ways Z = []
ways O = [(Top,Down),(Left,Down),(Right,Down)] 
ways Tw = [(Left,Right),(Right,Left)]
ways Th = [(Top,Down)]
ways Fo = [(Top,Left),(Right,Down)]
ways Fi = [(Top,Right),(Left,Down)]
ways Si = ways Fo
ways Se = [(Top,Down),(Right,Down)]
ways Ei = [(Right,Down),(Left,Down)]
ways N = [(Top,Down),(Left,Down)]
ways Te = [(Top,Left)]
ways El = [(Top,Right)]
ways Twe = [(Right,Down)]
ways Thi = [(Left,Down)]

input :: Ex -> M -> Bool
input e = (elem e) . map fst . ways
output e = (elem e) . map snd . ways

bind :: Ex -> (Ex,M) -> M -> Bool
bind l ((output l . snd &&& (\(e,m) -> (e,l) `elem` ways m)) -> (True,True)) (input (opposite l) -> True) = True
bind _ _ _  = False

type P = (Int,Int)

move :: P -> Ex -> P
move (i,j) Left = (i - 1,j)
move (i,j) Right = (i + 1,j)
move (i,j) Down = (i,j+1)

check :: Array (Int,Int) M -> (Ex,P) -> [P]
check a (e,p) = map fst . filter (\(j,l) -> bind l (e,a ! p) (a ! j))  . filter (inRange (bounds a) . fst) $ map (move p &&& id)[Left,Right,Down]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    [w,h] <- map read <$> words <$> getLine
    m <- listArray ((0,0),(w - 1, h -1)) <$> map toEnum <$> concat <$> transpose <$> replicateM h (map read <$> words <$> getLine)
        -- represents a line in the grid and contains W integers. Each integer represents one room of a given type.
    _ <- getLine
    loop m


decap (x:xs) = x:map toLower xs

loop :: Array (Int,Int) M -> IO ()
loop m = do
    [read -> w, read -> h, read . decap -> d] <- words <$> getLine
     
    -- hPutStrLn stderr "Debug messages..."
    
    -- One line containing the X Y coordinates of the room in which you believe Indy will be on the next turn.
    
    putStrLn $ intercalate " " . map show . (\[(x,y)] -> [x,y]) . check m $ (d,(w,h))
    
    loop m

a :: Array (Int,Int) M 
a = array ((0,0),(2,2)) [((0,0),Z),((0,1),Z),((0,2),Z),((1,0),Th),((1,1),Th),((1,2),Th),((2,0),Z),((2,1),Z),((2,2),Z)]
