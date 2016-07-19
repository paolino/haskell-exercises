{-# language ViewPatterns, TypeFamilies #-}

module Solved.Zipper where

import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)), fromList)
import Closures

data Z a = Z [a] a [a] deriving Show 

insert (Z xs f ys) x = Z xs x (f:ys)

focus (Z _ x _) = x

left (Z [] _ _) = Nothing
left (Z (x:xs) f ys) = Just . Z xs x $ f:ys

right (Z _ _ []) = Nothing
right (Z xs f (y:ys)) = Just . Z (f:xs) y $ ys


modify (Z xs x ys) f = case f x of
    Nothing -> case ys of 
        [] -> Nothing
        (y:ys) -> Just $ Z xs y ys
    Just x' -> Just $ Z xs x' ys

border f x = maybe x (border f) $ f x 

instance Closure (Z a) where
    type Expose (Z a) = NonEmpty a
    fromExpose (x :| xs) = border left $ Z [] x xs
    toExpose z = let Z [] x xs = border left z in x :| xs
    
