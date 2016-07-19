{-# language InstanceSigs, ScopedTypeVariables,  ViewPatterns #-}

{-# language TypeFamilies #-}

module Exercises.Zipper where

import Exercises.Lib (placeholder)
import Data.List.NonEmpty
import Closures

-- Ex 1. Zipper for list, algebraic. Don't lookup, you cheater
-- define the constructor for Z a, and the implementation for its API 
-- A zipper is an updatable structure with a suitable API to move the updating/reading focus around.
-- In OOP you can implement it with a mutable array with a mutable index on its elements indicating the actual focus.

----------------------------------
---- Algebraic. 0 star
-----------------------------------

-- define your constructor(s) here 
data Z a = Z [a] a [a] deriving Show 


-- insert at focused place, focus should return this element now, and right should move focus on the previous one
insert :: Z a -> a -> Z a
insert = placeholder


-- the focused element
focus :: Z a -> a
focus = placeholder


-- move focus left, when possible
left :: Z a -> Maybe (Z a)
left = placeholder


-- move focus right, when possible
right :: Z a -> Maybe (Z a)
right = placeholder


-- update the focused value, if result of modification is Nonhing , element is removed. Return Nonhing when deleting the last remaining element
modify :: Z a -> (a -> Maybe a) -> Maybe (Z a)
modify = placeholder

-- bring the focus to the most left or right position
border  :: (Z a -> Maybe (Z a))  -- movement (left or right)
        -> Z a -- start pos
        -> Z a -- end pos
border = placeholder 

-- implement serialization / de-serialization of 'Z a'. The focus position after fromExpose should be border left
instance Closure (Z a) where
    type Expose (Z a) = NonEmpty a
    fromExpose :: NonEmpty a -> Z a
    fromExpose = placeholder
    toExpose :: Z a -> NonEmpty a
    toExpose = placeholder

