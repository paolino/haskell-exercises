{-# language InstanceSigs, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, ViewPatterns #-}

module Exercises.ZipperC where

import Exercises.Lib (placeholder)
import Exercises.Zipper 

-----------------------------------------------------
---- Comonadic zipper . 1 star  ---------------------
-----------------------------------------------------

-- wrap up the solution from Zipper module to hide Z type to the user, leaving only the following API. 

data ZC a = ZC 
    {   insertC :: a -> ZC a   
    ,   focusC :: a
    ,   rightC :: Maybe (ZC a)
    ,   leftC :: Maybe (ZC a)
    ,   modifyC :: (a -> Maybe a) -> Maybe (ZC a)
    ,   toListC :: [a]
    }

makeZC :: a -> ZC a
makeZC = placeholder 

            


                

