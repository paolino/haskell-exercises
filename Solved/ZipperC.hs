{-# language InstanceSigs, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, ViewPatterns #-}

module Solved.ZipperC where

import Solved.Zipper
import Closures

data ZC a = ZC 
    {   insertC :: a -> ZC a   
    ,   focusC :: a
    ,   rightC :: Maybe (ZC a)
    ,   leftC :: Maybe (ZC a)
    ,   modifyC :: (a -> Maybe a) -> Maybe (ZC a)
    ,   toListC :: [a]
    }

makeZC :: a -> ZC a
makeZC = make . makeZ where
    make z = ZC (make . insert z) (focus z) (make <$> right z) (make <$> left z) (fmap make . modify z) (toList z)

instance Algebraic (ZC a) where
    type Algebraic (ZC a) = [a]
            


                

