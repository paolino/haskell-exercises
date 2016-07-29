{-# language TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, ViewPatterns #-}

module Solved.ZipperC where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import GHC.Exts

import Solved.Zipper

import Closures

data ZC a = ZC 
    {   insertC :: a -> ZC a   
    ,   focusC :: a
    ,   rightC :: Maybe (ZC a)
    ,   leftC :: Maybe (ZC a)
    ,   modifyC :: (a -> Maybe a) -> Maybe (ZC a)
    }

home x = maybe x home $ leftC x
collect x = focusC x: maybe [] collect (rightC x)

instance Closure (ZC a) where 
    type Expose (ZC a) = NonEmpty a
    toExpose = fromList . collect . home
    fromExpose = make . fromExpose  where
        make z = ZC (make . insert z) (focus z) (make <$> right z) (make <$> left z) (fmap make . modify z) 



                

