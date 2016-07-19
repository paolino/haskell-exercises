module Specs.Lib where

import Data.List
import Test.QuickCheck
import  qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))


move 0 f x = Just x
move n f x =  f x >>= move (n - 1) f

deleteIndex 0 (x:xs) = xs
deleteIndex n (x:xs) = x:deleteIndex (n - 1) xs

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$>arbitrary <*> arbitrary
