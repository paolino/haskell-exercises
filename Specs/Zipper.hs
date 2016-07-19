{-# language ScopedTypeVariables,FlexibleContexts #-}

module Specs.Zipper where

import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Maybe
import Data.List
import Control.Arrow hiding (right,left)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Exts
import Specs.Lib
import Control.Lens
import Closures

import Solved.Zipper
-- import Exercises.Zipper

------------------------------------------------
--- testing spec
-----------------------------------------------

sampleZ :: (Z Int -> IO a) -> IO a
sampleZ f = generate (fromExpose <$> arbitrary) >>= f 

wrapList f = fromList . f . toList

main = hspec $ do
    describe "Zipper invariants" $ do
        it "checks Closure instance" $ do
            property $ \x -> toExpose (fromExpose x :: Z Int) `shouldBe` x
        it "checks focus" $ do 
            property $ sampleZ $ \x -> do
                (NE.map focus . NE.unfold (id &&& right) $ x)  `shouldBe` (toExpose x)
        it "checks borders" $ do
            property $ sampleZ $ \x -> do
                focus (border right x) `shouldBe` NE.last (toExpose x)
                focus (border left $ border right x) `shouldBe` NE.head (toExpose x)
        it "checks modify as set" $ do
            property $ sampleZ $ \x -> do
                n <- generate $ choose (0, NE.length (toExpose x) - 1)
                let r = fromJust . move n right
                m <- generate arbitrary
                toExpose <$> modify (r x) (const . Just $ m) `shouldBe` Just (set (ix n) m $ toExpose x)
        it "checks modify as delete" $ do
            property $ sampleZ $ \x -> do
                let l = NE.length (toExpose x)
                n <- generate $ choose (0,l  - 1)
                let r = fromJust . move n right
                toExpose <$> modify (r x) (const Nothing) `shouldBe` 
                        if n == l - 1 then Nothing else Just (wrapList (deleteIndex n) $ toExpose x)

                


                

