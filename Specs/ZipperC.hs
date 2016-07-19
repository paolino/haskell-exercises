
{-# language ScopedTypeVariables, ViewPatterns #-}

import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Maybe
import Data.List

import Specs.Lib
import Solved.ZipperC

---------------------------
--- test specs
----------------
fromListC (reverse -> (x::Int):xs) = foldl insertC (makeZC x) xs

main = hspec $ do
    describe "ZipperC invariants" $ do   
        it "checks boot" $ do
            property $ \(x :: Int)  -> do
                let z = makeZC x
                focusC z `shouldBe` x
                toListC <$> leftC z  `shouldBe` Nothing
                toListC <$> rightC z `shouldBe` Nothing
        it "checks loading right" $ do
            property $ \x xs -> do
                let (y :: ZC Int) = foldl insertC (makeZC x) xs
                toListC y `shouldBe` (reverse $ x:xs)
        it "checks focus" $ do
            property $ \x xs -> do
                let y = fromListC $ x:xs
                focusC y `shouldBe` x
        it "checks modify" $ 
            property $ \x xs -> do
                let y = fromListC $ x:xs
                focusC <$> (modifyC y $ Just . (+1)) `shouldBe` Just (focusC y + 1)
        it "checks right . left" $ do
            property $ \x y -> do
                let z = fromListC $ x:[y]
                focusC z `shouldBe` focusC (fromJust (rightC z >>= leftC))
        it "checks delete" $ do
            property $ \x y xs -> do
                let ls = x:y:xs
                    l = length ls
                n <- generate $ choose (0,l - 2)
                let y = move n rightC (fromListC ls) >>= flip modifyC (const Nothing)
                toListC <$> y `shouldBe` Just (deleteIndex n ls)
                


