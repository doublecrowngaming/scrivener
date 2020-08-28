{-# LANGUAGE ScopedTypeVariables #-}

module Data.Conduit.AccumulateSpec (
  spec
) where

import           Conduit
import           Data.Conduit.Accumulate (accumulate)

import           Test.Hspec
import           Test.QuickCheck         hiding (output)

spec :: Spec
spec = do
  it "produces no output until there is a Flush" $ property $ \(input :: [Int]) -> do
    let output = runConduit $
                  yieldMany (map Chunk input)
                  .| accumulate
                  .| sinkList

    output `shouldBe` [[]]

  it "produces the empty list when no data has been recieved before a Flush" $
    runConduit (yield Flush .| accumulate .| sinkList) `shouldBe` ([[[]]] :: [[[Int]]])

  it "produces output in order" $ property $ \(input :: [Int]) -> do
    let output = runConduit $
                  (yieldMany (map Chunk input) >> yield Flush)
                  .| accumulate
                  .| sinkList

    output `shouldBe` [[input]]
