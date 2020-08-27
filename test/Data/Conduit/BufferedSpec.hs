{-# LANGUAGE LambdaCase #-}

module Data.Conduit.BufferedSpec (
  spec
) where

import           Conduit
import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan (newTMChanIO)
import           Data.Conduit.Buffered         (bufferSink)
import           Data.Conduit.TMChan

import           Test.Hspec

spec :: Spec
spec =
  it "flushes after no input for some time" $ do
    chan <- newTMChanIO

    let sink = bufferSink chan

    runConduitRes (yieldMany [1, 2, 3 :: Int] .| sink)

    threadDelay (300 * 1000)

    runConduitRes (yieldMany [4, 5, 6] .| sink)

    threadDelay (250 * 1000)

    contents <- atomically $ do
      closeTMChan chan
      drainTMChan chan

    contents `shouldBe` [Chunk 1, Chunk 2, Chunk 3, Chunk 4, Chunk 5, Chunk 6, Flush]

  where
    drainTMChan :: TMChan a -> STM [a]
    drainTMChan tmchan = doDrain []
      where
        doDrain acc =
          readTMChan tmchan >>= \case
            Nothing -> return $ reverse acc
            Just val -> doDrain (val : acc)
