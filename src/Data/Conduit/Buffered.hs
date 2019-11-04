{-# LANGUAGE LambdaCase #-}

module Data.Conduit.Buffered (bufferSink, bufferSource) where

import           Conduit
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM   (atomically)
import           Data.Conduit.TMChan
import           Data.Maybe               (isNothing)
import           Prelude

-- There are two flusher threads, a fast one and a slow one. The
-- fast flusher batches up bursts of messages coming across the
-- channel. The slow flusher triggers when there's a sustained
-- message flow to ensure channel latency has an upper bound.
--
-- When the fast flusher triggers it must kill the slow flusher
-- to avoid superfluous flushes.
data Flushers = Initial | Flushers (Async ()) (Async ())

updateFlushers :: MonadIO io => TMChan (Flush i) -> Flushers -> io Flushers
updateFlushers channel flushers = liftIO $
  case flushers of
    Initial -> do
      slow <- delayedFlush slowDelay Nothing
      fast <- delayedFlush fastDelay (Just slow)

      return $ Flushers fast slow
    (Flushers fast slow) -> do
      cancel fast

      slowIsLive <- isNothing <$> poll slow

      if slowIsLive then
        Flushers <$> delayedFlush fastDelay (Just slow) <*> pure slow
      else
        updateFlushers channel Initial

  where
    delayedFlush :: Double -> Maybe (Async ()) -> IO (Async ())
    delayedFlush n toCancel = async $ do
      threadDelay (round $ n * 1000 * 1000)
      atomically (writeTMChan channel Flush)

      mapM_ cancel toCancel

    slowDelay = 3
    fastDelay = 0.5

bufferSink :: (MonadIO io, MonadResource io) => TMChan (Flush i) -> ConduitT i Void io ()
bufferSink channel = listener Initial .| sinkTMChan channel
  where
    listener flushers =
      await >>= \case
        Just datum -> do
          yield (Chunk datum)
          updateFlushers channel flushers >>= listener
        Nothing -> return ()

bufferSource :: (MonadIO io, MonadResource io) => TMChan (Flush i) -> ConduitT () (Flush i) io ()
bufferSource = sourceTMChan
