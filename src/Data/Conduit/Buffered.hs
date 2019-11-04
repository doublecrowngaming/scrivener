{-# LANGUAGE LambdaCase #-}

module Data.Conduit.Buffered (bufferSink, bufferSource) where

import           Conduit
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM   (atomically)
import           Data.Conduit.TMChan
import           Data.Maybe               (isNothing)
import           Prelude

data Flushers = Initial | Flushers (Async ()) (Async ())

updateFlushers :: MonadIO io => TMChan (Flush i) -> Flushers -> io Flushers
updateFlushers channel flushers = liftIO $
  case flushers of
    Initial -> do
      f10 <- delayedFlush 10 Nothing
      f1  <- delayedFlush 1 (Just f10)

      return $ Flushers f1 f10
    (Flushers f1 f10) -> do
      cancel f1

      f10live <- isNothing <$> poll f10

      if f10live then
        Flushers <$> delayedFlush 1 (Just f10) <*> pure f10
      else
        updateFlushers channel Initial

  where
    delayedFlush n toCancel = async $ do
      threadDelay (n * 1000 * 1000)
      atomically (writeTMChan channel Flush)

      mapM_ cancel toCancel

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
