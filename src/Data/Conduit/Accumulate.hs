{-# LANGUAGE LambdaCase #-}

module Data.Conduit.Accumulate (
  accumulate
) where

import           Control.Monad.State (get, modify, put)
import           Data.Conduit
import           Data.Conduit.Lift   (evalStateC)

accumulate :: Monad m => ConduitT (Flush a) [a] m ()
accumulate =
  evalStateC [] $
    awaitForever $ \case
      Chunk item -> modify (item :)
      Flush      -> dget >>= yield . reverse
  where
    dget = do
      s <- get
      put []
      return s
