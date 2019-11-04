{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM.TMChan (newTMChanIO)
import           Control.Monad                 (void)
import           Data.Conduit
import           Data.Conduit.Binary           (lines)
import           Data.Conduit.Buffered
import           Data.Conduit.Combinators      (stdin)
import           Data.Conduit.LogDNA           (AppName (..), IngestToken (..),
                                                getHostname, logDNA, logLine)
import           Options.Applicative
import           Prelude                       hiding (lines)


data Parameters = Parameters {
  token   :: IngestToken,
  appName :: AppName
}

parameters :: Parser Parameters
parameters = Parameters <$> parseToken <*> parseAppName
  where
    parseToken   = argument (IngestToken <$> str) (metavar "API_TOKEN")
    parseAppName = argument (AppName <$> str) (metavar "APP_NAME")

main :: IO ()
main = do
  chan           <- newTMChanIO
  hostname       <- getHostname
  Parameters{..} <- customExecParser p (info (parameters <**> helper) fullDesc)

  -- Fork off input listener thread
  void . forkIO $ runConduitRes (stdin .| lines .| logLine appName .| bufferSink chan)

  -- Start output emitter
  runConduitRes (bufferSource chan .| logDNA token hostname)

  where
    p = prefs (showHelpOnEmpty <> showHelpOnError)
