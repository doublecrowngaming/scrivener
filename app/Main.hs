{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM.TMChan (newTMChanIO)
import           Control.Monad                 (void)
import           Data.Conduit
import           Data.Conduit.Accumulate       (accumulate)
import           Data.Conduit.Binary           (lines)
import           Data.Conduit.Buffered
import           Data.Conduit.Combinators      (stdin)
import           Data.Conduit.LogDNA           (AppName (..), Env (..),
                                                IngestToken (..), getHostname,
                                                logDNA, logLine)
import           Options.Applicative
import           Prelude                       hiding (lines)


data Parameters = Parameters {
  token   :: IngestToken,
  appName :: AppName,
  env     :: Env
}

parameters :: Parser Parameters
parameters = Parameters <$> parseToken <*> parseAppName <*> parseEnv
  where
    parseToken   = argument (IngestToken <$> str) (metavar "API_TOKEN")
    parseAppName = argument (AppName <$> str) (metavar "APP_NAME")
    parseEnv     = option (Env <$> str) $
                         long "environment"
                      <> metavar "ENVIRONMENT"
                      <> showDefaultWith (show . unEnv)
                      <> value (Env "default")
                      <> help "Set the environment the logs come from"

main :: IO ()
main = do
  chan           <- newTMChanIO
  hostname       <- getHostname
  Parameters{..} <- customExecParser p (info (parameters <**> helper) fullDesc)

  -- Fork off input listener thread
  void . forkIO $ runConduitRes (stdin .| lines .| logLine appName env .| bufferSink chan)

  -- Start output emitter
  runConduitRes (bufferSource chan .| accumulate .| logDNA token hostname)

  where
    p = prefs (showHelpOnEmpty <> showHelpOnError)
