{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Conduit.LogDNA (
  logLine,
  logDNA,
  IngestToken(..),
  AppName(..),
  SourceName(..),
  getHostname
) where

import           Control.Monad            (unless, void)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Control.Monad.State      (get, modify, put)
import           Data.Aeson               (ToJSON (..), object, (.=))
import           Data.ByteString          (ByteString)
import           Data.Conduit
import           Data.Conduit.Combinators (mapM)
import           Data.Conduit.Lift        (evalStateC)
import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (decodeUtf8)
import           Data.Time.Clock          (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX    (utcTimeToPOSIXSeconds)
import           Network.HostName         (getHostName)
import           Network.HTTP.Req
import           Prelude                  hiding (mapM)
import           Web.HttpApiData          (ToHttpApiData)


newtype IngestToken = IngestToken {unIngestToken :: ByteString } deriving (Show)
newtype AppName     = AppName { unAppName :: Text } deriving (ToJSON, Show)
newtype SourceName  = SourceName { unSourceName :: Text } deriving (Show, ToHttpApiData)
data LogLine        = LogLine UTCTime AppName ByteString deriving Show

instance ToJSON LogLine where
  toJSON (LogLine time app bytes) =
    object [
        "line"      .= decodeUtf8 bytes,
        "timestamp" .= utcTimeToPOSIXSeconds time,
        "app"       .= app
      ]


logLine :: MonadIO io => AppName -> ConduitT ByteString LogLine io ()
logLine appName =
  mapM $ \l -> liftIO (LogLine <$> getCurrentTime <*> pure appName <*> pure l)


logDNA :: MonadIO io => IngestToken -> SourceName -> ConduitT (Flush LogLine) o io ()
logDNA ingestToken hostname =
  evalStateC [] $
    awaitForever $ \case
      Chunk line -> modify (line :)
      Flush -> do
        logLines <- get

        unless (null logLines) $ do
          now <- liftIO getCurrentTime

          void . runReq defaultHttpConfig $
            req POST (https "logs.logdna.com" /: "logs" /: "ingest")
              (ReqBodyJson $ object ["lines" .= logLines])
              ignoreResponse
              (options <> "now" =: utcTimeToPOSIXSeconds now)

        put []

  where
    options =
      "hostname" =: hostname
        <> "apikey" `header` unIngestToken ingestToken


getHostname :: MonadIO io => io SourceName
getHostname = SourceName . pack <$> liftIO getHostName
