{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Conduit.LogDNA (
  logLine,
  logDNA,
  IngestToken(..),
  AppName(..),
  Env(..),
  getHostname
) where

import           Control.Monad            (unless, void)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Aeson               (ToJSON (..), object, (.=))
import           Data.ByteString          (ByteString)
import           Data.Conduit
import           Data.Conduit.Combinators (mapM)
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
newtype Env         = Env { unEnv :: Text } deriving (ToJSON, Show)
newtype Hostname    = Hostname { unHostname :: Text } deriving (Show, ToHttpApiData)
data LogLine        = LogLine UTCTime AppName Env ByteString deriving Show

instance ToJSON LogLine where
  toJSON (LogLine time app env bytes) =
    object [
        "line"      .= decodeUtf8 bytes,
        "timestamp" .= utcTimeToPOSIXSeconds time,
        "app"       .= app,
        "env"       .= env
      ]


logLine :: MonadIO io => AppName -> Env -> ConduitT ByteString LogLine io ()
logLine appName env =
  mapM $ \l -> liftIO (LogLine <$> getCurrentTime <*> pure appName <*> pure env <*> pure l)


logDNA :: MonadIO io => IngestToken -> Hostname -> ConduitT [LogLine] o io ()
logDNA ingestToken hostname =
  awaitForever $ \logLines ->
    unless (null logLines) $ do
      now <- liftIO getCurrentTime

      void . runReq defaultHttpConfig $
        req POST (https "logs.logdna.com" /: "logs" /: "ingest")
          (ReqBodyJson $ object ["lines" .= logLines])
          ignoreResponse
          (options <> "now" =: utcTimeToPOSIXSeconds now)

  where
    options =
      "hostname" =: hostname
        <> "apikey" `header` unIngestToken ingestToken


getHostname :: MonadIO io => io Hostname
getHostname = Hostname . pack <$> liftIO getHostName
