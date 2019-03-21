module Slack.BotClass where

data SlackStaticOptions = SlackStaticOptions
  { sConstToken :: String
  , sConstChannel :: String
  } deriving (Eq, Show)

class (Monad m) =>
      MonadSlackStaticOptions m
  where
  getSlackStaticOptions :: m SlackStaticOptions

class (Monad m) =>
      MonadTimestampState m
  where
  getTimestamp :: m (Maybe String)
  putTimestamp :: Maybe String -> m ()
