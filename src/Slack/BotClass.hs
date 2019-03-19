module Slack.BotClass where

data SlackConst = SlackConst
  { sConstToken :: String
  , sConstChannel :: String
  } deriving (Eq, Show)

class (Monad m) =>
      MonadSlackConst m
  where
  getSlackConst :: m SlackConst

class (Monad m) =>
      MonadTimestampState m
  where
  getTimestamp :: m (Maybe String)
  putTimestamp :: Maybe String -> m ()
