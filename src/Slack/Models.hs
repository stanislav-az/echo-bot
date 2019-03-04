module Slack.Models where

import qualified Data.Text                     as T

data SlackMessage = SlackMessage {
  smTimestamp :: String,
  smText :: T.Text
} deriving (Eq, Show)

data SlackReaction = SlackReaction {
  srRepeatNumber :: Int
} deriving (Eq, Show)

