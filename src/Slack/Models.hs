module Slack.Models where

import qualified Data.ByteString.Lazy as LB (ByteString(..))
import qualified Data.Text as T (Text(..))

data SlackMessage = SlackMessage
  { smTimestamp :: String
  , smText :: T.Text
  } deriving (Eq, Show)

data SlackReaction = SlackReaction
  { srName :: String
  } deriving (Eq, Show)

type SlackAnticipation = LB.ByteString

type SlackRepeatMap = Maybe Int

type SlackIterator = String

type SlackFlag = String
