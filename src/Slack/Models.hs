module Slack.Models where

import qualified Data.ByteString.Lazy as LB (ByteString(..))
import qualified Data.Text as T (Text(..))

data SlackMessage = SlackMessage
  { smTimestamp :: String
  , smText :: T.Text
  } deriving (Eq, Show)

data SlackAnticipation = SlackAnticipation
  { saPostMsgResponseBody :: LB.ByteString
  } deriving (Eq, Show)

data SlackFlag = SlackFlag
  { sfRepeatTimestamp :: String
  } deriving (Eq, Show)

data SlackReaction = SlackReaction
  { srName :: String
  } deriving (Eq, Show)

data SlackRepeat = SlackRepeat
  { sRepeatNumber :: Int
  } deriving (Eq, Show)

data SlackRepeatMap = SlackRepeatMap
  { thisChatRepeatNumber :: Maybe Int
  } deriving (Eq, Show)

emptySlackRepeatMap :: SlackRepeatMap
emptySlackRepeatMap = SlackRepeatMap Nothing

data SlackIterator = SlackIterator
  { lastTimestamp :: String
  } deriving (Eq, Show)
