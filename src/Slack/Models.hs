module Slack.Models where

import qualified Data.ByteString.Lazy as LB (ByteString(..))
import qualified Data.Text as T (Text(..))

data SlackMessage = SlackMessage
  { smTimestamp :: String
  , smHasAnticipation :: Bool
  , smText :: T.Text
  } deriving (Eq, Show)

data SlackAnticipation = SlackAnticipation
  { saPostMsgResponseBody :: LB.ByteString
  } deriving (Eq, Show)

data SlackFlag = SlackFlag
  { sfRepeatTimestamp :: String
  } deriving (Eq, Show)

data SlackReaction = SlackReaction
  { srRepeatNumber :: Int
  } deriving (Eq, Show)
