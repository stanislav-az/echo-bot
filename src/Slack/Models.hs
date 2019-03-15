module Slack.Models where

import qualified Data.ByteString.Lazy as LB (ByteString(..))
import qualified Data.Text as T (Text(..))

data SlackMessage
  = Message { smTimestamp :: String
            , smText :: T.Text }
  | Reaction { srName :: String }
  deriving (Eq, Show)

type SlackAnticipation = LB.ByteString

type SlackRepeatMap = Maybe Int

type SlackIterator = String

type SlackFlag = String
