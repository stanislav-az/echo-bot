{-# LANGUAGE RecordWildCards #-}

module Slack.Models where

import qualified Data.ByteString.Lazy as LB (ByteString(..))
import qualified Data.Text as T (Text(..))

data SlackMessage
  = Message { smTimestamp :: String
            , smIsRepeatMsg :: Bool
            , smText :: T.Text }
  | Reaction { srName :: String }
  deriving (Eq, Show)

type SlackRepeatMap = Maybe Int

sIsMessage :: SlackMessage -> Bool
sIsMessage Message {..} = True
sIsMessage _ = False

sIsReaction :: SlackMessage -> Bool
sIsReaction Reaction {..} = True
sIsReaction _ = False
