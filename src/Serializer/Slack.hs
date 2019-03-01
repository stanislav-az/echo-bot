module Serializer.Slack where

import qualified Data.Text                     as T

data SResponse = SResponse {
  sResponseIsOk :: Bool,
  sResponseMsgs :: Maybe [SMessage]
} deriving (Eq, Show)

data SPostResponse = SPostResponse {
  sPostResponseIsOk :: Bool,
  sPostResponseMsg :: Maybe SMessage
}

data SMessage = SMessage {
  sMessageUser :: Maybe String,
  sMessageText :: T.Text,
  sMessageTimestamp :: String,
  sMessageReactions :: Maybe [SReaction]
} deriving (Eq, Show)

data SReaction = SReaction {
  sReactionName :: String
} deriving (Eq, Show)
