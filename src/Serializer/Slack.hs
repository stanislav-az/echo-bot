{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.Slack where

import qualified Data.Text                     as T
import           Data.Aeson
import           Slack.Models

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

data SPostMessage = SPostMessage {
  sPostMessageChannel :: String,
  sPostMessageText :: T.Text
}

instance ToJSON SPostMessage where
  toJSON SPostMessage {..} =
    object ["channel" .= sPostMessageChannel, "text" .= sPostMessageText]

data SReaction = SReaction {
  sReactionName :: String
} deriving (Eq, Show)

instance FromJSON SResponse where
  parseJSON =
    withObject "SResponse" $ \v -> SResponse <$> v .: "ok" <*> v .:? "messages"

emptySResponse :: SResponse
emptySResponse = SResponse { sResponseIsOk = True, sResponseMsgs = Just [] }

instance FromJSON SPostResponse where
  parseJSON = withObject "SPostResponse"
    $ \v -> SPostResponse <$> v .: "ok" <*> v .:? "message"

emptySPostResponse :: SPostResponse
emptySPostResponse = SPostResponse False Nothing

instance FromJSON SMessage where
  parseJSON = withObject "SMessage" $ \v ->
    SMessage
      <$> v
      .:? "user"
      <*> v
      .:  "text"
      <*> v
      .:  "ts"
      <*> v
      .:? "reactions"

instance FromJSON SReaction where
  parseJSON = withObject "SReaction" $ \v -> SReaction <$> v .: "name"

sResponseToMsgs :: SResponse -> [SlackMessage]
sResponseToMsgs sResponse = maybe [] (foldl f []) (sResponseMsgs sResponse)
 where
  f ms SMessage{..} = case sMessageUser of
    Nothing -> ms
    _       -> SlackMessage sMessageTimestamp sMessageText : ms

sPostResponseToReactions :: SPostResponse -> [SlackReaction]
sPostResponseToReactions sPostResponse = SlackReaction
  <$> parseReactionNames reactionNames
 where
  reactionNames =
    fmap sReactionName
      <$> (sPostResponseMsg sPostResponse >>= sMessageReactions)
  parseReactionNames (Just [w]) = case w of
    "one"   -> [1]
    "two"   -> [2]
    "three" -> [3]
    "four"  -> [4]
    "five"  -> [5]
    _       -> []
  parseReactionNames _ = []

sMessageToPostMessage :: SlackMessage -> String -> SPostMessage
sMessageToPostMessage SlackMessage {..} channel =
  SPostMessage { sPostMessageChannel = channel, sPostMessageText = smText }
