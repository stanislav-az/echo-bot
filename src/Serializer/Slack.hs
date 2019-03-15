{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.Slack where

import Data.Aeson
import Data.Maybe (maybeToList)
import qualified Data.Text as T (Text(..))
import Slack.Models

data SResponse = SResponse
  { sResponseIsOk :: Bool
  , sResponseMsgs :: Maybe [SMessage]
  } deriving (Eq, Show)

data SPostResponse = SPostResponse
  { sPostResponseIsOk :: Bool
  , sPostResponseMsg :: Maybe SMessage
  } deriving (Eq, Show)

data SMessage = SMessage
  { sMessageUser :: Maybe String
  , sMessageText :: T.Text
  , sMessageTimestamp :: String
  , sMessageReactions :: Maybe [SReaction]
  } deriving (Eq, Show)

data SPostMessage = SPostMessage
  { sPostMessageChannel :: String
  , sPostMessageText :: T.Text
  } deriving (Eq, Show)

data SReaction = SReaction
  { sReactionName :: String
  } deriving (Eq, Show)

instance FromJSON SResponse where
  parseJSON =
    withObject "SResponse" $ \v -> SResponse <$> v .: "ok" <*> v .:? "messages"

instance ToJSON SResponse where
  toJSON SResponse {..} =
    object ["ok" .= sResponseIsOk, "messages" .= sResponseMsgs]

instance ToJSON SPostMessage where
  toJSON SPostMessage {..} =
    object ["channel" .= sPostMessageChannel, "text" .= sPostMessageText]

instance FromJSON SPostResponse where
  parseJSON =
    withObject "SPostResponse" $ \v ->
      SPostResponse <$> v .: "ok" <*> v .:? "message"

instance ToJSON SPostResponse where
  toJSON SPostResponse {..} =
    object ["ok" .= sPostResponseIsOk, "message" .= sPostResponseMsg]

instance FromJSON SMessage where
  parseJSON =
    withObject "SMessage" $ \v ->
      SMessage <$> v .:? "user" <*> v .: "text" <*> v .: "ts" <*>
      v .:? "reactions"

instance ToJSON SMessage where
  toJSON SMessage {..} =
    object
      [ "user" .= sMessageUser
      , "text" .= sMessageText
      , "ts" .= sMessageTimestamp
      , "reactions" .= sMessageReactions
      ]

instance FromJSON SReaction where
  parseJSON = withObject "SReaction" $ \v -> SReaction <$> v .: "name"

instance ToJSON SReaction where
  toJSON SReaction {..} = object ["name" .= sReactionName]

sResponseToMsgs :: SResponse -> [SlackMessage]
sResponseToMsgs sResponse = maybe [] (foldl f []) (sResponseMsgs sResponse)
  where
    f ms msg = maybeToList (sMessageToMsg msg) ++ ms

sPostResponseToMsg :: SPostResponse -> Maybe SlackMessage
sPostResponseToMsg SPostResponse {..} = composeMsg <$> sPostResponseMsg
  where
    composeMsg SMessage {..} = Message sMessageTimestamp sMessageText

sMessageToMsg :: SMessage -> Maybe SlackMessage
sMessageToMsg SMessage {..} =
  let msg = Message sMessageTimestamp sMessageText
   in maybe Nothing (const (Just msg)) sMessageUser

sPostResponseToReactions :: SPostResponse -> [SlackMessage]
sPostResponseToReactions sPostResponse = maybe [] (fmap Reaction) reactionNames
  where
    reactionNames =
      fmap sReactionName <$>
      (sPostResponseMsg sPostResponse >>= sMessageReactions)

constructSPostMessage :: String -> T.Text -> SPostMessage
constructSPostMessage = SPostMessage
