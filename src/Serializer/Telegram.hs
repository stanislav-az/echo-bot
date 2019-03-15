{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.Telegram where

import Data.Aeson
import qualified Data.Text as T (Text(..), pack)
import Ext.Data.Text (textify)
import Telegram.Models

data TResponse = TResponse
  { tResponseIsOk :: Bool
  , tResponseResult :: [TUpdate]
  } deriving (Eq, Show)

data TUpdate = TUpdate
  { tUpdateId :: Integer
  , tUpdateMessage :: Maybe TMessage
  , tUpdateCallbackQuery :: Maybe TCallbackQuery
  } deriving (Eq, Show)

data TMessage = TMessage
  { tMessageId :: Integer
  , tMessageChat :: TChat
  , tMessageText :: Maybe T.Text
  } deriving (Eq, Show)

data TChat = TChat
  { tChatId :: Integer
  } deriving (Eq, Show)

data TPostRepeatMessage = TPostRepeatMessage
  { tRepeatMsgChatId :: Integer
  , tRepeatMsgText :: T.Text
  , tRepeatMsgReplyMarkup :: TKeyboard
  } deriving (Eq, Show)

data TKeyboard = TKeyboard
  { tKeyboard :: [[TButton]]
  } deriving (Eq, Show)

data TButton = TButton
  { tButtonText :: T.Text
  , tButtonCallbackData :: String
  } deriving (Eq, Show)

data TCallbackQuery = TCallbackQuery
  { tCallbackQueryId :: String
  , tCallbackQueryMessage :: Maybe TMessage
  , tCallbackQueryData :: Maybe String
  } deriving (Eq, Show)

data TCallbackAnswer = TCallbackAnswer
  { tCallbackAnswerQueryId :: String
  , tCallbackAnswerText :: T.Text
  } deriving (Eq, Show)

data TPostMessage = TPostMessage
  { tPostMessageChatId :: Integer
  , tPostMessageText :: T.Text
  }

instance FromJSON TResponse where
  parseJSON =
    withObject "TResponse" $ \v -> TResponse <$> v .: "ok" <*> v .: "result"

instance ToJSON TResponse where
  toJSON TResponse {..} =
    object ["ok" .= tResponseIsOk, "result" .= tResponseResult]

instance FromJSON TUpdate where
  parseJSON =
    withObject "TUpdate" $ \v ->
      TUpdate <$> v .: "update_id" <*> v .:? "message" <*>
      v .:? "callback_query"

instance ToJSON TUpdate where
  toJSON TUpdate {..} =
    object
      [ "update_id" .= tUpdateId
      , "message" .= tUpdateMessage
      , "callback_query" .= tUpdateCallbackQuery
      ]

instance FromJSON TMessage where
  parseJSON =
    withObject "TMessage" $ \v ->
      TMessage <$> v .: "message_id" <*> v .: "chat" <*> v .:? "text"

instance ToJSON TMessage where
  toJSON TMessage {..} =
    object
      [ "message_id" .= tMessageId
      , "chat" .= tMessageChat
      , "text" .= tMessageText
      ]

instance FromJSON TChat where
  parseJSON = withObject "TChat" $ \v -> TChat <$> v .: "id"

instance ToJSON TChat where
  toJSON TChat {..} = object ["id" .= tChatId]

instance FromJSON TCallbackQuery where
  parseJSON =
    withObject "TCallbackQuery" $ \v ->
      TCallbackQuery <$> v .: "id" <*> v .:? "message" <*> v .:? "data"

instance ToJSON TCallbackQuery where
  toJSON TCallbackQuery {..} =
    object
      [ "id" .= tCallbackQueryId
      , "message" .= tCallbackQueryMessage
      , "data" .= tCallbackQueryData
      ]

instance ToJSON TPostMessage where
  toJSON TPostMessage {..} =
    object ["chat_id" .= tPostMessageChatId, "text" .= tPostMessageText]

instance ToJSON TPostRepeatMessage where
  toJSON TPostRepeatMessage {..} =
    object
      [ "chat_id" .= tRepeatMsgChatId
      , "text" .= tRepeatMsgText
      , "reply_markup" .= tRepeatMsgReplyMarkup
      ]

instance ToJSON TKeyboard where
  toJSON TKeyboard {..} = object ["inline_keyboard" .= tKeyboard]

instance ToJSON TButton where
  toJSON TButton {..} =
    object ["text" .= tButtonText, "callback_data" .= tButtonCallbackData]

instance ToJSON TCallbackAnswer where
  toJSON TCallbackAnswer {..} =
    object
      [ "callback_query_id" .= tCallbackAnswerQueryId
      , "text" .= tCallbackAnswerText
      ]

tResponseToMsgs :: TResponse -> [TelegramMessage]
tResponseToMsgs tResponse = foldr go [] (tResponseResult tResponse)
  where
    go (TUpdate uid a b) ms = getMsg uid a ++ getCb uid b ++ ms
    getMsg uid (Just (TMessage _ (TChat chatID) (Just txt))) =
      [Message uid chatID False txt]
    getMsg _ _ = []
    getCb uid (Just (TCallbackQuery queryID (Just msg) (Just btnPressed))) =
      [Callback uid queryID (tChatId $ tMessageChat msg) btnPressed]
    getCb _ _ = []

constructTPostMessage :: Integer -> T.Text -> TPostMessage
constructTPostMessage = TPostMessage

constructTPostRepeatMessage :: Integer -> T.Text -> TPostRepeatMessage
constructTPostRepeatMessage chatId text =
  TPostRepeatMessage
    { tRepeatMsgChatId = chatId
    , tRepeatMsgText = text
    , tRepeatMsgReplyMarkup = tStandardKeyboard
    }

constructTCallbackAnswer :: String -> T.Text -> TCallbackAnswer
constructTCallbackAnswer queryId text =
  TCallbackAnswer queryId $
  "You've choosen to repeat messages " <> text <> " times"

tStandardKeyboard :: TKeyboard
tStandardKeyboard = TKeyboard {tKeyboard = [button <$> [1 .. 5]]}
  where
    button i = TButton {tButtonText = textify i, tButtonCallbackData = show i}
