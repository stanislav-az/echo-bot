{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Serializer.Telegram where

import qualified Data.Text                     as T
import           Data.Aeson
import           Telegram.Models
import           Helpers
import           GHC.Generics

data TResponse = TResponse {
  tResponseIsOk :: Bool,
  tResponseResult :: [TUpdate]
} deriving (Eq, Show, Generic)

data TUpdate = TUpdate {
  tUpdateId :: Integer,
  tUpdateMessage :: Maybe TMessage,
  tUpdateCallbackQuery :: Maybe TCallbackQuery
} deriving (Eq, Show, Generic)

data TMessage = TMessage {
  tMessageId :: Integer,
  tMessageChat :: TChat,
  tMessageText :: Maybe T.Text
} deriving (Eq, Show, Generic)

data TChat = TChat {
  tChatId :: Integer
} deriving (Eq, Show, Generic)

data TPostRepeatMessage = TPostRepeatMessage {
  tRepeatMsgChatId :: Integer,
  tRepeatMsgText :: T.Text,
  tRepeatMsgReplyMarkup :: TKeyboard
} deriving (Eq, Show, Generic)

data TKeyboard = TKeyboard {
  tKeyboard :: [[TButton]]
} deriving (Eq, Show, Generic)

data TButton = TButton {
  tButtonText :: T.Text,
  tButtonCallbackData :: String
} deriving (Eq, Show, Generic)

data TCallbackQuery = TCallbackQuery {
  tCallbackQueryId :: String,
  tCallbackQueryMessage :: Maybe TMessage,
  tCallbackQueryData :: Maybe String
} deriving (Eq, Show, Generic)

data TCallbackAnswer = TCallbackAnswer {
  tCallbackAnswerQueryId :: String,
  tCallbackAnswerText :: T.Text
} deriving (Eq, Show, Generic)

newtype TPostMessage = TPostMessage TelegramMessage

instance FromJSON TResponse where
  parseJSON =
    withObject "TResponse" $ \v -> TResponse <$> v .: "ok" <*> v .: "result"

instance FromJSON TUpdate where
  parseJSON = withObject "TUpdate" $ \v ->
    TUpdate <$> v .: "update_id" <*> v .:? "message" <*> v .:? "callback_query"

instance FromJSON TMessage where
  parseJSON = withObject "TMessage"
    $ \v -> TMessage <$> v .: "message_id" <*> v .: "chat" <*> v .:? "text"

instance FromJSON TChat where
  parseJSON = withObject "TChat" $ \v -> TChat <$> v .: "id"

instance FromJSON TCallbackQuery where
  parseJSON = withObject "TCallbackQuery"
    $ \v -> TCallbackQuery <$> v .: "id" <*> v .:? "message" <*> v .:? "data"

instance ToJSON TPostMessage where
  toJSON (TPostMessage TelegramMessage {..}) =
    object ["chat_id" .= tmChatId, "text" .= tmText]

instance ToJSON TPostRepeatMessage where
  toJSON TPostRepeatMessage {..} = object
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
  toJSON TCallbackAnswer {..} = object
    [ "callback_query_id" .= tCallbackAnswerQueryId
    , "text" .= tCallbackAnswerText
    ]

emptyTResponse :: TResponse
emptyTResponse = TResponse True []

tResponseToModels :: TResponse -> ([TelegramMessage], [TelegramReaction])
tResponseToModels tResponse = foldr go ([], []) (tResponseResult tResponse) where
  go (TUpdate uid a b) (msgs, cbs) = (getMsg uid a ++ msgs, getCb uid b ++ cbs)
  getMsg uid (Just (TMessage _ (TChat chatID) (Just txt))) =
    [TelegramMessage uid chatID txt]
  getMsg _ _ = []
  getCb uid (Just (TCallbackQuery queryID (Just msg) (Just btnPressed))) =
    [TelegramReaction uid queryID (tChatId $ tMessageChat msg) btnPressed]
  getCb _ _ = []

tMessageToPostMessage :: TelegramMessage -> TPostMessage
tMessageToPostMessage = TPostMessage

tMessageToPostRepeatMessage :: TelegramMessage -> TPostRepeatMessage
tMessageToPostRepeatMessage TelegramMessage {..} = TPostRepeatMessage
  { tRepeatMsgChatId      = tmChatId
  , tRepeatMsgText        = tmText
  , tRepeatMsgReplyMarkup = tStandardKeyboard
  }

tStandardKeyboard :: TKeyboard
tStandardKeyboard = TKeyboard { tKeyboard = [button <$> [1 .. 5]] } where
  button i = TButton { tButtonText = texify i, tButtonCallbackData = show i }

tReactionToCallbackAnswer :: TelegramReaction -> TCallbackAnswer
tReactionToCallbackAnswer TelegramReaction {..} = TCallbackAnswer trId msg
 where
  msg =
    "You've choosen to repeat messages " <> T.pack trCallbackData <> " times"
