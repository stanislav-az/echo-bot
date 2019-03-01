module Serializer.Telegram where

import qualified Data.Text                     as T

data TResponse = TResponse {
  tResponseIsOk :: Bool,
  tResponseResult :: [TUpdate]
} deriving (Eq, Show)

data TUpdate = TUpdate {
  tUpdateId :: Integer,
  tUpdateMessage :: Maybe TMessage,
  tUpdateCallbackQuery :: Maybe TCallbackQuery
} deriving (Eq, Show)

data TMessage = TMessage {
  tMessageId :: Integer,
  tMessageChat :: TChat,
  tMessageText :: Maybe T.Text
} deriving (Eq, Show)

data TChat = TChat {
  tChatId :: Integer
} deriving (Eq, Show)

data TRepeatMsg = TRepeatMsg {
  tRepeatMsgChatId :: Integer,
  tRepeatMsgText :: T.Text,
  tRepeatMsgReplyMarkup :: TKeyboard
} deriving (Eq, Show)

data TKeyboard = TKeyboard {
  tKeyboard :: [[TButton]]
} deriving (Eq, Show)

data TButton = TButton {
  tButtonText :: T.Text,
  tButtonCallbackData :: String
} deriving (Eq, Show)

data TCallbackQuery = TCallbackQuery {
  tCallbackQueryId :: String,
  tCallbackQueryMessage :: Maybe TMessage,
  tCallbackQueryCallbackData :: Maybe String
} deriving (Eq, Show)
