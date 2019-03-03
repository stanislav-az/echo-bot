{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.Telegram where

import qualified Data.Text                     as T
import           Data.Aeson
import           Telegram.Models
import           Helpers

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

data TPostRepeatMessage = TPostRepeatMessage {
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
  tCallbackQueryData :: Maybe String
} deriving (Eq, Show)

data TCallbackAnswer = TCallbackAnswer {
  tCallbackAnswerQueryId :: String,
  tCallbackAnswerText :: T.Text
}

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

tResponseToUpdateId :: TResponse -> Maybe Integer -> Maybe Integer
tResponseToUpdateId tResponse = findLastUid (tResponseResult tResponse)
 where
  findLastUid [] offset = offset
  findLastUid us _      = Just $ tUpdateId $ last us

tResponseToModels
  :: TResponse -> Maybe Integer -> ([TelegramMessage], [TelegramReaction])
tResponseToModels tResponse offset = foldr
  go
  ([], [])
  (seeIfAreOld offset $ tResponseResult tResponse) where
  seeIfAreOld Nothing xs = xs
  seeIfAreOld _       [] = []
  seeIfAreOld _       xs = tail xs
  go (TUpdate _ a b) (msgs, cbs) = (getMsg a ++ msgs, getCb b ++ cbs)
  getMsg (Just (TMessage _ (TChat chatID) (Just txt))) =
    [TelegramMessage chatID txt]
  getMsg _ = []
  getCb (Just (TCallbackQuery queryID (Just msg) (Just btnPressed))) =
    [TelegramReaction queryID (tChatId $ tMessageChat msg) btnPressed]
  getCb _ = []

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
