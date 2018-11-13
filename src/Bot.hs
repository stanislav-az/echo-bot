{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Bot where

import GHC.Generics
import Data.Aeson
import Data.Text

data JResponse = JResponse {
    ok :: Bool,
    result :: [Update] 
} deriving (Generic, Show)

data Update = Update {
    update_id :: Integer,
    message :: Maybe Message 
} deriving (Generic, Show)

data Message = Message {
    message_id :: Integer,
    chat :: Chat,
    text :: Maybe Text
} deriving (Generic, Show)

data Chat = Chat {
    id :: Integer
} deriving (Generic, Show)

data RepeatMessageBody = RepeatMessageBody {
    chat_id :: Integer,
    text :: Text,
    reply_markup :: Maybe InlineKeyboardMarkup
} deriving (Generic, Show)

data InlineKeyboardMarkup = InlineKeyboardMarkup {
    inline_keyboard :: [[InlineKeyboardButton]]
} deriving (Generic, Show)

data InlineKeyboardButton = InlineKeyboardButton {
    text :: Text,
    callback_data :: CallbackQuery
} deriving (Generic, Show)

data CallbackQuery = CallbackQuery {
    id :: String,
    message :: Maybe Message
} deriving (Generic, Show)

instance ToJSON JResponse
instance FromJSON JResponse

instance ToJSON Update
instance FromJSON Update

instance ToJSON Message
instance FromJSON Message

instance ToJSON Chat
instance FromJSON Chat

instance ToJSON RepeatMessageBody
instance FromJSON RepeatMessageBody

instance ToJSON InlineKeyboardMarkup
instance FromJSON InlineKeyboardMarkup

instance ToJSON InlineKeyboardButton
instance FromJSON InlineKeyboardButton

instance ToJSON CallbackQuery
instance FromJSON CallbackQuery

emptyJResponse :: JResponse
emptyJResponse = JResponse {ok = True, result = []}