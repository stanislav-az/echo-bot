module Telegram.Models where

import qualified Data.Text as T

data TelegramMessage = TelegramMessage
  { tmUpdateId :: Integer
  , tmChatId :: Integer
  , tmText :: T.Text
  } deriving (Eq, Show)

data TelegramReaction = TelegramReaction
  { trUpdateId :: Integer
  , trId :: String
  , trChatId :: Integer
  , trCallbackData :: String
  } deriving (Eq, Show)
