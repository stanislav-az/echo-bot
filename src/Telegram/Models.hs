module Telegram.Models where

import qualified Data.Text                     as T

data TelegramMessage = TelegramMessage {
  tmChatId :: Integer,
  tmText :: T.Text
}

data TelegramReaction = TelegramReaction {
  trId :: String,
  trChatId :: Integer,
  trButton :: Int
}
