module Telegram.Models where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (Text(..))

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

type TelegramAnticipation = ()

type TelegramRepeatMap = HM.HashMap Integer Int

type TelegramIterator = Integer

type TelegramFlag = ()
