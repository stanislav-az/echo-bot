module Telegram.Models where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (Text(..))

data TelegramMessage
  = Message { tmUpdateId :: Integer
            , tmChatId :: Integer
            , tmText :: T.Text }
  | Callback { tcUpdateId :: Integer
             , tcId :: String
             , tcChatId :: Integer
             , tcCallbackData :: String }
  deriving (Eq, Show)

type TelegramAnticipation = ()

type TelegramRepeatMap = HM.HashMap Integer Int

type TelegramIterator = Integer

type TelegramFlag = ()
