{-# LANGUAGE RecordWildCards #-}

module Telegram.Models where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (Text(..))

data TelegramMessage
  = Message { tmUpdateId :: Integer
            , tmChatId :: Integer
            , tmHasKeyboard :: Bool
            , tmText :: T.Text }
  | Callback { tcUpdateId :: Integer
             , tcId :: String
             , tcChatId :: Integer
             , tcData :: String }
  deriving (Eq, Show)

type TelegramResponse = ()

type TelegramRepeatMap = HM.HashMap Integer Int

getUpdateId :: TelegramMessage -> Integer
getUpdateId Message {..} = tmUpdateId
getUpdateId Callback {..} = tcUpdateId
