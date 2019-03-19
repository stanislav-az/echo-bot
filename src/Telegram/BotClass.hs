module Telegram.BotClass where

data TelegramConst = TelegramConst
  { tConstToken :: String
  } deriving (Eq, Show)

class (Monad m) =>
      MonadTelegramConst m
  where
  getTelegramConst :: m TelegramConst
