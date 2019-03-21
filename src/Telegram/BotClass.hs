module Telegram.BotClass where

data TelegramStaticOptions = TelegramStaticOptions
  { tConstToken :: String
  } deriving (Eq, Show)

class (Monad m) =>
      MonadTelegramStaticOptions m
  where
  getTelegramStaticOptions :: m TelegramStaticOptions
