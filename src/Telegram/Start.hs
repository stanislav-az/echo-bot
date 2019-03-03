module Telegram.Start where

import           Config
import           Telegram.EchoBot
import           Bot.BotMonad
import           Bot.EchoBot
import           Bot.Exception
import           Control.Monad.Catch

startTelegramBot :: IO ()
startTelegramBot = do
  env <- makeTelegramEnv
  catches (runBot env $ goEchoBot telegramBot) exceptionHandlers
