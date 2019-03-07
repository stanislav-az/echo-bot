module Telegram.Start where

import Bot.BotMonad
import Bot.EchoBot
import Bot.Exception
import Config
import Control.Monad.Catch
import Telegram.EchoBot

startTelegramBot :: IO ()
startTelegramBot = do
  env <- makeTelegramEnv
  catches (runBot env $ goEchoBot telegramBot) exceptionHandlers
