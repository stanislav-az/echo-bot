module Telegram.Start where

import Bot.BotMonad (runBot)
import Bot.EchoBot (goEchoBot)
import Bot.Exception (exceptionHandlers)
import Config (makeTelegramEnv)
import Control.Monad.Catch (catches)
import Telegram.EchoBot

startTelegramBot :: IO ()
startTelegramBot = do
  env <- makeTelegramEnv
  catches (runBot env $ goEchoBot telegramBot) exceptionHandlers
