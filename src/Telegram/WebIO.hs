module Telegram.WebIO where

import           Config
import           Telegram.WebIOInternal
import           Bot.BotMonad
import           Bot.BotClass
import           Helpers
import           Control.Monad.Except
import           Errors

startTelegramBot :: IO ()
startTelegramBot = do
  env <- makeTelegramEnv
  res <- runTelegramBot env $ catchError goTelegramBot botErrorHandler
  either (logError . texify) pure res

