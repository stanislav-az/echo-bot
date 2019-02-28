{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Telegram.WebIO
import           Slack.WebIO
import           Config
import           Bot
import           Bot.BotClass
import qualified Control.Logger.Simple         as L
                                                ( withGlobalLogging )
main :: IO ()
main = do
  conf    <- loadConfig
  lc <- getLogConfig conf
  bot     <- getByName conf "bot_to_run"
  L.withGlobalLogging lc $ runbot bot
 where
  runbot Telegram = logInfo "Starting Telegram bot" >> startTelegramBot
  runbot Slack    = logInfo "Starting Slack bot" >> startSlackBot
