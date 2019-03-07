{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.BotClass
import Config
import qualified Control.Logger.Simple as L (withGlobalLogging)
import Slack.Start
import Telegram.Start

main :: IO ()
main = do
  conf <- loadConfig
  lc <- getLogConfig conf
  bot <- getByName conf "bot_to_run"
  L.withGlobalLogging lc $ run bot
  where
    run Telegram = logInfo "Starting Telegram bot" >> startTelegramBot
    run Slack = logInfo "Starting Slack bot" >> startSlackBot
