{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot.BotClass (logInfo)
import Config (Bot(..), getByName, getLogConfig, loadConfig)
import qualified Control.Logger.Simple as L (withGlobalLogging)
import Slack.Start (startSlackBot)
import Telegram.Start (startTelegramBot)

main :: IO ()
main = do
  conf <- loadConfig
  lc <- getLogConfig conf
  bot <- getByName conf "bot_to_run"
  L.withGlobalLogging lc $ run bot
  where
    run Telegram = logInfo "Starting Telegram bot" >> startTelegramBot
    run Slack = logInfo "Starting Slack bot" >> startSlackBot
