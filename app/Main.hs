{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Telegram.WebIO
import           Slack.Start
import           Config
import           Bot
import           Bot.BotClass
import qualified Control.Logger.Simple         as L
                                                ( withGlobalLogging )
main :: IO ()
main = do
  conf <- loadConfig
  lc   <- getLogConfig conf
  bot  <- getByName conf "bot_to_run"
  L.withGlobalLogging lc $ run bot
 where
  run Telegram = logInfo "Starting Telegram bot" >> startTelegramBot
  run Slack    = logInfo "Starting Slack bot" >> startSlackBot
