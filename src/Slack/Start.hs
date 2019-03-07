module Slack.Start where

import Bot.BotMonad
import Bot.EchoBot
import Bot.Exception
import Config
import Control.Monad.Catch
import Slack.EchoBot

startSlackBot :: IO ()
startSlackBot = do
  env <- makeSlackEnv
  catches (runBot env $ goEchoBot slackBot) exceptionHandlers
