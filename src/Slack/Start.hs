module Slack.Start where

import           Config
import           Slack.EchoBot
import           Bot.BotMonad
import           Bot.EchoBot
import           Bot.Exception
import           Control.Monad.Catch

startSlackBot :: IO ()
startSlackBot = do
  env <- makeSlackEnv
  catches (runBot env $ goEchoBot slackBot) exceptionHandlers

