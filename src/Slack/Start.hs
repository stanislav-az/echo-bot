module Slack.Start where

import Bot.BotMonad (runBot)
import Bot.EchoBot (goEchoBot)
import Bot.Exception (exceptionHandlers)
import Config (makeSlackEnv)
import Control.Monad.Catch (catches)
import Slack.EchoBot

startSlackBot :: IO ()
startSlackBot = do
  env <- makeSlackEnv
  catches (runBot env $ goEchoBot slackBot) exceptionHandlers
