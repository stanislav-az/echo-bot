module Slack.WebIO where

import           Control.Monad.Except
import           Errors
import           Config
import           Slack.WebIOInternal
import           Bot.BotMonadD
import           Bot.BotClass
import           Helpers

-- startSlackBot :: IO ()
-- startSlackBot = do
--   env <- makeSlackEnv
--   res <- runSlackBot env $ catchError goSlackBot botErrorHandler
--   either (logError .  texify) pure res

