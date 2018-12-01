module Slack.WebIO where

import Config
import Slack.WebIOInternal
import Control.Monad.State

runSlackBot :: IO ()
runSlackBot = do
    (token, channel) <- slackConfig
    hMsg <- helpMsg
    rMsg <- repeatMsg
    r <- defaultRepeat
    dlog <- debugLogging
    evalStateT goSlackBot (Nothing, token, channel, hMsg, rMsg, r, dlog, Nothing)
    
        