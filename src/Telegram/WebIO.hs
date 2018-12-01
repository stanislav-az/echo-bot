module Telegram.WebIO where

import Config
import Telegram.WebIOInternal
import Control.Monad.State
import Data.HashMap.Strict

runTelegramBot :: IO ()
runTelegramBot = do
    sr <- tStandardRequest
    hMsg <- helpMsg
    rMsg <- repeatMsg
    r <- defaultRepeat
    dlog <- debugLogging
    evalStateT sendLastMsgs (Nothing, sr, hMsg, rMsg, r, empty, dlog)
