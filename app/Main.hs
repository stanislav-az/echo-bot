module Main where

import Telegram.WebIO
import Slack.WebIO
import Logging

main :: IO ()
main = do
    logStart
    runTelegramBot

{-To DO
-- unit-tests, readme
-}

