module Main where

import           Telegram.WebIO
import           Logging

main :: IO ()
main = do
    logStart
    runTelegramBot

{-To DO
-- keyboard forming on every makeCallbackQuery call
-- unit-tests, readme
-}

