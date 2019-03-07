module Main where

import qualified SlackBotSpec as SS
import qualified TelegramBotSpec as TS
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  ss <- testSpec "Slack Tests" SS.spec
  ts <- testSpec "Telegram Tests" TS.spec
  defaultMain (testGroup "main tests" [ss, ts])
