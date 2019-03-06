module Main where

import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Hspec               ( testSpec )
import qualified SlackBotSpec                  as SS

main :: IO ()
main = do
  ss <- testSpec "Slack Tests" SS.spec
  defaultMain (testGroup "main tests" [ss])
