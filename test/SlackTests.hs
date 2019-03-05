{-# LANGUAGE OverloadedStrings #-}
module SlackTests where

import           Config
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Generic.Random
import           Generic.Random.Tutorial
import           Data.Maybe
import           Control.Monad.State
import           Data.HashMap.Strict     hiding ( null )
import           Network.HTTP.Simple
import           Network.HTTP.Conduit
import           Data.CaseInsensitive
import           Data.String
import qualified Data.ByteString               as B
import           Serializer.Slack
import           Slack.Requests

-- runSlackTests :: IO ()
-- runSlackTests = hspec $ do

instance Arbitrary SResponse where
  arbitrary = genericArbitraryU

instance Arbitrary SMessage where
  arbitrary = genericArbitraryU

instance Arbitrary SReaction where
  arbitrary = genericArbitraryU

slackChannel :: String
slackChannel = "DE0000"

slackToken :: String
slackToken = "xoxb-00-00-EE"
