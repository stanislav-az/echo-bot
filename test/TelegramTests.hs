{-# LANGUAGE OverloadedStrings #-}
module TelegramTests where

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
import qualified Data.ByteString               as B
import           Serializer.Telegram

-- runTelegramTests :: IO ()
-- runTelegramTests = hspec $ do

instance Arbitrary TResponse where
    arbitrary = genericArbitraryU

instance Arbitrary TUpdate where
    arbitrary = genericArbitraryU

instance Arbitrary TMessage where
    arbitrary = genericArbitraryU

instance Arbitrary TChat where
    arbitrary = genericArbitraryU

instance Arbitrary TCallbackQuery where
    arbitrary = genericArbitraryU

telegramToken :: String
telegramToken = "00:AA"
