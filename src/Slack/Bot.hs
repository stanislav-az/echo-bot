{-# LANGUAGE DeriveGeneric #-}
module Slack.Bot where

import GHC.Generics
import Data.Aeson
import Data.Text

data SJResponse = SJResponse {
    ok :: Bool,
    messages :: [SMessage]
} deriving (Generic, Show)

data SMessage = SMessage {
    user :: Maybe String,
    text :: Text,
    ts :: String
} deriving (Generic, Show)

instance ToJSON SJResponse
instance FromJSON SJResponse

instance ToJSON SMessage
instance FromJSON SMessage

emptySJResponse :: SJResponse
emptySJResponse = SJResponse {ok = True, messages = []}
