{-# LANGUAGE DeriveGeneric #-}
module Slack.Bot where

import           GHC.Generics
import           Data.Aeson
import           Data.Text

data SJResponse = SJResponse {
  ok :: Bool,
  messages :: Maybe [SMessage],
  message :: Maybe SMessage
} deriving (Generic, Show)

data SMessage = SMessage {
  user :: Maybe String,
  text :: Text,
  ts :: String,
  reactions :: Maybe [SReaction]
} deriving (Generic, Show)

data SReaction = SReaction {
  name :: String
} deriving (Generic, Show)

instance ToJSON SJResponse
instance FromJSON SJResponse

instance ToJSON SMessage
instance FromJSON SMessage

instance ToJSON SReaction
instance FromJSON SReaction

emptySJResponse :: SJResponse
emptySJResponse =
  SJResponse { ok = True, messages = Just [], message = Nothing }
