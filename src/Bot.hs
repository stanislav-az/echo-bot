{-# LANGUAGE DeriveGeneric #-}
module Bot where

import GHC.Generics
import Data.Aeson
import Data.Text

data JResponse = JResponse {
    ok :: Bool,
    result :: [Update] 
} deriving (Generic, Show)

data Update = Update {
    update_id :: Integer,
    message :: Maybe Message 
} deriving (Generic, Show)

data Message = Message {
    message_id :: Integer,
    chat :: Chat,
    text :: Maybe Text
} deriving (Generic, Show)

data Chat = Chat {
    id :: Integer
} deriving (Generic, Show)

instance ToJSON JResponse
instance FromJSON JResponse

instance ToJSON Update
instance FromJSON Update

instance ToJSON Message
instance FromJSON Message

instance ToJSON Chat
instance FromJSON Chat

emptyJResponse :: JResponse
emptyJResponse = JResponse {ok = True, result = []}