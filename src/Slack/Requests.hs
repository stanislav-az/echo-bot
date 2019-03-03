{-# LANGUAGE OverloadedStrings #-}

module Slack.Requests where

import           Network.HTTP.Simple
import           Slack.Models
import qualified Data.ByteString               as B
import           Data.CaseInsensitive
import           Data.String
import           Data.Aeson
import           Serializer.Slack
import           Helpers

makeConHistory :: Maybe String -> String -> String -> Request
makeConHistory timestamp token channel = case timestamp of
  Nothing   -> setRequestQueryString (("limit", Just "1") : address) req
  (Just ts) -> setRequestQueryString (("oldest", toQueryItem ts) : address) req
 where
  req     = parseRequest_ "GET https://slack.com/api/conversations.history"
  address = [("token", toQueryItem token), ("channel", toQueryItem channel)]

makeGetReactions :: String -> String -> String -> Request
makeGetReactions token channel repeatTs = setRequestQueryString query req
 where
  req = parseRequest_ "GET https://slack.com/api/reactions.get"
  query =
    [ ("token"    , toQueryItem token)
    , ("channel"  , toQueryItem channel)
    , ("timestamp", toQueryItem repeatTs)
    ]

makePostMessage :: String -> String -> SlackMessage -> Request
makePostMessage token channel msg = setRequestBodyJSON postMessage
                                                       reqWithHeaders
 where
  req = parseRequest_ "POST https://slack.com/api/chat.postMessage"
  reqWithHeaders =
    addRequestHeader "Authorization" ("Bearer " <> fromString token) req
  postMessage = sMessageToPostMessage msg channel
