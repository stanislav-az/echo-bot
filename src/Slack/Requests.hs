{-# LANGUAGE OverloadedStrings #-}

module Slack.Requests where

import qualified Network.HTTP.Simple           as HTTP
import           Slack.Models
import           Data.Aeson
import           Serializer.Slack
import           Helpers
import           Bot.Request
import           Data.String

makeConHistory :: Maybe String -> String -> String -> Request
makeConHistory timestamp token channel = case timestamp of
  Nothing -> reqWithEmptyBody
    $ HTTP.setRequestQueryString (("limit", Just "1") : address) req
  (Just ts) -> reqWithEmptyBody
    $ HTTP.setRequestQueryString (("oldest", toQueryItem ts) : address) req
 where
  req     = HTTP.parseRequest_ "GET https://slack.com/api/conversations.history"
  address = [("token", toQueryItem token), ("channel", toQueryItem channel)]

makeGetReactions :: String -> String -> String -> Request
makeGetReactions token channel repeatTs =
  reqWithEmptyBody $ HTTP.setRequestQueryString query req
 where
  req = HTTP.parseRequest_ "GET https://slack.com/api/reactions.get"
  query =
    [ ("token"    , toQueryItem token)
    , ("channel"  , toQueryItem channel)
    , ("timestamp", toQueryItem repeatTs)
    ]

makePostMessage :: String -> String -> SlackMessage -> Request
makePostMessage token channel msg = Request reqWithBody postMessage
 where
  req = HTTP.parseRequest_ "POST https://slack.com/api/chat.postMessage"
  reqWithHeaders = HTTP.setRequestHeaders
    [ ("Content-Type" , "application/json; charset=utf-8")
    , ("Authorization", "Bearer " <> fromString token)
    ]
    req
  postMessage = encode $ sMessageToPostMessage msg channel
  reqWithBody = HTTP.setRequestBodyLBS postMessage reqWithHeaders
