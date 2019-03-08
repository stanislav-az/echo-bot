{-# LANGUAGE OverloadedStrings #-}

module Slack.Requests where

import Data.String (fromString)
import Helpers (toQueryItem)
import qualified Network.HTTP.Simple as HTTP
  ( Request(..)
  , addRequestHeader
  , parseRequest_
  , setRequestBodyJSON
  , setRequestQueryString
  )
import Serializer.Slack (sMessageToPostMessage)
import Slack.Models (SlackMessage(..))

makeConHistory :: Maybe String -> String -> String -> HTTP.Request
makeConHistory timestamp token channel =
  case timestamp of
    Nothing -> HTTP.setRequestQueryString (("limit", Just "1") : address) req
    (Just ts) ->
      HTTP.setRequestQueryString (("oldest", toQueryItem ts) : address) req
  where
    req = HTTP.parseRequest_ "GET https://slack.com/api/conversations.history"
    address = [("token", toQueryItem token), ("channel", toQueryItem channel)]

makeGetReactions :: String -> String -> String -> HTTP.Request
makeGetReactions token channel repeatTs = HTTP.setRequestQueryString query req
  where
    req = HTTP.parseRequest_ "GET https://slack.com/api/reactions.get"
    query =
      [ ("token", toQueryItem token)
      , ("channel", toQueryItem channel)
      , ("timestamp", toQueryItem repeatTs)
      ]

makePostMessage :: String -> String -> SlackMessage -> HTTP.Request
makePostMessage token channel msg =
  HTTP.setRequestBodyJSON postMessage reqWithHeaders
  where
    req = HTTP.parseRequest_ "POST https://slack.com/api/chat.postMessage"
    reqWithHeaders =
      HTTP.addRequestHeader "Authorization" ("Bearer " <> fromString token) req
    postMessage = sMessageToPostMessage msg channel
