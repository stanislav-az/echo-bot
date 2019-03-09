{-# LANGUAGE OverloadedStrings #-}

module Slack.Requests where

import qualified Data.ByteString.Char8 as B8 (pack)
import Data.String (fromString)
import qualified Network.HTTP.Simple as HTTP
  ( Request(..)
  , addRequestHeader
  , parseRequest_
  , setRequestBodyJSON
  , setRequestQueryString
  )
import qualified Network.HTTP.Types.URI as Q (simpleQueryToQuery)
import Serializer.Slack (sMessageToPostMessage)
import Slack.Models (SlackMessage(..))

makeConHistory :: Maybe String -> String -> String -> HTTP.Request
makeConHistory timestamp token channel =
  case timestamp of
    Nothing ->
      HTTP.setRequestQueryString
        (Q.simpleQueryToQuery $ ("limit", "1") : address)
        req
    (Just ts) ->
      HTTP.setRequestQueryString
        (Q.simpleQueryToQuery $ ("oldest", B8.pack ts) : address)
        req
  where
    req = HTTP.parseRequest_ "GET https://slack.com/api/conversations.history"
    address = [("token", B8.pack token), ("channel", B8.pack channel)]

makeGetReactions :: String -> String -> String -> HTTP.Request
makeGetReactions token channel repeatTs = HTTP.setRequestQueryString query req
  where
    req = HTTP.parseRequest_ "GET https://slack.com/api/reactions.get"
    query =
      Q.simpleQueryToQuery
        [ ("token", B8.pack token)
        , ("channel", B8.pack channel)
        , ("timestamp", B8.pack repeatTs)
        ]

makePostMessage :: String -> String -> SlackMessage -> HTTP.Request
makePostMessage token channel msg =
  HTTP.setRequestBodyJSON postMessage reqWithHeaders
  where
    req = HTTP.parseRequest_ "POST https://slack.com/api/chat.postMessage"
    reqWithHeaders =
      HTTP.addRequestHeader "Authorization" ("Bearer " <> fromString token) req
    postMessage = sMessageToPostMessage msg channel
