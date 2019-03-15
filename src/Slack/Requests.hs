{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack.Requests where

import qualified Data.ByteString.Char8 as B8 (pack)
import Data.String (fromString)
import qualified Data.Text as T (Text(..))
import qualified Network.HTTP.Simple as HTTP
  ( Request(..)
  , addRequestHeader
  , parseRequest_
  , setRequestBodyJSON
  , setRequestQueryString
  )
import qualified Network.HTTP.Types.URI as Q (simpleQueryToQuery)
import Serializer.Slack (constructSPostMessage)
import Slack.Models (SlackFlag(..), SlackIterator(..), SlackMessage(..))

makeConHistory :: Maybe SlackIterator -> String -> String -> HTTP.Request
makeConHistory timestamp token channel =
  HTTP.setRequestQueryString (Q.simpleQueryToQuery $ arg : address) req
  where
    req = HTTP.parseRequest_ "GET https://slack.com/api/conversations.history"
    address = [("token", B8.pack token), ("channel", B8.pack channel)]
    arg = maybe ("limit", "1") (("oldest", ) . B8.pack) timestamp

makeGetReactions :: String -> String -> SlackFlag -> HTTP.Request
makeGetReactions token channel repeatTs = HTTP.setRequestQueryString query req
  where
    req = HTTP.parseRequest_ "GET https://slack.com/api/reactions.get"
    query =
      Q.simpleQueryToQuery
        [ ("token", B8.pack token)
        , ("channel", B8.pack channel)
        , ("timestamp", B8.pack repeatTs)
        ]

makePostMessage :: String -> String -> T.Text -> HTTP.Request
makePostMessage token channel text =
  HTTP.setRequestBodyJSON postMessage reqWithHeaders
  where
    req = HTTP.parseRequest_ "POST https://slack.com/api/chat.postMessage"
    reqWithHeaders =
      HTTP.addRequestHeader "Authorization" ("Bearer " <> fromString token) req
    postMessage = constructSPostMessage channel text
