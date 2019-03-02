{-# LANGUAGE OverloadedStrings #-}

module Slack.Requests where

import           Network.HTTP.Simple
import           Slack.Models
import qualified Data.ByteString               as B
import           Data.CaseInsensitive
import           Data.String
import           Data.Aeson
import           Serializer.Slack

-- find something better than parseRequest_

makeConHistory :: Maybe String -> String -> String -> Request
makeConHistory timestamp token channel = case timestamp of
  Nothing   -> parseRequest_ $ reqString ++ "&limit=1"
  (Just ts) -> parseRequest_ $ reqString ++ "&oldest=" ++ ts
 where
  reqString =
    "GET "
      ++ "https://slack.com/api/conversations.history?token="
      ++ token
      ++ "&channel="
      ++ channel

makeGetReactions :: String -> String -> String -> Request
makeGetReactions token channel repeatTs = parseRequest_ reqString
 where
  reqString =
    "GET "
      ++ "https://slack.com/api/reactions.get?token="
      ++ token
      ++ "&channel="
      ++ channel
      ++ "&timestamp="
      ++ repeatTs

makePostMessage :: String -> String -> SlackMessage -> Request
makePostMessage token channel msg = setRequestBodyLBS body reqWithHeaders
 where
  req = parseRequest_ $ "POST " ++ "https://slack.com/api/chat.postMessage"
  reqWithHeaders = setRequestHeaders
    [ ("Content-Type" :: CI B.ByteString , "application/json; charset=utf-8")
    , ("Authorization" :: CI B.ByteString, "Bearer " <> fromString token)
    ]
    req
  body = encode $ sMessageToPostMessage msg channel
