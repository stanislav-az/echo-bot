{-# LANGUAGE OverloadedStrings #-}

module Telegram.Requests where

import qualified Data.ByteString.Char8 as B8 (pack)
import Ext.Network.HTTP.Types.URI (showToQueryItem)
import qualified Network.HTTP.Simple as HTTP
  ( Request(..)
  , parseRequest_
  , setRequestBodyJSON
  , setRequestQueryString
  )
import qualified Network.HTTP.Types.URI as Q (simpleQueryToQuery)
import Serializer.Telegram
  ( tMessageToPostMessage
  , tMessageToPostRepeatMessage
  , tReactionToCallbackAnswer
  )
import Telegram.Models

standardRequest :: String -> String
standardRequest = ("https://api.telegram.org/bot" ++)

makeGetUpdates :: Maybe Integer -> String -> HTTP.Request
makeGetUpdates offset token = HTTP.setRequestQueryString query req
  where
    req = HTTP.parseRequest_ $ "GET " ++ standardRequest token ++ "/getUpdates"
    simpleQuery =
      [ ("timeout", B8.pack "10")
      , ("allowed_updates[]", B8.pack "callback_query,message")
      ]
    addOffset x = ("offset", showToQueryItem x) : simpleQuery
    query = Q.simpleQueryToQuery $ maybe simpleQuery addOffset offset

makeSendMessage :: String -> TelegramMessage -> HTTP.Request
makeSendMessage token msg = HTTP.setRequestBodyJSON postMessage req
  where
    req =
      HTTP.parseRequest_ $ "POST " ++ standardRequest token ++ "/sendMessage"
    postMessage = tMessageToPostMessage msg

makeCallbackQuery :: String -> TelegramMessage -> HTTP.Request
makeCallbackQuery token msg = HTTP.setRequestBodyJSON postMessage req
  where
    req =
      HTTP.parseRequest_ $ "POST " ++ standardRequest token ++ "/sendMessage"
    postMessage = tMessageToPostRepeatMessage msg

makeAnswerCallbackQuery :: String -> TelegramReaction -> HTTP.Request
makeAnswerCallbackQuery token tr = HTTP.setRequestBodyJSON answerMessage req
  where
    req =
      HTTP.parseRequest_ $
      "POST " ++ standardRequest token ++ "/answerCallbackQuery"
    answerMessage = tReactionToCallbackAnswer tr
