{-# LANGUAGE OverloadedStrings #-}

module Telegram.Requests where

import Helpers (showToQueryItem)
import qualified Network.HTTP.Simple as HTTP
  ( Request(..)
  , parseRequest_
  , setRequestBodyJSON
  , setRequestQueryString
  )
import Serializer.Telegram
  ( tMessageToPostMessage
  , tMessageToPostRepeatMessage
  , tReactionToCallbackAnswer
  )
import Telegram.Models

standardRequest :: String -> String
standardRequest = ("https://api.telegram.org/bot" ++)

makeGetUpdates :: Maybe Integer -> String -> HTTP.Request
makeGetUpdates offset token =
  case offset of
    Nothing -> HTTP.setRequestQueryString query req
    (Just o) ->
      HTTP.setRequestQueryString (("offset", showToQueryItem o) : query) req
  where
    req = HTTP.parseRequest_ $ "GET " ++ standardRequest token ++ "/getUpdates"
    query =
      [ ("timeout", Just "10")
      , ("allowed_updates[]", Just "callback_query,message")
      ]

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
