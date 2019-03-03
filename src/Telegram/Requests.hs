{-# LANGUAGE OverloadedStrings #-}

module Telegram.Requests where

import           Network.HTTP.Simple
import           Helpers
import           Telegram.Models
import           Serializer.Telegram

standardRequest :: String -> String
standardRequest = ("https://api.telegram.org/bot" ++)

makeGetUpdates :: Maybe Integer -> String -> Request
makeGetUpdates offset token = case offset of
  Nothing  -> setRequestQueryString query req
  (Just o) -> setRequestQueryString (("offset", showToQueryItem o) : query) req
 where
  req = parseRequest_ $ "GET " ++ standardRequest token ++ "/getUpdates"
  query =
    [ ("timeout"          , Just "10")
    , ("allowed_updates[]", Just "callback_query,message")
    ]

makeSendMessage :: String -> TelegramMessage -> Request
makeSendMessage token msg = setRequestBodyJSON postMessage req
 where
  req = parseRequest_ $ "POST " ++ standardRequest token ++ "/sendMessage"
  postMessage = tMessageToPostMessage msg

makeCallbackQuery :: String -> TelegramMessage -> Request
makeCallbackQuery token msg = setRequestBodyJSON postMessage req
 where
  req = parseRequest_ $ "POST " ++ standardRequest token ++ "/sendMessage"
  postMessage = tMessageToPostRepeatMessage msg

makeAnswerCallbackQuery :: String -> TelegramReaction -> Request
makeAnswerCallbackQuery token tr = setRequestBodyJSON answerMessage req
 where
  req =
    parseRequest_ $ "POST " ++ standardRequest token ++ "/answerCallbackQuery"
  answerMessage = tReactionToCallbackAnswer tr
