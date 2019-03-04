{-# LANGUAGE OverloadedStrings #-}

module Telegram.Requests where

import qualified Network.HTTP.Simple           as HTTP
import           Helpers
import           Telegram.Models
import           Serializer.Telegram
import           Bot.Request
import           Data.Aeson

standardRequest :: String -> String
standardRequest = ("https://api.telegram.org/bot" ++)

makeGetUpdates :: Maybe Integer -> String -> Request
makeGetUpdates offset token = case offset of
  Nothing  -> reqWithEmptyBody $ HTTP.setRequestQueryString query req
  (Just o) -> reqWithEmptyBody
    $ HTTP.setRequestQueryString (("offset", showToQueryItem o) : query) req
 where
  req = HTTP.parseRequest_ $ "GET " ++ standardRequest token ++ "/getUpdates"
  query =
    [ ("timeout"          , Just "10")
    , ("allowed_updates[]", Just "callback_query,message")
    ]

makeSendMessage :: String -> TelegramMessage -> Request
makeSendMessage token msg = Request reqWithBody postMessage
 where
  req = HTTP.parseRequest_ $ "POST " ++ standardRequest token ++ "/sendMessage"
  postMessage = encode $ tMessageToPostMessage msg
  reqWithHeaders =
    HTTP.addRequestHeader "Content-Type" "application/json; charset=utf-8" req
  reqWithBody = HTTP.setRequestBodyLBS postMessage reqWithHeaders

makeCallbackQuery :: String -> TelegramMessage -> Request
makeCallbackQuery token msg = Request reqWithBody postMessage
 where
  req = HTTP.parseRequest_ $ "POST " ++ standardRequest token ++ "/sendMessage"
  postMessage = encode $ tMessageToPostRepeatMessage msg
  reqWithHeaders =
    HTTP.addRequestHeader "Content-Type" "application/json; charset=utf-8" req
  reqWithBody = HTTP.setRequestBodyLBS postMessage reqWithHeaders

makeAnswerCallbackQuery :: String -> TelegramReaction -> Request
makeAnswerCallbackQuery token tr = Request reqWithBody answerMessage
 where
  req =
    HTTP.parseRequest_
      $  "POST "
      ++ standardRequest token
      ++ "/answerCallbackQuery"
  answerMessage = encode $ tReactionToCallbackAnswer tr
  reqWithHeaders =
    HTTP.addRequestHeader "Content-Type" "application/json; charset=utf-8" req
  reqWithBody = HTTP.setRequestBodyLBS answerMessage reqWithHeaders
