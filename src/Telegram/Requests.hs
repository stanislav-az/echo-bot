{-# LANGUAGE OverloadedStrings #-}

module Telegram.Requests where

import qualified Data.ByteString.Char8 as B8 (pack)
import qualified Data.Text as T (Text(..))
import Ext.Network.HTTP.Types.URI (showToQueryItem)
import qualified Network.HTTP.Simple as HTTP
  ( Request(..)
  , parseRequest_
  , setRequestBodyJSON
  , setRequestQueryString
  )
import qualified Network.HTTP.Types.URI as Q (simpleQueryToQuery)
import Serializer.Telegram
  ( constructTCallbackAnswer
  , constructTPostMessage
  , constructTPostRepeatMessage
  )

standardRequest :: String -> String
standardRequest = ("https://api.telegram.org/bot" ++)

makeGetUpdates :: String -> Maybe Integer -> HTTP.Request
makeGetUpdates token offset = HTTP.setRequestQueryString query req
  where
    req = HTTP.parseRequest_ $ "GET " ++ standardRequest token ++ "/getUpdates"
    simpleQuery =
      [ ("timeout", B8.pack "10")
      , ("allowed_updates[]", B8.pack "callback_query,message")
      ]
    addOffset x = ("offset", showToQueryItem x) : simpleQuery
    query = Q.simpleQueryToQuery $ maybe simpleQuery addOffset offset

makeSendMessage :: String -> Integer -> T.Text -> HTTP.Request
makeSendMessage token chatId text = HTTP.setRequestBodyJSON postMessage req
  where
    req =
      HTTP.parseRequest_ $ "POST " ++ standardRequest token ++ "/sendMessage"
    postMessage = constructTPostMessage chatId text

makeCallbackQuery :: String -> Integer -> T.Text -> HTTP.Request
makeCallbackQuery token chatId text = HTTP.setRequestBodyJSON postMessage req
  where
    req =
      HTTP.parseRequest_ $ "POST " ++ standardRequest token ++ "/sendMessage"
    postMessage = constructTPostRepeatMessage chatId text

makeAnswerCallbackQuery :: String -> String -> T.Text -> HTTP.Request
makeAnswerCallbackQuery token queryId text =
  HTTP.setRequestBodyJSON answerMessage req
  where
    req =
      HTTP.parseRequest_ $
      "POST " ++ standardRequest token ++ "/answerCallbackQuery"
    answerMessage = constructTCallbackAnswer queryId text
