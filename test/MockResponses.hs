{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MockResponses where

import Control.Monad.Catch
import qualified Data.ByteString as B (ByteString(..))
import qualified Data.ByteString.Lazy as LB (ByteString(..), empty)
import qualified Network.HTTP.Client.Internal as Client
  ( CookieJar(..)
  , Request(..)
  , Response(..)
  , ResponseClose(..)
  )
import qualified Network.HTTP.Types as Types (http11, status404, status429,status200)

type ResponseLBS = Client.Response LB.ByteString

type Path = B.ByteString

data SlackResponseStack = SlackResponseStack
  { conHistoryRes :: Maybe ResponseLBS
  , getReactionsRes :: Maybe ResponseLBS
  , postMessageRes :: [ResponseLBS]
  } deriving (Show)

data SlackRequestStack = SlackRequestStack
  { conHistoryReq :: [Client.Request]
  , getReactionsReq :: [Client.Request]
  , postMessageReq :: [Client.Request]
  } deriving (Show)

data TelegramResponseStack = TelegramResponseStack
  { getUpdatesRes :: Maybe ResponseLBS
  , sendMessageRes :: [ResponseLBS]
  , answerCallbackQueryRes :: [ResponseLBS]
  } deriving (Show)

data TelegramRequestStack = TelegramRequestStack
  { getUpdatesReq :: [Client.Request]
  , sendMessageReq :: [Client.Request]
  , answerCallbackQueryReq :: [Client.Request]
  } deriving (Show)

data TestException =
  TooManyRequests
  deriving (Eq, Show)

instance Exception TestException

notFoundResponse :: ResponseLBS
notFoundResponse =
  Client.Response
    { Client.responseStatus = Types.status404
    , Client.responseVersion = Types.http11
    , Client.responseHeaders = []
    , Client.responseBody = LB.empty
    , Client.responseCookieJar = Client.CJ {Client.expose = []}
    , Client.responseClose' = Client.ResponseClose $ pure ()
    }

tooManyRequestsResponse :: ResponseLBS
tooManyRequestsResponse =
  Client.Response
    { Client.responseStatus = Types.status429
    , Client.responseVersion = Types.http11
    , Client.responseHeaders = []
    , Client.responseBody = LB.empty
    , Client.responseCookieJar = Client.CJ {Client.expose = []}
    , Client.responseClose' = Client.ResponseClose $ pure ()
    }

makeOkResWithBody :: LB.ByteString -> ResponseLBS
makeOkResWithBody body =
  Client.Response
    { Client.responseStatus = Types.status200
    , Client.responseVersion = Types.http11
    , Client.responseHeaders =
        [("Content-Type", "application/json; charset=utf-8")]
    , Client.responseBody = body
    , Client.responseCookieJar = Client.CJ {Client.expose = []}
    , Client.responseClose' = Client.ResponseClose $ pure ()
    }

ok :: ResponseLBS
ok = makeOkResWithBody LB.empty

emptySlackRequestStack :: SlackRequestStack
emptySlackRequestStack = SlackRequestStack [] [] []

emptySlackResponseStack :: SlackResponseStack
emptySlackResponseStack = SlackResponseStack Nothing Nothing []

countConHistoryReqs :: SlackRequestStack -> Int
countConHistoryReqs SlackRequestStack {..} = length conHistoryReq

countGetReactionsReqs :: SlackRequestStack -> Int
countGetReactionsReqs SlackRequestStack {..} = length getReactionsReq

countPostMessageReqs :: SlackRequestStack -> Int
countPostMessageReqs SlackRequestStack {..} = length postMessageReq

countSlackReqs :: SlackRequestStack -> (Int, Int, Int)
countSlackReqs stack =
  ( countConHistoryReqs stack
  , countGetReactionsReqs stack
  , countPostMessageReqs stack)

emptyTelegramRequestStack :: TelegramRequestStack
emptyTelegramRequestStack = TelegramRequestStack [] [] []

emptyTelegramResponseStack :: TelegramResponseStack
emptyTelegramResponseStack = TelegramResponseStack Nothing [] []

countGetUpdatesReqs :: TelegramRequestStack -> Int
countGetUpdatesReqs TelegramRequestStack {..} = length getUpdatesReq

countSendMessageReqs :: TelegramRequestStack -> Int
countSendMessageReqs TelegramRequestStack {..} = length sendMessageReq

countAnswerCallbackQueryReqs :: TelegramRequestStack -> Int
countAnswerCallbackQueryReqs TelegramRequestStack {..} =
  length answerCallbackQueryReq

countTelegramReqs :: TelegramRequestStack -> (Int, Int, Int)
countTelegramReqs stack =
  ( countGetUpdatesReqs stack
  , countSendMessageReqs stack
  , countAnswerCallbackQueryReqs stack)
