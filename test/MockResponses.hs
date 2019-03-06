{-# LANGUAGE OverloadedStrings #-}

module MockResponses where

import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString               as B
import qualified Network.HTTP.Client.Internal  as Client
import qualified Network.HTTP.Types            as Types
import           Control.Monad.Catch
import           RequestBody

type ResponseLBS = Client.Response LB.ByteString
type Path = B.ByteString

data SlackResponseStack = SlackResponseStack {
  conHistoryRes :: Maybe ResponseLBS ,
  getReactionsRes :: Maybe ResponseLBS ,
  postMessageRes :: [ResponseLBS]
} deriving Show

emptySlackResponseStack :: SlackResponseStack
emptySlackResponseStack = SlackResponseStack Nothing Nothing []

data SlackRequestStack = SlackRequestStack {
  conHistoryReq :: [Client.Request] ,
  getReactionsReq :: [Client.Request] ,
  postMessageReq :: [Client.Request]
} deriving Show

emptySlackRequestStack :: SlackRequestStack
emptySlackRequestStack = SlackRequestStack [] [] []

data TestException = TooManyRequests
  deriving (Eq, Show)

instance Exception TestException

notFoundResponse :: ResponseLBS
notFoundResponse = Client.Response
  { Client.responseStatus    = Types.status404
  , Client.responseVersion   = Types.http11
  , Client.responseHeaders   = []
  , Client.responseBody      = LB.empty
  , Client.responseCookieJar = Client.CJ { Client.expose = [] }
  , Client.responseClose'    = Client.ResponseClose $ pure ()
  }

tooManyRequestsResponse :: ResponseLBS
tooManyRequestsResponse = Client.Response
  { Client.responseStatus    = Types.status429
  , Client.responseVersion   = Types.http11
  , Client.responseHeaders   = []
  , Client.responseBody      = LB.empty
  , Client.responseCookieJar = Client.CJ { Client.expose = [] }
  , Client.responseClose'    = Client.ResponseClose $ pure ()
  }

makeOkResWithBody :: LB.ByteString -> Client.Response LB.ByteString
makeOkResWithBody body = Client.Response
  { Client.responseStatus    = Types.status200
  , Client.responseVersion   = Types.http11
  , Client.responseHeaders   = [ ( "Content-Type"
                                 , "application/json; charset=utf-8"
                                 )
                               ]
  , Client.responseBody      = body
  , Client.responseCookieJar = Client.CJ { Client.expose = [] }
  , Client.responseClose'    = Client.ResponseClose $ pure ()
  }