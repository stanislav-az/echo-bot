{-# LANGUAGE OverloadedStrings #-}
module Slack.WebIO (runSlackBot) where

import Config
import Errors
import Slack.Bot
import qualified Data.Text as T
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.Text.Encoding (encodeUtf8)
import           Data.CaseInsensitive
import           Data.String
import           Network.HTTP.Simple
import           Control.Monad.State
import           Control.Monad.Except

runSlackBot :: IO ()
runSlackBot = do
    (token, channel) <- slackConfig
    hMsg <- helpMsg
    rMsg <- repeatMsg
    r <- defaultRepeat
    dlog <- debugLogging
    evalStateT goSlackBot (Nothing, token, channel, hMsg, rMsg, r, dlog)
    
--                    timestamp     token   channel hMsg    rMsg    r    dlog
goSlackBot :: StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool) IO ()
goSlackBot = do
    conHistory <- makeConHistory
    response <- httpLBS conHistory
    checked <- liftIO $ runExceptT $ 
        catchError (hadleResponse response) sParsingErrorHandler
    liftIO $ print $ getResponseBody response

makeConHistory :: StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool) IO Request
makeConHistory = do
    (timestamp, token, channel, _, _, _, _) <- get
    let reqString = "GET " ++ "https://slack.com/api/conversations.history?token="
            ++ token ++ "&channel=" ++ channel
    case timestamp of 
        Nothing -> return $ parseRequest_ $ reqString ++ "&limit=1" 
        (Just ts) -> return $ parseRequest_ $ reqString ++ "&oldest=" ++ ts

makePostMessage :: T.Text ->
    StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool) IO Request
makePostMessage msgText = do
    (_, token, channel, _, _, _, _) <- get
    let req = parseRequest_ $ "POST " ++ "https://slack.com/api/chat.postMessage"
        bodyText = "{\"channel\": \"" `T.append` (T.pack channel) `T.append`
            "\", \"text\": \"" `T.append` msgText `T.append` "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json; charset=utf-8"),
            ("Authorization" :: CI B.ByteString, "Bearer " `B.append` (fromString token))] req
        endReq = setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders
    return endReq

hadleResponse :: Response LB.ByteString -> ExceptT BotError IO SJResponse
hadleResponse = undefined