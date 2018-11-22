{-# LANGUAGE OverloadedStrings #-}
module Slack.WebIO (runSlackBot) where

import Config
import Errors
import Logging
import Bot
import Helpers
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
import           Data.Maybe
import           Data.Aeson

{-To DO
-- /command problem
-}

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
    (timestamp, _, _, _, _, _, _) <- get
    conHistory <- makeConHistory
    response <- httpLBS conHistory
    checked <- liftIO $ runExceptT $ 
        catchError (hadleResponse response) sParsingErrorHandler
    let sJResponse = either (const emptySJResponse) id checked
        msgs = sortSJResponse $ messages sJResponse
        lastTS = case messages sJResponse of
            [] -> timestamp
            ms -> Just $ ts $ head ms
    --liftIO $ print sJResponse
    runExceptT $
        catchError (forM_ msgs handleMessage) sResponseErrorHandler

    (_, token, channel, hMsg, rMsg, r, dlog) <- get
    put (lastTS, token, channel, hMsg, rMsg, r, dlog)
    goSlackBot

sortSJResponse :: [SMessage] -> [T.Text]
sortSJResponse = foldl f [] where
    f txts sm = case user sm of
        Nothing -> txts
        _ -> text sm : txts

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
hadleResponse response = do
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe SJResponse
    when (isNothing parsed) $ throwError $ NoParse $ show unparsed
    return $ fromJust parsed

handleMessage :: T.Text -> 
    ExceptT BotError (StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool) IO) ()
handleMessage "/help" = do
    (_, _, _, hMsg, _, _, _) <- get
    postMessage hMsg
handleMessage "/repeat" = do
    (_, _, _, _, rMsg, r, _) <- get
    let rText = T.pack $ show r
        rnMsg = rMsg `T.append` rText
    postMessage rnMsg
handleMessage msg = do
    (_, _, _, _, _, r, _) <- get
    replicateM_ r $ postMessage msg

postMessage :: T.Text -> 
    ExceptT BotError (StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool) IO) ()
postMessage msgText = do
    (_, _, _, _, _, _, dlog) <- get
    req <- lift $ makePostMessage msgText
    response <- httpLBS req
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    currTime <- liftIO getCurrTime
    when dlog $ liftIO $ 
        (logDebug Slack) $ "\tA message was sent\n" `T.append` "\tText: " `T.append` msgText
    