{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.CaseInsensitive
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Bot
import           Errors
import           Logging
import           Helpers
import           Config
import           Data.Aeson
import           Data.Maybe (fromJust, isNothing)
import           Data.Either (either, fromLeft, fromRight)
import           Control.Monad (unless, when, zipWithM)
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Class
import           Prelude hiding (id)

main :: IO ()
main = do
    currTime <- getCurrTime
    appendFile "./log/debug.log" $ "LOG START at " ++ show currTime ++ "\n"
    appendFile "./log/error.log" $ "LOG START at " ++ show currTime ++ "\n"
    evalStateT sendLastMsgs Nothing

{-To DO
-- where to keep a token
-- could not parse stickers
-- how to make ./log directory and .log files on 'stack build' command
-}

--                     lastUpdtID
sendLastMsgs :: StateT (Maybe Integer) IO ()
sendLastMsgs = do
    offset <- get
    getUpdates <- liftIO $ makeGetUpdates offset
    response <- liftIO $ httpLBS $ getUpdates
    checked <- liftIO $ runExceptT $ 
        catchError (handleResponse response) parsingErrorHandler
    let jresponse = either (const emptyJResponse) myID checked

    unless (null $ result jresponse) $ do
        let updts = tail $ result jresponse
            lastUpdtID = case updts of
                [] -> offset
                _  -> Just $ update_id $ last updts
            msgText = text . message
            chatID = (id :: Chat -> Integer) . chat . message
        put lastUpdtID
        liftIO $ runExceptT $ 
            catchError (zipWithM_ sendMessage (fmap chatID updts) (fmap msgText updts)) responseErrorHandler
        return ()

    sendLastMsgs        

makeGetUpdates :: Maybe Integer -> IO Request
makeGetUpdates Nothing = do
    sr <- standardRequest
    return $ parseRequest_ $ "GET " ++ sr ++ "getUpdates"
makeGetUpdates (Just offset) = do
    sr <- standardRequest
    let req = parseRequest_ $ "GET " ++ sr ++ "getUpdates" 
        query = [("offset", Just $ BC.pack $ show offset)
                ,("timeout", Just "10")
                ,("allowed_updates[]", Just "message")]
    return $ setQueryString query req

handleResponse :: Response LB.ByteString -> ExceptT BotError IO JResponse
handleResponse response = do
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
    when (isNothing parsed) $ throwError $ NoParse $ show unparsed
    return $ fromJust parsed

sendMessage :: Integer -> T.Text -> ExceptT BotError IO ()
sendMessage chatID msgText = do
    sr <- liftIO standardRequest
    let req = parseRequest_ $ "POST " ++ sr ++  "sendMessage"
        chatIDText = T.pack $ show chatID
        bodyText = "{\"chat_id\": \"" `T.append` chatIDText `T.append` "\", \"text\": \"" `T.append` msgText `T.append` "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
        endReq = setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders
    response <- liftIO $ httpLBS endReq
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    currTime <- liftIO getCurrTime
    liftIO $ logDebug $ "\tA message was sent\n" `T.append` "\tTo: " `T.append` chatIDText `T.append` "\n" `T.append` "\tText: " `T.append` msgText


