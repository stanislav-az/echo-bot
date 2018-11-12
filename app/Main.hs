{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Maybe (fromJust, isNothing, catMaybes)
import           Data.Either (either, fromLeft, fromRight)
import           Control.Monad (unless, when, forM_, join, replicateM_)
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
-- load config on every request or put all parameters to StateT
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
        let updts = case offset of
                Nothing -> result jresponse
                _ -> tail $ result jresponse
            lastUpdtID = case updts of
                [] -> offset
                _  -> Just $ update_id $ last updts
            post = envelop updts
        put lastUpdtID
        liftIO $ runExceptT $ 
            catchError (forM_ post $ uncurry handleMessage) responseErrorHandler
        return ()

    sendLastMsgs        

envelop :: [Update] -> [(Integer,T.Text)]
envelop updts = catMaybes $ fmap (join . make) updts where
    make updt = fmap f $ message updt
    f msg = case text msg of 
            Nothing -> Nothing
            textMsg -> Just (id $ chat msg, fromJust $ text msg)

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

handleMessage :: Integer -> T.Text -> ExceptT BotError IO ()
handleMessage chatID "/help" = do
    hMsg <- liftIO helpMsg
    sendMessage chatID hMsg
handleMessage chatID "/repeat" = do
    rMsg <- liftIO repeatMsg
    sendMessage chatID rMsg
handleMessage chatID msg = do
    num <- liftIO getRepeat
    replicateM_ num $ sendMessage chatID msg

getRepeat :: IO Int
getRepeat = defaultRepeat