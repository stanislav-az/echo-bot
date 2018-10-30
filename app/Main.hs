{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import           Data.CaseInsensitive
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Bot
import           Data.Aeson
import           Data.Maybe (fromJust)
import           Control.Monad (unless, when, zipWithM)
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Trans.Class
import           Data.Maybe (maybe)
import           Prelude hiding (id)
import           Data.String (fromString)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)

main :: IO ()
main = do
    writeFile "./log/debug.log" ""
    evalStateT sendLastMsgs Nothing

{-
writeLog :: WriterT LB.ByteString IO () -> IO ()
writeLog w = do
    logBS <- execWriterT w
    LB.appendFile "./log/debug.log" logBS
    -- время событие 
    -- debug, error

tell $ LB.fromStrict $ BC.pack $ 
-}

--                     lastUpdtID
sendLastMsgs :: StateT (Maybe Integer) IO ()
sendLastMsgs = do
    offset <- get
    response <- lift $ httpLBS $ getUpdates offset
    
    let iLog = "The response status code was: " ++ (show $ getResponseStatusCode response) ++ "\n"
        unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
        jresponse = fromJust parsed -- потенциальная ошибка не обрабатыватся

    unless (null $ result jresponse) $ do
        let updts = tail $ result jresponse
            lastUpdtID = case updts of
                [] -> offset
                _  -> Just $ update_id $ last updts
            msgText = text . message
            chatID = id . chat . message
        put lastUpdtID
        lift $ zipWithM_ sendMessage (fmap chatID updts) (fmap msgText updts)
    
    --lift $ appendFile "./log/debug.log" iLog
    sendLastMsgs          

standardRequest :: String
standardRequest = "https://api.telegram.org/bot631489276:AAGPK2n_xXE7_nB95uBkui2U6dMxcdbLMyM/"

getUpdates :: Maybe Integer -> Request
getUpdates Nothing = parseRequest_ $ "GET " ++ standardRequest ++ "getUpdates"
getUpdates (Just offset) = setQueryString query req where
    req = parseRequest_ $ "GET " ++ standardRequest ++ "getUpdates" 
    query = [("offset", Just $ BC.pack $ show offset)
            ,("timeout", Just "10")
            ,("allowed_updates[]", Just "message")]

sendMessage :: Integer -> String -> IO ()
sendMessage chatID msgText = do
    let req = parseRequest_ $ "POST " ++ standardRequest ++  "sendMessage"
        bodyStr = "{\"chat_id\": \"" ++ show chatID ++ "\", \"text\": \"" ++ msgText ++ "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
        endReq = setRequestBody (fromString bodyStr) reqWithHeaders
    httpLBS endReq
    sysTime <- getSystemTime
    let currTime = systemToUTCTime sysTime
    appendFile "./log/debug.log" $ "DEBUG entry at " ++ show currTime ++ "\n"
                                    ++"\tA message was sent\n" 
                                    ++ "\tTo: " ++ show chatID ++ "\n"
                                    ++ "\tText: " ++ msgText ++ "\n"
