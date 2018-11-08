{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import           Data.CaseInsensitive
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Bot
import           Errors
import           Data.Aeson
import           Data.Maybe (fromJust, isNothing)
import           Control.Monad (unless, when, zipWithM)
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Except
import           Control.Monad.Trans.Class
import           Data.Maybe (maybe)
import           Prelude hiding (id)
import           Data.String (fromString)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import           Data.Time.Clock (UTCTime(..))

main :: IO ()
main = do
    writeFile "./log/debug.log" ""
    writeFile "./log/error.log" ""
    evalStateT (runExceptT $ catchError sendLastMsgs botErrorHandler) Nothing
    return ()

botErrorHandler :: BotError -> ExceptT BotError (StateT (Maybe Integer) IO) ()
botErrorHandler (NoParse body) = do
    currTime <- lift $ lift $ getCurrTime
    lift $ lift $ appendFile "./log/error.log" $ "ERROR entry at " ++ show currTime ++ "\n"
                                              ++ "\tCould not parse response body\n" 
                                              ++ "\tResponse body was: " ++ body ++ "\n"
botErrorHandler (ResponseError status) = do
    currTime <- lift $ lift $ getCurrTime
    lift $ lift $ appendFile "./log/error.log" $ "ERROR entry at " ++ show currTime ++ "\n"
                                              ++ "\tResponse status was not ok\n" 
                                              ++ "\tResponse status was: " ++ status ++ "\n"

{-
writeLog :: WriterT LB.ByteString IO () -> IO ()
writeLog w = do
    logBS <- execWriterT w
    LB.appendFile "./log/debug.log" logBS
    -- время событие 
    -- debug, error

tell $ LB.fromStrict $ BC.pack $ 
-}

{-To DO
--where to keep a token
--is it ok to write debug/warning logs in situ
--is there another way to clear file on execution than [writeFile "./log/debug.log" ""]
-}

--                     lastUpdtID
sendLastMsgs :: ExceptT BotError (StateT (Maybe Integer) IO) ()
sendLastMsgs = do
    offset <- get
    response <- lift $ lift $ httpLBS $ getUpdates offset
    
    let isOK = case getResponseStatusCode response of
                    200 -> True
                    _   -> False
    unless isOK $ throwError $ ResponseError $ show $ getResponseStatus response
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
    when (isNothing parsed) $ throwError $ NoParse $ show unparsed
    let jresponse = fromJust parsed

    unless (null $ result jresponse) $ do
        let updts = tail $ result jresponse
            lastUpdtID = case updts of
                [] -> offset
                _  -> Just $ update_id $ last updts
            msgText = text . message
            chatID = id . chat . message
        put lastUpdtID
        lift $ lift $ zipWithM_ sendMessage (fmap chatID updts) (fmap msgText updts)
    
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
    currTime <- getCurrTime
    appendFile "./log/debug.log" $ "DEBUG entry at " ++ show currTime ++ "\n"
                                ++ "\tA message was sent\n" 
                                ++ "\tTo: " ++ show chatID ++ "\n"
                                ++ "\tText: " ++ msgText ++ "\n"

getCurrTime :: IO UTCTime
getCurrTime = do
    sysTime <- getSystemTime
    return $ systemToUTCTime sysTime