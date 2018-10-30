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
import           Control.Monad.Trans.Class
import           Data.Maybe (maybe)
import           Prelude hiding (id)
import           Data.String (fromString)

main :: IO ()
main = evalStateT sendLastMsgs Nothing

--                     lastUpdtID
sendLastMsgs :: StateT (Maybe Integer) IO ()
sendLastMsgs = do
    offsetU <- get
    jresponse <- lift $ getJResponse offsetU
    unless (null $ result jresponse) $ do
        let updts = tail $ result jresponse
            lastUpdtID = case updts of
                [] -> offsetU
                _  -> Just $ update_id $ last updts
            msgText = text . message
            chatID = id . chat . message
        put lastUpdtID
        lift $ zipWithM_ sendMessage (fmap chatID updts) (fmap msgText updts)
    sendLastMsgs

getJResponse :: Maybe Integer -> IO JResponse
getJResponse offset = do 
    response <- httpLBS $ getUpdates offset
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
    return $ fromJust parsed -- потенциальная ошибка не обрабатыватся

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
    return ()
