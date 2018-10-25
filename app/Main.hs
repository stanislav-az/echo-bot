{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.CaseInsensitive
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Bot
import           Data.Aeson
import           Data.Maybe (fromJust)
import           Control.Monad (unless, when)
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Data.Maybe (maybe)
import           Prelude hiding (id)
import           Data.String (fromString)

main :: IO ()
main = evalStateT sendLastMsg (Nothing,Nothing)

--                     lastUpdateID   lastMsgID
sendLastMsg :: StateT (Maybe Integer, Maybe Integer) IO ()
sendLastMsg = do
    (offsetU, offsetM) <- get
    jresponse <- lift $ getJResponse offsetU
    unless (null $ result jresponse) $ do
        let lastUpdtID = jresponse & result & last & update_id
            msgText = jresponse & result & last & message & text
            chatID = jresponse & result & last & message & chat & id
            msgID = jresponse & result & last & message & message_id
            isNewUpdt = maybe True (lastUpdtID /=) offsetU
            isNewMsg = maybe False (msgID /=) offsetM
        put (Just lastUpdtID, Just msgID)
        when (isNewUpdt && isNewMsg) $ do
            lift $ putStrLn msgText
            msgSent <- lift $ sendMessage chatID msgText
            (notChanged, _) <- get
            put (notChanged, Just $ message_id msgSent) 
    sendLastMsg

getJResponse :: Maybe Integer -> IO JResponse
getJResponse offset = do 
    response <- httpLBS $ getUpdates offset
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
    return $ fromJust parsed

standardRequest :: String
standardRequest = "https://api.telegram.org/bot631489276:AAGPK2n_xXE7_nB95uBkui2U6dMxcdbLMyM/"

getUpdates :: Maybe Integer -> Request
getUpdates Nothing = parseRequest_ $ standardRequest ++ "getUpdates"
getUpdates (Just offset) = parseRequest_ $ standardRequest ++ "getUpdates" ++ "?offset=" ++ show offset

sendMessage :: Integer -> String -> IO Message
sendMessage chatID msgText = do
    let req = parseRequest_ $ "POST " ++ standardRequest ++  "sendMessage"
        bodyStr = "{\"chat_id\": \"" ++ show chatID ++ "\", \"text\": \"" ++ msgText ++ "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
        endReq = setRequestBody (fromString bodyStr) reqWithHeaders
    response <- httpLBS endReq
    let unparsed = getResponseBody response
        parsed = decode (LB.init $ LB.drop 20 unparsed) :: Maybe Message
    return $ fromJust parsed
    
infixl 0 &
(&) = flip ($)

msg :: Message
msg = undefined