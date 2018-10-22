{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Simple
import           Bot
import           Data.Aeson
import           Data.Maybe (fromJust)
import           Control.Monad (unless, when)
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Data.Maybe (maybe)

main :: IO ()
main = evalStateT printLastMsg Nothing

printLastMsg :: StateT (Maybe Integer) IO ()
printLastMsg = do
    offset <- get
    jresponse <- lift $ getJResponse offset
    unless (null $ result jresponse) $ do
        let lastID = lastUpdtID jresponse
            msgText = jresponse & result & last & message & text
        put $ Just lastID
        when (maybe True (lastID /=) offset) (lift $ putStrLn msgText)
    printLastMsg

getJResponse :: Maybe Integer -> IO JResponse
getJResponse offset = do 
    response <- httpLBS $ getUpdates offset
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
    return $ fromJust parsed

lastUpdtID :: JResponse -> Integer
lastUpdtID = update_id . last . result

standardRequest :: String
standardRequest = "https://api.telegram.org/bot631489276:AAGPK2n_xXE7_nB95uBkui2U6dMxcdbLMyM/"

getUpdates :: Maybe Integer -> Request
getUpdates Nothing = parseRequest_ $ standardRequest ++ "getUpdates"
getUpdates (Just offset) = parseRequest_ $ standardRequest ++ "getUpdates" ++ "?offset=" ++ show offset

sendMessage :: Request
sendMessage = parseRequest_ $ standardRequest ++ "sendMessage"

infixl 0 &
(&) = flip ($)