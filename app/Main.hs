{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Simple
import           Bot
import           Data.Aeson
import           Data.Maybe (fromJust)
import           Control.Monad (unless)

main :: IO ()
main = fun 0

fun :: Integer -> IO ()
fun offset = do
    response <- httpLBS $ getUpdates offset
    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
        lastMsg = parsed & fromJust & result & last & message & message_id
    --LB.putStrLn unparsed
    print parsed
    unless (lastMsg == offset) (fun lastMsg)
    
standardRequest :: String
standardRequest = "https://api.telegram.org/bot631489276:AAGPK2n_xXE7_nB95uBkui2U6dMxcdbLMyM/"

getUpdates :: Integer -> Request
getUpdates offset = parseRequest_ $ standardRequest ++ "getUpdates" ++ "?offset=" ++ show offset

sendMessage :: Request
sendMessage = parseRequest_ $ standardRequest ++ "sendMessage"

infixl 0 &
(&) = flip ($)