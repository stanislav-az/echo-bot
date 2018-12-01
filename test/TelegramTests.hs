{-# LANGUAGE OverloadedStrings #-}
module TelegramTests where

import Telegram.WebIOInternal
import Config
import Tokens
import Test.Hspec
import Control.Monad.State
import Data.HashMap.Strict
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Data.CaseInsensitive
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

{-
All Requests are not compared properly. Request data has no Eq instance.
So only hosts, paths, headers, queries are compared because 
Request data has only Show instance. The part omited is request body and
RequestBody data has no Eq or Show instances.
-}

runTelegramTests :: IO ()
runTelegramTests = hspec $ do
    describeUpdateState
    describeMakeGetUpdates
    describeMakeSendMessage
    describeMakeCallbackQuery
    describeMakeAnswerCallbackQuery

describeUpdateState :: SpecWith ()
describeUpdateState = do
    describe "updateState" $ do
        it "Should update only offset" $ do
            execStateT (updateState $ Just 5) 
                (Nothing, "sr", "hMsg", "rMsg", 1, empty, True) `shouldReturn`
                (Just 5, "sr", "hMsg", "rMsg", 1, empty, True)

describeMakeGetUpdates :: SpecWith ()
describeMakeGetUpdates = do
    describe "makeGetUpdates" $ do
        it "Should make correct requests w/o offset" $ do
            (fmap show $ evalStateT makeGetUpdates 
                (Nothing, standardRequest, "hMsg", "rMsg", 1, empty, True)) `shouldReturn`
                getUpdatesReq
        it "Should make correct requests with offset" $ do
            (fmap show $ evalStateT makeGetUpdates 
                (Just 5, standardRequest, "hMsg", "rMsg", 1, empty, True)) `shouldReturn`
                getUpdatesReqWithQuery

describeMakeSendMessage :: SpecWith ()
describeMakeSendMessage = do
    describe "makeSendMessage" $ do
        it "Should make correct requests" $ do
            (fmap show $ evalStateT (makeSendMessage 123 "hello") 
                (Nothing, standardRequest, "hMsg", "rMsg", 1, empty, True)) `shouldReturn`
                sendMessageReq

describeMakeCallbackQuery :: SpecWith ()
describeMakeCallbackQuery = do
    describe "makeCallbackQuery" $ do
        it "Should make correct requests" $ do
            (fmap show $ evalStateT (makeCallbackQuery 123 "hello") 
                (Nothing, standardRequest, "hMsg", "rMsg", 1, empty, True)) `shouldReturn`
                callbackQueryReq                

describeMakeAnswerCallbackQuery :: SpecWith ()
describeMakeAnswerCallbackQuery = do
    describe "makeAnswerCallbackQuery" $ do
        it "Should make correct requests" $ do
            (fmap show $ evalStateT (makeAnswerCallbackQuery "123" "3") 
                (Nothing, standardRequest, "hMsg", "rMsg", 1, empty, True)) `shouldReturn`
                answerCallbackQueryReq    

getUpdatesReq :: String
getUpdatesReq = show $ parseRequest_ $ "GET https://api.telegram.org/bot" ++ 
    telegramToken ++ "/getUpdates"

getUpdatesReqWithQuery :: String
getUpdatesReqWithQuery = show endReq where
    req = parseRequest_ $ "GET " ++ standardRequest ++ "getUpdates" 
    query = [("offset", Just "5")
            ,("timeout", Just "10")
            ,("allowed_updates[]", Just "callback_query,message")]
    endReq = setQueryString query req

standardRequest :: String
standardRequest = "https://api.telegram.org/bot" ++ telegramToken ++ "/" 

sendMessageReq :: String
sendMessageReq = show reqWithHeaders where
    req = parseRequest_ $ "POST " ++ standardRequest ++  "sendMessage"
    reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req

callbackQueryReq :: String
callbackQueryReq = show reqWithHeaders where
    req = parseRequest_ $ "POST " ++ standardRequest ++  "sendMessage"
    reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req

answerCallbackQueryReq :: String
answerCallbackQueryReq = show reqWithHeaders where
    req = parseRequest_ $ "POST " ++ standardRequest ++  "answerCallbackQuery"
    reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
