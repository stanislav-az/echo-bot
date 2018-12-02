{-# LANGUAGE OverloadedStrings #-}
module TelegramTests where

import Telegram.WebIOInternal
import Telegram.Bot
import Config
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Generic.Random
import Generic.Random.Tutorial
import Data.Maybe
import Control.Monad.State
import Data.HashMap.Strict hiding (null)
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Data.CaseInsensitive
import qualified Data.ByteString as B

{-
All Requests are not compared properly. Request data has no Eq instance.
So only hosts, paths, headers, queries are compared because 
Request data has only Show instance. The part omited is request body and
RequestBody data has no Eq or Show instances.
-}

runTelegramTests :: IO ()
runTelegramTests = hspec $ do
    describeUpdateState
    describeSortJResponse
    describeFindLastUpdtID
    describeMakeGetUpdates
    describeMakeSendMessage
    describeMakeCallbackQuery
    describeMakeAnswerCallbackQuery

instance Arbitrary JResponse where
    arbitrary = genericArbitraryU 

instance Arbitrary Update where
    arbitrary = genericArbitraryU 

instance Arbitrary Message where
    arbitrary = genericArbitraryU 

instance Arbitrary Chat where
    arbitrary = genericArbitraryU 

instance Arbitrary CallbackQuery where
    arbitrary = genericArbitraryU 

describeUpdateState :: SpecWith ()
describeUpdateState = do
    describe "updateState" $ do
        it "Should update only offset" $ do
            execStateT (updateState $ Just 5) 
                (Nothing, "sr", "hMsg", "rMsg", 1, empty, True) `shouldReturn`
                (Just 5, "sr", "hMsg", "rMsg", 1, empty, True)

describeSortJResponse :: SpecWith ()
describeSortJResponse = do
    describe "sortJResponse" $ do
        it "Should have no messages or queries when there is no updates" $ do
            sortJResponse emptyJResponse Nothing `shouldBe` ([],[])

describeFindLastUpdtID :: SpecWith ()
describeFindLastUpdtID = do
    describe "findLastUpdtID" $ do
        it "Returns Nothing if and only if there is no updates and offset is Nothing" $ property $ 
            \us ot -> ((null us) && (isNothing ot)) == (isNothing $ findLastUpdtID us ot)

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

telegramToken :: String
telegramToken = "00:AA"