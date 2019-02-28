{-# LANGUAGE OverloadedStrings #-}
module SlackTests where

import Slack.WebIOInternal
import Slack.Bot
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
import Data.String
import qualified Data.ByteString as B

{-
All Requests are not compared properly. Request data has no Eq instance.
So only hosts, paths, headers, queries are compared because 
Request data has only Show instance. The part omited is request body and
RequestBody data has no Eq or Show instances.
-}

runSlackTests :: IO ()
runSlackTests = hspec $ do
    describeUpdateAndDelay
    describeSortSJResponse
    describeFindLastTS
    describeParseReaction
    describeMakeConHistory
    describeMakePostMessage  
    describeMakeGetReactions

instance Arbitrary SJResponse where
    arbitrary = genericArbitraryU

instance Arbitrary SMessage where
    arbitrary = genericArbitraryU

instance Arbitrary SReaction where
    arbitrary = genericArbitraryU    

describeUpdateAndDelay :: SpecWith ()
describeUpdateAndDelay = do
    describe "updateAndDelay" $ do
        it "Should update only timestamp" $ do
            execStateT (updateAndDelay $ Just "000") 
                (Nothing, "token", "channel", "hMsg", "rMsg", 1, True, Nothing) `shouldReturn`
                (Just "000", "token", "channel", "hMsg", "rMsg", 1, True, Nothing)

describeSortSJResponse :: SpecWith ()
describeSortSJResponse = do 
    describe "sortSJResponse" $ do
        it "Never skips messages" $ property $
            \mbmsgs -> (length $ fromMaybe [] mbmsgs) >= (length $ sortSJResponse mbmsgs)

describeFindLastTS :: SpecWith ()
describeFindLastTS = do
    describe "findLastTS" $ do
        it "Should work for some examples" $ do
            findLastTS emptySJResponse Nothing `shouldBe` Nothing

describeParseReaction :: SpecWith ()
describeParseReaction = do
    describe "parseReaction" $ do
        it "Should work for some examples" $ do
            parseReaction (Just ["three"]) `shouldBe` (Just 3)

describeMakeConHistory :: SpecWith ()
describeMakeConHistory = do
    describe "makeConHistory" $ do
        it "Should make correct requests w/o timestamp" $ do
            (fmap show $ evalStateT makeConHistory 
                (Nothing, slackToken, slackChannel, "hMsg", "rMsg", 1, True, Nothing)) `shouldReturn`
                conHistoryReq
        it "Should make correct requests with timestamp" $ do
            (fmap show $ evalStateT makeConHistory 
                (Just "000", slackToken, slackChannel, "hMsg", "rMsg", 1, True, Nothing)) `shouldReturn`
                conHistoryReqWithTS

describeMakePostMessage :: SpecWith ()
describeMakePostMessage = do
    describe "makePostMessage" $ do
        it "Should make correct requests" $ do
            (fmap show $ evalStateT (makePostMessage "hello") 
                (Nothing, slackToken, slackChannel, "hMsg", "rMsg", 1, True, Nothing)) `shouldReturn`
                postMessageReq

describeMakeGetReactions :: SpecWith ()
describeMakeGetReactions = do
    describe "makeGetReactions" $ do
        it "Should make correct requests with timestamp" $ do
            (fmap show $ evalStateT makeGetReactions
                (Nothing, slackToken, slackChannel, "hMsg", "rMsg", 1, True, Just "000")) `shouldReturn`
                getReactionsReq
{- -- Maybe this does not work because StateT is lazy ?
        it "Should throw error w/o timestamp" $ do
            (fmap show $ evalStateT makeGetReactions
                (Nothing, slackToken, slackChannel, "hMsg", "rMsg", 1, True, Nothing)) `shouldThrow`
                anyIOException-}

conHistoryReq :: String
conHistoryReq = show $ parseRequest_ reqString where
    reqString = "GET " ++ "https://slack.com/api/conversations.history?token=" ++
        slackToken ++ "&channel=" ++ slackChannel ++ "&limit=1" 

conHistoryReqWithTS :: String
conHistoryReqWithTS = show $ parseRequest_ reqString where
    reqString = "GET " ++ "https://slack.com/api/conversations.history?token=" ++
        slackToken ++ "&channel=" ++ slackChannel ++ "&oldest=000"

postMessageReq :: String
postMessageReq = show reqWithHeaders where
    req = parseRequest_ $ "POST " ++ "https://slack.com/api/chat.postMessage"
    reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json; charset=utf-8"),
        ("Authorization" :: CI B.ByteString, "Bearer " <> (fromString slackToken))] req

getReactionsReq :: String
getReactionsReq = show $ parseRequest_ reqString where
    reqString = "GET " ++ "https://slack.com/api/reactions.get?token="
            ++ slackToken ++ "&channel=" ++ slackChannel ++ "&timestamp=000"

slackChannel :: String
slackChannel = "DE0000" 

slackToken :: String
slackToken = "xoxb-00-00-EE"
