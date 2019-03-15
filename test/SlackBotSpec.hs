{-# LANGUAGE OverloadedStrings #-}

module SlackBotSpec
  ( spec
  ) where

import qualified Data.Aeson as JSON (encode)
import qualified Data.Text as T (Text(..))
import MockMonad (MockIO(..), runTestSlack, testSlack)
import MockResponses
import RequestBody (getReqBodyLBS)
import Serializer.Slack
import Slack.Models
import Test.Hspec (Spec(..), describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Get history capability" $ do
    it "Should work with empty history list" $ do
      let stack = SlackResponseStack (Just getHistory1) Nothing []
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should work with typical history list" $ do
      let stack = SlackResponseStack (Just getHistory2) Nothing [ok]
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should work with message with reactions list" $ do
      let stack = SlackResponseStack (Just getHistory3) Nothing $ replicate 2 ok
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should get history each cycle" $ do
      let stack1 =
            SlackResponseStack (Just getHistory3) Nothing $ replicate 2 ok
          stack2 =
            SlackResponseStack (Just getHistory4) Nothing $ replicate 3 ok
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countConHistoryReqs res `shouldBe` 2
  describe "Get reactions capability" $ do
    it "Should not send requests with no repeat timestamp" $ do
      let stack = SlackResponseStack (Just getHistory1) Nothing []
      res <- runTestSlack $ testSlack stack
      countGetReactionsReqs res `shouldBe` 0
    it "Should send request if _repeat message was sent" $ do
      let getR = makeOkPostResWithMsg msg3
          postMsg = makeOkPostResWithMsg msg3
          stack1 = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          stack2 = SlackResponseStack (Just getHistory1) (Just getR) []
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countGetReactionsReqs res `shouldBe` 1
    it
      "Should stop sending requests if received some (even not parsable) reactions" $ do
      let getR = makeOkPostResWithMsg msg2
          postMsg = makeOkPostResWithMsg msg3
          stack1 = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          stack2 = SlackResponseStack (Just getHistory1) (Just getR) []
          stack3 = SlackResponseStack (Just getHistory1) Nothing []
      res <-
        runTestSlack $ testSlack stack1 >> testSlack stack2 >> testSlack stack3
      countGetReactionsReqs res `shouldBe` 1
    it "Should continue sending requests if not received any reactions" $ do
      let getR = makeOkPostResWithMsg msg3
          postMsg = makeOkPostResWithMsg msg3
          stack1 = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          stack2 = SlackResponseStack (Just getHistory1) (Just getR) []
          stack3 = SlackResponseStack (Just getHistory1) (Just getR) []
      res <-
        runTestSlack $ testSlack stack1 >> testSlack stack2 >> testSlack stack3
      countGetReactionsReqs res `shouldBe` 2
  describe "Send messages capability" $ do
    it "Should not send anything with empty updates list" $ do
      let stack = SlackResponseStack (Just getHistory1) Nothing []
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 0
    it "Should send messages back with the same text" $ do
      let stack = SlackResponseStack (Just getHistory2) Nothing [ok]
          body = JSON.encode slackMsg1
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 1
      (getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it "Should send specific help message on _help command" $ do
      let getH = makeOkResWithMsgs [msg4]
          stack = SlackResponseStack (Just getH) Nothing [ok]
          body = JSON.encode slackMsg2
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 1
      (getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it
      "Should send specific repeat message on _repeat command and append current repeat number to it" $ do
      let postMsg = makeOkPostResWithMsg msg3
          stack = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          body = JSON.encode slackMsg3
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 1
      (getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it "Should respond with messages in correct order" $ do
      let stack = SlackResponseStack (Just getHistory3) Nothing $ replicate 2 ok
          body1 = JSON.encode slackMsg1
          body2 = JSON.encode slackMsg4
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 2
      (getReqBodyLBS <$> postMessageReq res) `shouldBe` [body1, body2]
    it "Should respond with messages each cycle when there are updates" $ do
      let stack1 =
            SlackResponseStack (Just getHistory3) Nothing $ replicate 2 ok
          stack2 =
            SlackResponseStack (Just getHistory4) Nothing $ replicate 3 ok
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countPostMessageReqs res `shouldBe` 5
    it "Should repeat messages a choosen number of times" $ do
      let getH = makeOkResWithMsgs [msg2]
          getR = makeOkPostResWithMsg msg5
          postMsg = makeOkPostResWithMsg msg3
          stack0 = SlackResponseStack (Just getHistory2) Nothing [ok]
          stack1 = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          stack2 = SlackResponseStack (Just getHistory1) (Just getR) []
          stack3 = SlackResponseStack (Just getH) Nothing $ replicate 3 ok
          body1 = JSON.encode slackMsg1
          body2 = JSON.encode slackMsg3
          body3 = JSON.encode slackMsg4
      res <-
        runTestSlack $
        testSlack stack0 >> testSlack stack1 >> testSlack stack2 >>
        testSlack stack3
      countPostMessageReqs res `shouldBe` 5
      (getReqBodyLBS <$> postMessageReq res) `shouldBe`
        [body3, body3, body3, body2, body1]

emptySResponse :: SResponse
emptySResponse = SResponse {sResponseIsOk = True, sResponseMsgs = Just []}

putMsgsInSResponse :: [SMessage] -> SResponse
putMsgsInSResponse = SResponse True . Just

putTextInSPostMessage :: T.Text -> SPostMessage
putTextInSPostMessage = constructSPostMessage "slack_channel"

putMsgInSPostResponse :: SMessage -> SPostResponse
putMsgInSPostResponse = SPostResponse True . Just

makeOkResWithMsgs :: [SMessage] -> ResponseLBS
makeOkResWithMsgs = makeOkResWithBody . JSON.encode . putMsgsInSResponse

makeOkPostResWithMsg :: SMessage -> ResponseLBS
makeOkPostResWithMsg = makeOkResWithBody . JSON.encode . putMsgInSPostResponse

getHistory1 :: ResponseLBS
getHistory1 = makeOkResWithBody $ JSON.encode emptySResponse

getHistory2 :: ResponseLBS
getHistory2 = makeOkResWithMsgs [msg1]

getHistory3 :: ResponseLBS
getHistory3 = makeOkResWithMsgs [msg1, msg2]

getHistory4 :: ResponseLBS
getHistory4 = makeOkResWithMsgs [msg1, msg2, msg1]

getHistory5 :: ResponseLBS
getHistory5 = makeOkResWithMsgs [msg3]

msg1 :: SMessage
msg1 =
  SMessage
    { sMessageUser = Just "I am user number 1!"
    , sMessageText = "Hello, this is captain speaking"
    , sMessageTimestamp = "0001"
    , sMessageReactions = Nothing
    }

msg2 :: SMessage
msg2 =
  SMessage
    { sMessageUser = Just "I am user number 2!"
    , sMessageText = "Hello again, this is captain speaking"
    , sMessageTimestamp = "0002"
    , sMessageReactions = Just [SReaction "WOW"]
    }

msg3 :: SMessage
msg3 =
  SMessage
    { sMessageUser = Just "I am user number 3!"
    , sMessageText = "_repeat"
    , sMessageTimestamp = "0003"
    , sMessageReactions = Nothing
    }

msg4 :: SMessage
msg4 =
  SMessage
    { sMessageUser = Just "I am user number 4!"
    , sMessageText = "_help"
    , sMessageTimestamp = "0004"
    , sMessageReactions = Nothing
    }

msg5 :: SMessage
msg5 =
  SMessage
    { sMessageUser = Just "I am user number 5!"
    , sMessageText = "Hello again, this is captain speaking"
    , sMessageTimestamp = "0005"
    , sMessageReactions = Just [SReaction "three"]
    }

slackMsg1 :: SPostMessage
slackMsg1 = putTextInSPostMessage "Hello, this is captain speaking"

slackMsg2 :: SPostMessage
slackMsg2 = putTextInSPostMessage "slack_help_msg"

slackMsg3 :: SPostMessage
slackMsg3 = putTextInSPostMessage "slack_repeat_msg1"

slackMsg4 :: SPostMessage
slackMsg4 = putTextInSPostMessage "Hello again, this is captain speaking"
