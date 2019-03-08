{-# LANGUAGE OverloadedStrings #-}

module SlackBotSpec
  ( spec
  ) where

import qualified Data.Aeson as JSON (encode)
import MockMonad (MockIO(..), runTestSlack, testSlack)
import MockResponses
import RequestBody (getReqBodyLBS)
import Serializer.Slack
import Slack.Models
import Test.Hspec (Spec(..), describe, it, shouldBe)

getHistory1 = makeOkResWithBody $ JSON.encode emptySResponse

getHistory2 = makeOkResWithBody $ JSON.encode $ putMsgInSResponse [msg1]

getHistory3 = makeOkResWithBody $ JSON.encode $ putMsgInSResponse [msg1, msg2]

getHistory4 =
  makeOkResWithBody $ JSON.encode $ putMsgInSResponse [msg1, msg2, msg1]

getHistory5 = makeOkResWithBody $ JSON.encode $ putMsgInSResponse [msg3]

spec :: Spec
spec = do
  describe "Get history capability" $ do
    it "Should work with empty history list" $ do
      let stack = SlackResponseStack (Just getHistory1) Nothing []
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should work with typical history list" $ do
      let postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg1
          stack = SlackResponseStack (Just getHistory2) Nothing [postMsg]
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should work with message with reactions list" $ do
      let postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg1
          stack =
            SlackResponseStack (Just getHistory3) Nothing $ replicate 2 postMsg
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should get history each cycle" $ do
      let postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg1
          stack1 =
            SlackResponseStack (Just getHistory3) Nothing $ replicate 2 postMsg
          stack2 =
            SlackResponseStack (Just getHistory4) Nothing $ replicate 3 postMsg
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countConHistoryReqs res `shouldBe` 2
  describe "Get reactions capability" $ do
    it "Should not send requests with no repeat timestamp" $ do
      let stack = SlackResponseStack (Just getHistory1) Nothing []
      res <- runTestSlack $ testSlack stack
      countGetReactionsReqs res `shouldBe` 0
    it "Should send request if _repeat message was sent" $ do
      let getR = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg3
          postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg3
          stack1 = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          stack2 = SlackResponseStack (Just getHistory1) (Just getR) []
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countGetReactionsReqs res `shouldBe` 1
    it
      "Should stop sending requests if received some (even not parsable) reactions" $ do
      let getR = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg2
          postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg3
          stack1 = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          stack2 = SlackResponseStack (Just getHistory1) (Just getR) []
          stack3 = SlackResponseStack (Just getHistory1) Nothing []
      res <-
        runTestSlack $ testSlack stack1 >> testSlack stack2 >> testSlack stack3
      countGetReactionsReqs res `shouldBe` 1
    it "Should continue sending requests if not received any reactions" $ do
      let getR = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg3
          postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg3
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
      let postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg1
          stack = SlackResponseStack (Just getHistory2) Nothing [postMsg]
          body = JSON.encode $ sMessageToPostMessage slackMsg1 "slack_channel"
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 1
      (take 1 $ getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it "Should send specific help message on _help command" $ do
      let getH = makeOkResWithBody $ JSON.encode $ putMsgInSResponse [msg4]
          postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg4
          stack = SlackResponseStack (Just getH) Nothing [postMsg]
          body = JSON.encode $ sMessageToPostMessage slackMsg2 "slack_channel"
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 1
      (take 1 $ getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it
      "Should send specific repeat message on _repeat command and append current repeat number to it" $ do
      let postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg4
          stack = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          body = JSON.encode $ sMessageToPostMessage slackMsg3 "slack_channel"
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 1
      (take 1 $ getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it "Should respond with messages in correct order" $ do
      let postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg1
          stack =
            SlackResponseStack (Just getHistory3) Nothing $ replicate 2 postMsg
          body1 = JSON.encode $ sMessageToPostMessage slackMsg1 "slack_channel"
          body2 = JSON.encode $ sMessageToPostMessage slackMsg4 "slack_channel"
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 2
      (take 2 $ getReqBodyLBS <$> postMessageReq res) `shouldBe` [body1, body2]
    it "Should respond with messages each cycle when there are updates" $ do
      let postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg1
          stack1 =
            SlackResponseStack (Just getHistory3) Nothing $ replicate 2 postMsg
          stack2 =
            SlackResponseStack (Just getHistory4) Nothing $ replicate 3 postMsg
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countPostMessageReqs res `shouldBe` 5
    it "Should repeat messages a choosen number of times" $ do
      let getH = makeOkResWithBody $ JSON.encode $ putMsgInSResponse [msg2]
          getR = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg5
          postMsg = makeOkResWithBody $ JSON.encode $ putMsgInSPostResponse msg3
          stack0 = SlackResponseStack (Just getHistory2) Nothing [postMsg]
          stack1 = SlackResponseStack (Just getHistory5) Nothing [postMsg]
          stack2 = SlackResponseStack (Just getHistory1) (Just getR) []
          stack3 = SlackResponseStack (Just getH) Nothing $ replicate 3 postMsg
          body1 = JSON.encode $ sMessageToPostMessage slackMsg1 "slack_channel"
          body2 = JSON.encode $ sMessageToPostMessage slackMsg3 "slack_channel"
          body3 = JSON.encode $ sMessageToPostMessage slackMsg4 "slack_channel"
      res <-
        runTestSlack $
        testSlack stack0 >> testSlack stack1 >> testSlack stack2 >>
        testSlack stack3
      countPostMessageReqs res `shouldBe` 5
      (take 5 $ getReqBodyLBS <$> postMessageReq res) `shouldBe`
        [body3, body3, body3, body2, body1]

putMsgInSResponse :: [SMessage] -> SResponse
putMsgInSResponse = SResponse True . Just

putMsgInSPostResponse :: SMessage -> SPostResponse
putMsgInSPostResponse = SPostResponse True . Just

msg1 :: SMessage
msg1 =
  SMessage
    { sMessageUser = Just "I am user number 1!"
    , sMessageText = "Hello, this is captain speaking"
    , sMessageTimestamp = "0001"
    , sMessageReactions = Nothing
    }

slackMsg1 :: SlackMessage
slackMsg1 = SlackMessage "0001" "Hello, this is captain speaking"

slackMsg2 :: SlackMessage
slackMsg2 = SlackMessage "0004" "slack_help_msg"

slackMsg3 :: SlackMessage
slackMsg3 = SlackMessage "0003" "slack_repeat_msg1"

slackMsg4 :: SlackMessage
slackMsg4 = SlackMessage "0002" "Hello again, this is captain speaking"

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
