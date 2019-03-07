{-# LANGUAGE OverloadedStrings #-}
module SlackBotSpec
  ( spec
  )
where

import           Test.Hspec                     ( Spec(..)
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Serializer.Slack
import           Slack.Models
import           Data.Aeson
import           MockResponses
import           MockMonad
import           RequestBody

spec :: Spec
spec = do
  describe "Get history capability" $ do
    it "Should work with empty history list" $ do
      let getH  = makeOkResWithBody $ encode emptySResponse
          stack = SlackResponseStack (Just getH) Nothing []
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should work with typical history list" $ do
      let getH    = makeOkResWithBody $ encode $ putMsgInSResponse [msg1]
          postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg1
          stack   = SlackResponseStack (Just getH) Nothing [postMsg]
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should work with message with reactions list" $ do
      let getH    = makeOkResWithBody $ encode $ putMsgInSResponse [msg1, msg2]
          postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg1
          stack   = SlackResponseStack (Just getH) Nothing $ replicate 2 postMsg
      res <- runTestSlack $ testSlack stack
      countConHistoryReqs res `shouldBe` 1
    it "Should get history each cycle" $ do
      let
        getH1 = makeOkResWithBody $ encode $ putMsgInSResponse [msg1, msg2]
        getH2 =
          makeOkResWithBody $ encode $ putMsgInSResponse [msg1, msg2, msg1]
        postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg1
        stack1  = SlackResponseStack (Just getH1) Nothing $ replicate 2 postMsg
        stack2  = SlackResponseStack (Just getH2) Nothing $ replicate 3 postMsg
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countConHistoryReqs res `shouldBe` 2

  describe "Get reactions capability" $ do
    it "Should not send requests with no repeat timestamp" $ do
      let getH  = makeOkResWithBody $ encode emptySResponse
          stack = SlackResponseStack (Just getH) Nothing []
      res <- runTestSlack $ testSlack stack
      countGetReactionsReqs res `shouldBe` 0
    it "Should send request if _repeat message was sent" $ do
      let getH1   = makeOkResWithBody $ encode $ putMsgInSResponse [msg3]
          getH2   = makeOkResWithBody $ encode emptySResponse
          getR    = makeOkResWithBody $ encode $ putMsgInSPostResponse msg3
          postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg3
          stack1  = SlackResponseStack (Just getH1) Nothing [postMsg]
          stack2  = SlackResponseStack (Just getH2) (Just getR) []
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countGetReactionsReqs res `shouldBe` 1
    it
        "Should stop sending requests if received some (even not parsable) reactions"
      $ do
          let getH1   = makeOkResWithBody $ encode $ putMsgInSResponse [msg3]
              getH2   = makeOkResWithBody $ encode emptySResponse
              getR    = makeOkResWithBody $ encode $ putMsgInSPostResponse msg2
              postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg3
              stack1  = SlackResponseStack (Just getH1) Nothing [postMsg]
              stack2  = SlackResponseStack (Just getH2) (Just getR) []
              stack3  = SlackResponseStack (Just getH2) Nothing []
          res <-
            runTestSlack
            $  testSlack stack1
            >> testSlack stack2
            >> testSlack stack3
          countGetReactionsReqs res `shouldBe` 1
    it "Should continue sending requests if not received any reactions" $ do
      let getH1   = makeOkResWithBody $ encode $ putMsgInSResponse [msg3]
          getH2   = makeOkResWithBody $ encode emptySResponse
          getR    = makeOkResWithBody $ encode $ putMsgInSPostResponse msg3
          postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg3
          stack1  = SlackResponseStack (Just getH1) Nothing [postMsg]
          stack2  = SlackResponseStack (Just getH2) (Just getR) []
          stack3  = SlackResponseStack (Just getH2) (Just getR) []
      res <-
        runTestSlack $ testSlack stack1 >> testSlack stack2 >> testSlack stack3
      countGetReactionsReqs res `shouldBe` 2

  describe "Send messages capability" $ do
    it "Should not send anything with empty updates list" $ do
      let getH  = makeOkResWithBody $ encode emptySResponse
          stack = SlackResponseStack (Just getH) Nothing []
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 0
    it "Should send messages back with the same text" $ do
      let getH    = makeOkResWithBody $ encode $ putMsgInSResponse [msg1]
          postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg1
          stack   = SlackResponseStack (Just getH) Nothing [postMsg]
          body    = encode $ sMessageToPostMessage slackMsg1 "slack_channel"
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 1
      (take 1 $ getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it "Should send specific help message on _help command" $ do
      let getH    = makeOkResWithBody $ encode $ putMsgInSResponse [msg4]
          postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg4
          stack   = SlackResponseStack (Just getH) Nothing [postMsg]
          body    = encode $ sMessageToPostMessage slackMsg2 "slack_channel"
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 1
      (take 1 $ getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it
        "Should send specific repeat message on _repeat command and append current repeat number to it"
      $ do
          let getH    = makeOkResWithBody $ encode $ putMsgInSResponse [msg3]
              postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg4
              stack   = SlackResponseStack (Just getH) Nothing [postMsg]
              body    = encode $ sMessageToPostMessage slackMsg3 "slack_channel"
          res <- runTestSlack $ testSlack stack
          countPostMessageReqs res `shouldBe` 1
          (take 1 $ getReqBodyLBS <$> postMessageReq res) `shouldBe` [body]
    it "Should respond with messages in correct order" $ do
      let getH    = makeOkResWithBody $ encode $ putMsgInSResponse [msg1, msg2]
          postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg1
          stack   = SlackResponseStack (Just getH) Nothing $ replicate 2 postMsg
          body1   = encode $ sMessageToPostMessage slackMsg1 "slack_channel"
          body2   = encode $ sMessageToPostMessage slackMsg4 "slack_channel"
      res <- runTestSlack $ testSlack stack
      countPostMessageReqs res `shouldBe` 2
      (take 2 $ getReqBodyLBS <$> postMessageReq res) `shouldBe` [body1, body2]
    it "Should respond with messages each cycle when there are updates" $ do
      let
        getH1 = makeOkResWithBody $ encode $ putMsgInSResponse [msg1, msg2]
        getH2 =
          makeOkResWithBody $ encode $ putMsgInSResponse [msg1, msg2, msg1]
        postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg1
        stack1  = SlackResponseStack (Just getH1) Nothing $ replicate 2 postMsg
        stack2  = SlackResponseStack (Just getH2) Nothing $ replicate 3 postMsg
      res <- runTestSlack $ testSlack stack1 >> testSlack stack2
      countPostMessageReqs res `shouldBe` 5
    it "Should repeat messages a choosen number of times" $ do
      let
        getH0   = makeOkResWithBody $ encode $ putMsgInSResponse [msg1]
        getH1   = makeOkResWithBody $ encode $ putMsgInSResponse [msg3]
        getH2   = makeOkResWithBody $ encode emptySResponse
        getH3   = makeOkResWithBody $ encode $ putMsgInSResponse [msg2]
        getR    = makeOkResWithBody $ encode $ putMsgInSPostResponse msg5
        postMsg = makeOkResWithBody $ encode $ putMsgInSPostResponse msg3
        stack0  = SlackResponseStack (Just getH0) Nothing [postMsg]
        stack1  = SlackResponseStack (Just getH1) Nothing [postMsg]
        stack2  = SlackResponseStack (Just getH2) (Just getR) []
        stack3  = SlackResponseStack (Just getH3) Nothing $ replicate 3 postMsg
        body1   = encode $ sMessageToPostMessage slackMsg1 "slack_channel"
        body2   = encode $ sMessageToPostMessage slackMsg3 "slack_channel"
        body3   = encode $ sMessageToPostMessage slackMsg4 "slack_channel"
      res <-
        runTestSlack
        $  testSlack stack0
        >> testSlack stack1
        >> testSlack stack2
        >> testSlack stack3
      countPostMessageReqs res `shouldBe` 5
      (take 5 $ getReqBodyLBS <$> postMessageReq res)
        `shouldBe` [body3, body3, body3, body2, body1]

putMsgInSResponse :: [SMessage] -> SResponse
putMsgInSResponse = SResponse True . Just

putMsgInSPostResponse :: SMessage -> SPostResponse
putMsgInSPostResponse = SPostResponse True . Just

msg1 :: SMessage
msg1 = SMessage { sMessageUser      = Just "I am user number 1!"
                , sMessageText      = "Hello, this is captain speaking"
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
msg2 = SMessage { sMessageUser      = Just "I am user number 2!"
                , sMessageText      = "Hello again, this is captain speaking"
                , sMessageTimestamp = "0002"
                , sMessageReactions = Just [SReaction "WOW"]
                }

msg3 :: SMessage
msg3 = SMessage { sMessageUser      = Just "I am user number 3!"
                , sMessageText      = "_repeat"
                , sMessageTimestamp = "0003"
                , sMessageReactions = Nothing
                }

msg4 :: SMessage
msg4 = SMessage { sMessageUser      = Just "I am user number 4!"
                , sMessageText      = "_help"
                , sMessageTimestamp = "0004"
                , sMessageReactions = Nothing
                }

msg5 :: SMessage
msg5 = SMessage { sMessageUser      = Just "I am user number 5!"
                , sMessageText      = "Hello again, this is captain speaking"
                , sMessageTimestamp = "0005"
                , sMessageReactions = Just [SReaction "three"]
                }
