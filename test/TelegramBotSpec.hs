{-# LANGUAGE OverloadedStrings #-}

module TelegramBotSpec
  ( spec
  ) where

import qualified Bot.BotMonad as BM (BotException(..))
import qualified Data.Aeson as JSON (encode)
import MockMonad (MockIO(..), runTestTelegram, testTelegram)
import MockResponses
import RequestBody (getReqBodyLBS)
import Serializer.Telegram
import Telegram.Models
import Test.Hspec (Spec(..), describe, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  describe "Get updates capability" $ do
    it "Should work with empty updates list" $ do
      let getU = makeOkResWithBody $ JSON.encode emptyTResponse
          stack = TelegramResponseStack (Just getU) [] []
      res <- runTestTelegram $ testTelegram stack
      countGetUpdatesReqs res `shouldBe` 1
    it "Should work with typical updates list" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd1]
          stack = TelegramResponseStack (Just getU) [ok] []
      res <- runTestTelegram $ testTelegram stack
      countGetUpdatesReqs res `shouldBe` 1
    it "Should work with callback query in updates list" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd4]
          stack = TelegramResponseStack (Just getU) [] [ok]
      res <- runTestTelegram $ testTelegram stack
      countGetUpdatesReqs res `shouldBe` 1
    it "Should get updates each cycle" $ do
      let getU1 = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd1]
          getU2 =
            makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd7, upd8]
          stack1 = TelegramResponseStack (Just getU1) [ok] []
          stack2 = TelegramResponseStack (Just getU2) (replicate 2 ok) []
      res <- runTestTelegram $ testTelegram stack1 >> testTelegram stack2
      countGetUpdatesReqs res `shouldBe` 2
  describe "Send messages capability" $ do
    it "Should ignore empty updates list" $ do
      let getU = makeOkResWithBody $ JSON.encode emptyTResponse
          stack = TelegramResponseStack (Just getU) [] []
      res <- runTestTelegram $ testTelegram stack
      countSendMessageReqs res `shouldBe` 0
    it "Should ignore messages without text" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd2]
          stack = TelegramResponseStack (Just getU) [] []
      res <- runTestTelegram $ testTelegram stack
      countSendMessageReqs res `shouldBe` 0
    it "Should ignore empty updates" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd3]
          stack = TelegramResponseStack (Just getU) [] []
      res <- runTestTelegram $ testTelegram stack
      countSendMessageReqs res `shouldBe` 0
    it "Should send messages back with the same text" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd1]
          stack = TelegramResponseStack (Just getU) [ok] []
          body = JSON.encode $ tMessageToPostMessage telegramMsg1
      res <- runTestTelegram $ testTelegram stack
      countSendMessageReqs res `shouldBe` 1
      (take 1 $ getReqBodyLBS <$> sendMessageReq res) `shouldBe` [body]
    it "Should send specific help message on /help command" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd9]
          stack = TelegramResponseStack (Just getU) [ok] []
          body = JSON.encode $ tMessageToPostMessage telegramMsg9
      res <- runTestTelegram $ testTelegram stack
      countSendMessageReqs res `shouldBe` 1
      (take 1 $ getReqBodyLBS <$> sendMessageReq res) `shouldBe` [body]
    it
      "Should send specific repeat message on /repeat command and append current repeat number to it" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd10]
          stack = TelegramResponseStack (Just getU) [ok] []
          body = JSON.encode $ tMessageToPostRepeatMessage telegramMsg10
      res <- runTestTelegram $ testTelegram stack
      countSendMessageReqs res `shouldBe` 1
      (take 1 $ getReqBodyLBS <$> sendMessageReq res) `shouldBe` [body]
    it "Should respond with messages in correct order" $ do
      let getU =
            makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd7, upd8]
          stack = TelegramResponseStack (Just getU) (replicate 2 ok) []
          body1 = JSON.encode $ tMessageToPostMessage telegramMsg7
          body2 = JSON.encode $ tMessageToPostMessage telegramMsg8
      res <- runTestTelegram $ testTelegram stack
      countSendMessageReqs res `shouldBe` 2
      (take 2 $ getReqBodyLBS <$> sendMessageReq res) `shouldBe` [body2, body1]
    it "Should respond with messages each cycle when there are updates" $ do
      let getU1 = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd1]
          getU2 =
            makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd7, upd8]
          stack1 = TelegramResponseStack (Just getU1) [ok] []
          stack2 = TelegramResponseStack (Just getU2) (replicate 2 ok) []
          body1 = JSON.encode $ tMessageToPostMessage telegramMsg1
          body2 = JSON.encode $ tMessageToPostMessage telegramMsg7
          body3 = JSON.encode $ tMessageToPostMessage telegramMsg8
      res <- runTestTelegram $ testTelegram stack1 >> testTelegram stack2
      countSendMessageReqs res `shouldBe` 3
      (take 3 $ getReqBodyLBS <$> sendMessageReq res) `shouldBe`
        [body3, body2, body1]
    it "Should repeat messages a choosen number of times" $ do
      let getU1 = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd1]
          getU2 =
            makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd10]
          getU3 = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd4]
          getU4 = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd8]
          stack1 = TelegramResponseStack (Just getU1) [ok] []
          stack2 = TelegramResponseStack (Just getU2) [ok] []
          stack3 = TelegramResponseStack (Just getU3) [] [ok]
          stack4 = TelegramResponseStack (Just getU4) (replicate 3 ok) []
          body1 = JSON.encode $ tMessageToPostMessage telegramMsg1
          body2 = JSON.encode $ tMessageToPostRepeatMessage telegramMsg10
          body3 = JSON.encode $ tMessageToPostMessage telegramMsg8
      res <-
        runTestTelegram $
        testTelegram stack1 >> testTelegram stack2 >> testTelegram stack3 >>
        testTelegram stack4
      countGetUpdatesReqs res `shouldBe` 4
      countSendMessageReqs res `shouldBe` 5
      countAnswerCallbackQueryReqs res `shouldBe` 1
      (take 5 $ getReqBodyLBS <$> sendMessageReq res) `shouldBe`
        [body3, body3, body3, body2, body1]
  describe "Answer callbacks capability" $ do
    it "Should ignore empty updates list" $ do
      let getU = makeOkResWithBody $ JSON.encode emptyTResponse
          stack = TelegramResponseStack (Just getU) [] []
      res <- runTestTelegram $ testTelegram stack
      countAnswerCallbackQueryReqs res `shouldBe` 0
    it "Should work with typical callback query" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd4]
          stack = TelegramResponseStack (Just getU) [] [ok]
      res <- runTestTelegram $ testTelegram stack
      countAnswerCallbackQueryReqs res `shouldBe` 1
    it "Should ignore callbacks with no message" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd5]
          stack = TelegramResponseStack (Just getU) [] []
      res <- runTestTelegram $ testTelegram stack
      countAnswerCallbackQueryReqs res `shouldBe` 0
    it "Should throw exception on callback with random data" $ do
      let getU = makeOkResWithBody $ JSON.encode $ putUpdatesInTResponse [upd6]
          stack = TelegramResponseStack (Just getU) [] []
          res = runTestTelegram $ testTelegram stack
          badCallbackData (BM.BadCallbackData "random_data") = True
          badCallbackData _ = False
      res `shouldThrow` badCallbackData

putUpdatesInTResponse :: [TUpdate] -> TResponse
putUpdatesInTResponse = TResponse True

upd1 :: TUpdate
upd1 = TUpdate 1 msg1 Nothing
  where
    msg1 =
      Just
        TMessage
          { tMessageId = 1
          , tMessageChat = TChat 42
          , tMessageText = Just "Hello, this is captain speaking"
          }

telegramMsg1 :: TelegramMessage
telegramMsg1 =
  TelegramMessage
    { tmUpdateId = 1
    , tmChatId = 42
    , tmHasKeyboard = False
    , tmText = "Hello, this is captain speaking"
    }

upd2 :: TUpdate
upd2 = TUpdate 2 msg2 Nothing
  where
    msg2 =
      Just
        TMessage
          {tMessageId = 2, tMessageChat = TChat 42, tMessageText = Nothing}

upd3 :: TUpdate
upd3 = TUpdate 3 Nothing Nothing

upd4 :: TUpdate
upd4 = TUpdate 4 Nothing cb4
  where
    cb4 =
      Just
        TCallbackQuery
          { tCallbackQueryId = "004"
          , tCallbackQueryMessage = msg4
          , tCallbackQueryData = Just "3"
          }
    msg4 =
      Just
        TMessage
          {tMessageId = 4, tMessageChat = TChat 42, tMessageText = Nothing}

upd5 :: TUpdate
upd5 = TUpdate 5 Nothing cb5
  where
    cb5 =
      Just
        TCallbackQuery
          { tCallbackQueryId = "005"
          , tCallbackQueryMessage = Nothing
          , tCallbackQueryData = Just "3"
          }

upd6 :: TUpdate
upd6 = TUpdate 6 Nothing cb6
  where
    cb6 =
      Just
        TCallbackQuery
          { tCallbackQueryId = "006"
          , tCallbackQueryMessage = msg6
          , tCallbackQueryData = Just "random_data"
          }
    msg6 =
      Just
        TMessage
          {tMessageId = 6, tMessageChat = TChat 42, tMessageText = Nothing}

upd7 :: TUpdate
upd7 = TUpdate 7 msg7 Nothing
  where
    msg7 =
      Just
        TMessage
          { tMessageId = 7
          , tMessageChat = TChat 42
          , tMessageText = Just "Hello again, this is captain speaking"
          }

telegramMsg7 :: TelegramMessage
telegramMsg7 =
  TelegramMessage
    { tmUpdateId = 7
    , tmChatId = 42
    , tmHasKeyboard = False
    , tmText = "Hello again, this is captain speaking"
    }

upd8 :: TUpdate
upd8 = TUpdate 8 msg8 Nothing
  where
    msg8 =
      Just
        TMessage
          { tMessageId = 8
          , tMessageChat = TChat 42
          , tMessageText = Just "Вам письмо"
          }

telegramMsg8 :: TelegramMessage
telegramMsg8 =
  TelegramMessage
    { tmUpdateId = 8
    , tmChatId = 42
    , tmHasKeyboard = False
    , tmText = "Вам письмо"
    }

upd9 :: TUpdate
upd9 = TUpdate 9 msg9 Nothing
  where
    msg9 =
      Just
        TMessage
          {tMessageId = 9, tMessageChat = TChat 42, tMessageText = Just "/help"}

telegramMsg9 :: TelegramMessage
telegramMsg9 =
  TelegramMessage
    { tmUpdateId = 9
    , tmChatId = 42
    , tmHasKeyboard = False
    , tmText = "telegram_help_msg"
    }

upd10 :: TUpdate
upd10 = TUpdate 10 msg10 Nothing
  where
    msg10 =
      Just
        TMessage
          { tMessageId = 10
          , tMessageChat = TChat 42
          , tMessageText = Just "/repeat"
          }

telegramMsg10 :: TelegramMessage
telegramMsg10 =
  TelegramMessage
    { tmUpdateId = 10
    , tmChatId = 42
    , tmHasKeyboard = False
    , tmText = "telegram_repeat_msg1"
    }
