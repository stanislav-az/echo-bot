{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MockMonad where

import qualified Bot.BotClass as Bot
import qualified Bot.BotMonad as Bot
import qualified Bot.EchoBot as Bot
import Control.Monad.Catch.Pure
import Control.Monad.State
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T (Text(..))
import MockConfig (getSlackEnv, getTelegramEnv)
import MockResponses
import qualified Network.HTTP.Conduit as HTTP (Request(..), path)
import qualified Slack.EchoBot as Bot
import qualified Slack.Models as Bot
import qualified Telegram.EchoBot as Bot
import qualified Telegram.Models as Bot

data MockIO e = MockIO
  { mockLog :: [MockLog]
  , mockDelayCounter :: Integer
  , mockBotEnv :: e
  , mockSlackResponseStack :: SlackResponseStack
  , mockSlackRequestStack :: SlackRequestStack
  , mockTelegramResponseStack :: TelegramResponseStack
  , mockTelegramRequestStack :: TelegramRequestStack
  } deriving (Show)

data MockLog
  = Debug T.Text
  | Info T.Text
  | Warn T.Text
  | Error T.Text
  deriving (Eq, Show)

newtype MockMonad e a = MockMonad
  { runMockMonad :: StateT (MockIO e) Catch a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState (MockIO e)
             , MonadThrow
             , MonadCatch
             )

runMock :: MockIO e -> MockMonad e a -> Either SomeException (MockIO e)
runMock env = runCatch . (`execStateT` env) . runMockMonad

instance Bot.MonadDelay (MockMonad e) where
  delay i =
    modify $ \s@MockIO {..} ->
      s {mockDelayCounter = mockDelayCounter + fromIntegral i}

instance Bot.MonadLogger (MockMonad e) where
  logDebug e = modify $ \s@MockIO {..} -> s {mockLog = Debug e : mockLog}
  logInfo e = modify $ \s@MockIO {..} -> s {mockLog = Info e : mockLog}
  logWarn e = modify $ \s@MockIO {..} -> s {mockLog = Warn e : mockLog}
  logError e = modify $ \s@MockIO {..} -> s {mockLog = Error e : mockLog}

instance Bot.MonadHTTP (MockMonad e) where
  http req = route req $ routes req

instance Bot.MonadSlackConst (MockMonad Bot.SlackEnv) where
  getSlackConst = Bot.sSlackConst <$> gets mockBotEnv

instance Bot.MonadTelegramConst (MockMonad Bot.TelegramEnv) where
  getTelegramConst = Bot.tTelegramConst <$> gets mockBotEnv

instance Bot.MonadBotConst (MockMonad Bot.SlackEnv) where
  getBotConst = Bot.sBotConst <$> gets mockBotEnv

instance Bot.MonadBotConst (MockMonad Bot.TelegramEnv) where
  getBotConst = Bot.tBotConst <$> gets mockBotEnv

instance Bot.MonadLastMsgState (MockMonad Bot.SlackEnv) Bot.SlackMessage where
  getLastMsg = Bot.sLastMsg <$> gets mockBotEnv
  putLastMsg lastMsg =
    modify $ \s@MockIO {..} ->
      s {mockBotEnv = mockBotEnv {Bot.sLastMsg = lastMsg}}

instance Bot.MonadLastMsgState (MockMonad Bot.TelegramEnv) Bot.TelegramMessage where
  getLastMsg = Bot.tLastMsg <$> gets mockBotEnv
  putLastMsg lastMsg =
    modify $ \s@MockIO {..} ->
      s {mockBotEnv = mockBotEnv {Bot.tLastMsg = lastMsg}}

instance Bot.MonadTimestampState (MockMonad Bot.SlackEnv) where
  getTimestamp = Bot.sTimestamp <$> gets mockBotEnv
  putTimestamp ts =
    modify $ \s@MockIO {..} -> s {mockBotEnv = mockBotEnv {Bot.sTimestamp = ts}}

instance Bot.MonadRepeatMapState (MockMonad Bot.SlackEnv) where
  getRepeatMap = Bot.sRepeatMap <$> gets mockBotEnv
  putRepeatMap repeatMap =
    modify $ \s@MockIO {..} ->
      s {mockBotEnv = mockBotEnv {Bot.sRepeatMap = repeatMap}}

instance Bot.MonadRepeatMapState (MockMonad Bot.TelegramEnv) where
  getRepeatMap = Bot.tRepeatMap <$> gets mockBotEnv
  putRepeatMap repeatMap =
    modify $ \s@MockIO {..} ->
      s {mockBotEnv = mockBotEnv {Bot.tRepeatMap = repeatMap}}

mockIOSlack :: MockIO Bot.SlackEnv
mockIOSlack =
  MockIO
    { mockLog = []
    , mockDelayCounter = 0
    , mockBotEnv = getSlackEnv
    , mockSlackResponseStack = emptySlackResponseStack
    , mockSlackRequestStack = emptySlackRequestStack
    , mockTelegramResponseStack = emptyTelegramResponseStack
    , mockTelegramRequestStack = emptyTelegramRequestStack
    }

runTestSlack :: MonadThrow m => MockMonad Bot.SlackEnv () -> m SlackRequestStack
runTestSlack test = either throwM pure $ mockSlackRequestStack <$> resSlack
  where
    resSlack = runMock mockIOSlack test

testSlack :: SlackResponseStack -> MockMonad Bot.SlackEnv ()
testSlack resStack = do
  modify $ \s -> s {mockSlackResponseStack = resStack}
  Bot.botCycle Bot.slackBot

mockIOTelegram :: MockIO Bot.TelegramEnv
mockIOTelegram =
  MockIO
    { mockLog = []
    , mockDelayCounter = 0
    , mockBotEnv = getTelegramEnv
    , mockSlackResponseStack = emptySlackResponseStack
    , mockSlackRequestStack = emptySlackRequestStack
    , mockTelegramResponseStack = emptyTelegramResponseStack
    , mockTelegramRequestStack = emptyTelegramRequestStack
    }

runTestTelegram ::
     MonadThrow m => MockMonad Bot.TelegramEnv () -> m TelegramRequestStack
runTestTelegram test =
  either throwM pure $ mockTelegramRequestStack <$> resTelegram
  where
    resTelegram = runMock mockIOTelegram test

testTelegram :: TelegramResponseStack -> MockMonad Bot.TelegramEnv ()
testTelegram resStack = do
  modify $ \s -> s {mockTelegramResponseStack = resStack}
  Bot.botCycle Bot.telegramBot

route ::
     HTTP.Request
  -> [(Path, MockMonad e ResponseLBS)]
  -> MockMonad e ResponseLBS
route req rs = fromMaybe (pure notFoundResponse) response
  where
    path = HTTP.path req
    response = lookup path rs

routes :: HTTP.Request -> [(Path, MockMonad e ResponseLBS)]
routes req =
  [ ("/api/conversations.history", respondToConHistory req)
  , ("/api/reactions.get", respondToGetReactions req)
  , ("/api/chat.postMessage", respondToPostMessage req)
  , ("/bottelegram_token/getUpdates", respondToGetUpdates req)
  , ("/bottelegram_token/sendMessage", respondToSendMessage req)
  , ("/bottelegram_token/answerCallbackQuery", respondToAnswerCallbackQuery req)
  ]

respondToConHistory :: HTTP.Request -> MockMonad e ResponseLBS
respondToConHistory req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _) ->
    s {mockSlackRequestStack = stack {conHistoryReq = req : conHistoryReq}}
  mbRes <- conHistoryRes <$> gets mockSlackResponseStack
  modify $ \s@(MockIO _ _ _ stack@SlackResponseStack {..} _ _ _) ->
    s {mockSlackResponseStack = stack {conHistoryRes = Nothing}}
  maybe throwTooManyRequests pure mbRes

respondToGetReactions :: HTTP.Request -> MockMonad e ResponseLBS
respondToGetReactions req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _) ->
    s {mockSlackRequestStack = stack {getReactionsReq = req : getReactionsReq}}
  mbRes <- getReactionsRes <$> gets mockSlackResponseStack
  modify $ \s@(MockIO _ _ _ stack@SlackResponseStack {..} _ _ _) ->
    s {mockSlackResponseStack = stack {getReactionsRes = Nothing}}
  maybe throwTooManyRequests pure mbRes

throwTooManyRequests :: MockMonad e b
throwTooManyRequests = throwM TooManyRequests

respondToPostMessage :: HTTP.Request -> MockMonad e ResponseLBS
respondToPostMessage req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _) ->
    s {mockSlackRequestStack = stack {postMessageReq = req : postMessageReq}}
  rs <- postMessageRes <$> gets mockSlackResponseStack
  let mbRes = listToMaybe rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ stack@SlackResponseStack {..} _ _ _) ->
    s {mockSlackResponseStack = stack {postMessageRes = newRs}}
  maybe throwTooManyRequests pure mbRes

respondToGetUpdates :: HTTP.Request -> MockMonad e ResponseLBS
respondToGetUpdates req = do
  modify $ \s@(MockIO _ _ _ _ _ _ stack@TelegramRequestStack {..}) ->
    s {mockTelegramRequestStack = stack {getUpdatesReq = req : getUpdatesReq}}
  mbRes <- getUpdatesRes <$> gets mockTelegramResponseStack
  modify $ \s@(MockIO _ _ _ _ _ stack@TelegramResponseStack {..} _) ->
    s {mockTelegramResponseStack = stack {getUpdatesRes = Nothing}}
  maybe throwTooManyRequests pure mbRes

respondToSendMessage :: HTTP.Request -> MockMonad e ResponseLBS
respondToSendMessage req = do
  modify $ \s@(MockIO _ _ _ _ _ _ stack@TelegramRequestStack {..}) ->
    s {mockTelegramRequestStack = stack {sendMessageReq = req : sendMessageReq}}
  rs <- sendMessageRes <$> gets mockTelegramResponseStack
  let mbRes = listToMaybe rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ _ _ stack@TelegramResponseStack {..} _) ->
    s {mockTelegramResponseStack = stack {sendMessageRes = newRs}}
  maybe throwTooManyRequests pure mbRes

respondToAnswerCallbackQuery :: HTTP.Request -> MockMonad e ResponseLBS
respondToAnswerCallbackQuery req = do
  modify $ \s@(MockIO _ _ _ _ _ _ stack@TelegramRequestStack {..}) ->
    s
      { mockTelegramRequestStack =
          stack {answerCallbackQueryReq = req : answerCallbackQueryReq}
      }
  rs <- answerCallbackQueryRes <$> gets mockTelegramResponseStack
  let mbRes = listToMaybe rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ _ _ stack@TelegramResponseStack {..} _) ->
    s {mockTelegramResponseStack = stack {answerCallbackQueryRes = newRs}}
  maybe throwTooManyRequests pure mbRes
