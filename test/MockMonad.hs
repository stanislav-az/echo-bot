{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Telegram.EchoBot as Bot

data MockIO = MockIO
  { mockLog :: [MockLog]
  , mockDelayCounter :: Integer
  , mockSlackEnv :: Bot.SlackEnv
  , mockSlackResponseStack :: SlackResponseStack
  , mockSlackRequestStack :: SlackRequestStack
  , mockTelegramEnv :: Bot.TelegramEnv
  , mockTelegramResponseStack :: TelegramResponseStack
  , mockTelegramRequestStack :: TelegramRequestStack
  } deriving (Show)

data MockLog
  = Debug T.Text
  | Info T.Text
  | Warn T.Text
  | Error T.Text
  deriving (Eq, Show)

newtype MockMonad a = MockMonad
  { runMockMonad :: StateT MockIO Catch a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState MockIO
             , MonadThrow
             , MonadCatch
             )

runMock :: MockIO -> MockMonad a -> Either SomeException MockIO
runMock env = runCatch . (`execStateT` env) . runMockMonad

instance Bot.MonadDelay MockMonad where
  delay i =
    modify $ \s@MockIO {..} ->
      s {mockDelayCounter = mockDelayCounter + fromIntegral i}

instance Bot.MonadLogger MockMonad where
  logDebug e = modify $ \s@MockIO {..} -> s {mockLog = Debug e : mockLog}
  logInfo e = modify $ \s@MockIO {..} -> s {mockLog = Info e : mockLog}
  logWarn e = modify $ \s@MockIO {..} -> s {mockLog = Warn e : mockLog}
  logError e = modify $ \s@MockIO {..} -> s {mockLog = Error e : mockLog}

instance Bot.MonadHTTP MockMonad where
  http req = route req $ routes req

instance Bot.HasSlackConst MockMonad where
  getSlackConst = Bot.sSlackConst <$> gets mockSlackEnv

instance Bot.HasSlackMod MockMonad where
  sGetLastTimestamp = Bot.sLastTimestamp <$> gets mockSlackEnv
  sGetRepeatNumber = Bot.sRepeatNumber <$> gets mockSlackEnv
  sGetRepeatTimestamp = Bot.sRepeatTimestamp <$> gets mockSlackEnv
  sPutLastTimestamp x =
    modify $ \s@MockIO {..} ->
      s {mockSlackEnv = mockSlackEnv {Bot.sLastTimestamp = x}}
  sPutRepeatNumber x =
    modify $ \s@MockIO {..} ->
      s {mockSlackEnv = mockSlackEnv {Bot.sRepeatNumber = x}}
  sPutRepeatTimestamp x =
    modify $ \s@MockIO {..} ->
      s {mockSlackEnv = mockSlackEnv {Bot.sRepeatTimestamp = x}}

instance Bot.HasTelegramConst MockMonad where
  getTelegramConst = Bot.tTelegramConst <$> gets mockTelegramEnv

instance Bot.HasTelegramMod MockMonad where
  tGetLastUpdateId = Bot.tLastUpdateId <$> gets mockTelegramEnv
  tGetRepeatMap = Bot.tRepeatMap <$> gets mockTelegramEnv
  tPutLastUpdateId x =
    modify $ \s@MockIO {..} ->
      s {mockTelegramEnv = mockTelegramEnv {Bot.tLastUpdateId = x}}
  tPutRepeatMap x =
    modify $ \s@MockIO {..} ->
      s {mockTelegramEnv = mockTelegramEnv {Bot.tRepeatMap = x}}

mockIOInitial :: MockIO
mockIOInitial =
  MockIO
    { mockLog = []
    , mockDelayCounter = 0
    , mockSlackEnv = getSlackEnv
    , mockSlackResponseStack = emptySlackResponseStack
    , mockSlackRequestStack = emptySlackRequestStack
    , mockTelegramEnv = getTelegramEnv
    , mockTelegramResponseStack = emptyTelegramResponseStack
    , mockTelegramRequestStack = emptyTelegramRequestStack
    }

runTestSlack :: MonadThrow m => MockMonad () -> m SlackRequestStack
runTestSlack test = either throwM pure $ mockSlackRequestStack <$> resSlack
  where
    resSlack = runMock mockIOInitial test

testSlack :: SlackResponseStack -> MockMonad ()
testSlack resStack = do
  modify $ \s -> s {mockSlackResponseStack = resStack}
  Bot.botCycle Bot.slackBot

runTestTelegram :: MonadThrow m => MockMonad () -> m TelegramRequestStack
runTestTelegram test =
  either throwM pure $ mockTelegramRequestStack <$> resTelegram
  where
    resTelegram = runMock mockIOInitial test

testTelegram :: TelegramResponseStack -> MockMonad ()
testTelegram resStack = do
  modify $ \s -> s {mockTelegramResponseStack = resStack}
  Bot.botCycle Bot.telegramBot

route ::
     HTTP.Request -> [(Path, MockMonad ResponseLBS)] -> MockMonad ResponseLBS
route req rs = fromMaybe (pure notFoundResponse) response
  where
    path = HTTP.path req
    response = lookup path rs

routes :: HTTP.Request -> [(Path, MockMonad ResponseLBS)]
routes req =
  [ ("/api/conversations.history", respondToConHistory req)
  , ("/api/reactions.get", respondToGetReactions req)
  , ("/api/chat.postMessage", respondToPostMessage req)
  , ("/bottelegram_token/getUpdates", respondToGetUpdates req)
  , ("/bottelegram_token/sendMessage", respondToSendMessage req)
  , ("/bottelegram_token/answerCallbackQuery", respondToAnswerCallbackQuery req)
  ]

respondToConHistory :: HTTP.Request -> MockMonad ResponseLBS
respondToConHistory req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _ _) ->
    s {mockSlackRequestStack = stack {conHistoryReq = req : conHistoryReq}}
  mbRes <- conHistoryRes <$> gets mockSlackResponseStack
  modify $ \s@(MockIO _ _ _ stack@SlackResponseStack {..} _ _ _ _) ->
    s {mockSlackResponseStack = stack {conHistoryRes = Nothing}}
  maybe throwTooManyRequests pure mbRes

respondToGetReactions :: HTTP.Request -> MockMonad ResponseLBS
respondToGetReactions req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _ _) ->
    s {mockSlackRequestStack = stack {getReactionsReq = req : getReactionsReq}}
  mbRes <- getReactionsRes <$> gets mockSlackResponseStack
  modify $ \s@(MockIO _ _ _ stack@SlackResponseStack {..} _ _ _ _) ->
    s {mockSlackResponseStack = stack {getReactionsRes = Nothing}}
  maybe throwTooManyRequests pure mbRes

throwTooManyRequests :: MockMonad ResponseLBS
throwTooManyRequests = throwM TooManyRequests >> pure tooManyRequestsResponse

respondToPostMessage :: HTTP.Request -> MockMonad ResponseLBS
respondToPostMessage req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _ _) ->
    s {mockSlackRequestStack = stack {postMessageReq = req : postMessageReq}}
  rs <- postMessageRes <$> gets mockSlackResponseStack
  let mbRes = listToMaybe rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ stack@SlackResponseStack {..} _ _ _ _) ->
    s {mockSlackResponseStack = stack {postMessageRes = newRs}}
  maybe throwTooManyRequests pure mbRes

respondToGetUpdates :: HTTP.Request -> MockMonad ResponseLBS
respondToGetUpdates req = do
  modify $ \s@(MockIO _ _ _ _ _ _ _ stack@TelegramRequestStack {..}) ->
    s {mockTelegramRequestStack = stack {getUpdatesReq = req : getUpdatesReq}}
  mbRes <- getUpdatesRes <$> gets mockTelegramResponseStack
  modify $ \s@(MockIO _ _ _ _ _ _ stack@TelegramResponseStack {..} _) ->
    s {mockTelegramResponseStack = stack {getUpdatesRes = Nothing}}
  maybe throwTooManyRequests pure mbRes

respondToSendMessage :: HTTP.Request -> MockMonad ResponseLBS
respondToSendMessage req = do
  modify $ \s@(MockIO _ _ _ _ _ _ _ stack@TelegramRequestStack {..}) ->
    s {mockTelegramRequestStack = stack {sendMessageReq = req : sendMessageReq}}
  rs <- sendMessageRes <$> gets mockTelegramResponseStack
  let mbRes = listToMaybe rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ _ _ _ stack@TelegramResponseStack {..} _) ->
    s {mockTelegramResponseStack = stack {sendMessageRes = newRs}}
  maybe throwTooManyRequests pure mbRes

respondToAnswerCallbackQuery :: HTTP.Request -> MockMonad ResponseLBS
respondToAnswerCallbackQuery req = do
  modify $ \s@(MockIO _ _ _ _ _ _ _ stack@TelegramRequestStack {..}) ->
    s
      { mockTelegramRequestStack =
          stack {answerCallbackQueryReq = req : answerCallbackQueryReq}
      }
  rs <- answerCallbackQueryRes <$> gets mockTelegramResponseStack
  let mbRes = listToMaybe rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ _ _ _ stack@TelegramResponseStack {..} _) ->
    s {mockTelegramResponseStack = stack {answerCallbackQueryRes = newRs}}
  maybe throwTooManyRequests pure mbRes
