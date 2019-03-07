{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MockMonad where

import qualified Data.Text                     as T
import           Control.Monad.State
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure
import qualified Bot.BotMonad                  as BM
import qualified Bot.BotClass                  as BC
import           Config
import           Slack.EchoBot
import           Telegram.EchoBot
import           Bot.EchoBot
import           Bot.Exception
import           MockResponses
import           MockConfig
import qualified Network.HTTP.Conduit          as HTTP
import           Data.Maybe

data MockIO = MockIO {
  mockLog :: [MockLog],
  mockDelayCounter :: Integer,
  mockSlackEnv :: BM.SlackEnv,
  mockSlackResponseStack :: SlackResponseStack,
  mockSlackRequestStack :: SlackRequestStack,
  mockTelegramEnv :: BM.TelegramEnv,
  mockTelegramResponseStack :: TelegramResponseStack,
  mockTelegramRequestStack :: TelegramRequestStack
} deriving Show

data MockLog = Debug T.Text | Info T.Text | Warn T.Text | Error T.Text
  deriving (Eq, Show)

newtype MockMonad a = MockMonad {runMockMonad :: StateT MockIO Catch a}
  deriving (Functor, Applicative, Monad, MonadState MockIO, MonadThrow, MonadCatch )

runMock :: MockIO -> MockMonad a -> Either SomeException MockIO
runMock env = runCatch . (`execStateT` env) . runMockMonad

instance BC.MonadDelay MockMonad where
  delay i = modify $ \s@MockIO {..} ->
    s { mockDelayCounter = mockDelayCounter + fromIntegral i }

instance BC.MonadLogger MockMonad where
  logDebug e = modify $ \s@MockIO {..} -> s { mockLog = Debug e : mockLog }
  logInfo e = modify $ \s@MockIO {..} -> s { mockLog = Info e : mockLog }
  logWarn e = modify $ \s@MockIO {..} -> s { mockLog = Warn e : mockLog }
  logError e = modify $ \s@MockIO {..} -> s { mockLog = Error e : mockLog }

instance BC.MonadHTTP MockMonad where
  http req = route req $ routes req

instance BC.HasSlackEnv MockMonad where
  sEnvToken     = BM.sToken <$> gets mockSlackEnv
  sEnvChannel   = BM.sChannel <$> gets mockSlackEnv
  sEnvHelpMsg   = BM.sHelpMsg <$> gets mockSlackEnv
  sEnvRepeatMsg = BM.sRepeatMsg <$> gets mockSlackEnv

instance BC.HasSlackMod MockMonad where
  sGetLastTimestamp   = BM.sLastTimestamp <$> gets mockSlackEnv
  sGetRepeatNumber    = BM.sRepeatNumber <$> gets mockSlackEnv
  sGetRepeatTimestamp = BM.sRepeatTimestamp <$> gets mockSlackEnv

  sPutLastTimestamp x = modify $ \s@MockIO {..} ->
    s { mockSlackEnv = mockSlackEnv { BM.sLastTimestamp = x } }
  sPutRepeatNumber x = modify $ \s@MockIO {..} ->
    s { mockSlackEnv = mockSlackEnv { BM.sRepeatNumber = x } }
  sPutRepeatTimestamp x = modify $ \s@MockIO {..} ->
    s { mockSlackEnv = mockSlackEnv { BM.sRepeatTimestamp = x } }

instance BC.HasTelegramEnv MockMonad where
  tEnvToken        = BM.tToken <$> gets mockTelegramEnv
  tEnvHelpMsg      = BM.tHelpMsg <$> gets mockTelegramEnv
  tEnvRepeatMsg    = BM.tRepeatMsg <$> gets mockTelegramEnv
  tEnvRepeatNumber = BM.tRepeatNumber <$> gets mockTelegramEnv

instance BC.HasTelegramMod MockMonad where
  tGetLastUpdateId = BM.tLastUpdateId <$> gets mockTelegramEnv
  tGetRepeatMap    = BM.tRepeatMap <$> gets mockTelegramEnv

  tPutLastUpdateId x = modify $ \s@MockIO {..} ->
    s { mockTelegramEnv = mockTelegramEnv { BM.tLastUpdateId = x } }
  tPutRepeatMap x = modify $ \s@MockIO {..} ->
    s { mockTelegramEnv = mockTelegramEnv { BM.tRepeatMap = x } }

mockIOInitial :: MockIO
mockIOInitial = MockIO { mockLog                   = []
                       , mockDelayCounter          = 0
                       , mockSlackEnv              = getSlackEnv
                       , mockSlackResponseStack    = emptySlackResponseStack
                       , mockSlackRequestStack     = emptySlackRequestStack
                       , mockTelegramEnv           = getTelegramEnv
                       , mockTelegramResponseStack = emptyTelegramResponseStack
                       , mockTelegramRequestStack  = emptyTelegramRequestStack
                       }

runTestSlack :: MonadThrow m => MockMonad () -> m SlackRequestStack
runTestSlack test = either throwM pure $ mockSlackRequestStack <$> resSlack
  where resSlack = runMock mockIOInitial $ catch test botExceptionHandler

testSlack :: SlackResponseStack -> MockMonad ()
testSlack resStack = do
  modify $ \s -> s { mockSlackResponseStack = resStack }
  botCycle slackBot

runTestTelegram :: MonadThrow m => MockMonad () -> m TelegramRequestStack
runTestTelegram test =
  either throwM pure $ mockTelegramRequestStack <$> resTelegram
  where resTelegram = runMock mockIOInitial $ catch test botExceptionHandler

testTelegram :: TelegramResponseStack -> MockMonad ()
testTelegram resStack = do
  modify $ \s -> s { mockTelegramResponseStack = resStack }
  botCycle telegramBot

route
  :: HTTP.Request -> [(Path, MockMonad ResponseLBS)] -> MockMonad ResponseLBS
route req rs = fromMaybe (pure notFoundResponse) response
 where
  path     = HTTP.path req
  response = lookup path rs

routes :: HTTP.Request -> [(Path, MockMonad ResponseLBS)]
routes req =
  [ ("/api/conversations.history"            , respondToConHistory req)
  , ("/api/reactions.get"                    , respondToGetReactions req)
  , ("/api/chat.postMessage"                 , respondToPostMessage req)
  , ("/bottelegram_token/getUpdates"         , respondToGetUpdates req)
  , ("/bottelegram_token/sendMessage"        , respondToSendMessage req)
  , ("/bottelegram_token/answerCallbackQuery", respondToAnswerCallbackQuery req)
  ]

respondToConHistory :: HTTP.Request -> MockMonad ResponseLBS
respondToConHistory req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _ _) -> s
    { mockSlackRequestStack = stack { conHistoryReq = req : conHistoryReq }
    }
  mbRes <- conHistoryRes <$> gets mockSlackResponseStack
  maybe throwTooManyRequests pure mbRes

respondToGetReactions :: HTTP.Request -> MockMonad ResponseLBS
respondToGetReactions req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _ _) -> s
    { mockSlackRequestStack = stack { getReactionsReq = req : getReactionsReq }
    }
  mbRes <- getReactionsRes <$> gets mockSlackResponseStack
  maybe throwTooManyRequests pure mbRes

throwTooManyRequests :: MockMonad ResponseLBS
throwTooManyRequests = throwM TooManyRequests >> pure tooManyRequestsResponse

respondToPostMessage :: HTTP.Request -> MockMonad ResponseLBS
respondToPostMessage req = do
  modify $ \s@(MockIO _ _ _ _ stack@SlackRequestStack {..} _ _ _) -> s
    { mockSlackRequestStack = stack { postMessageReq = req : postMessageReq }
    }
  rs <- postMessageRes <$> gets mockSlackResponseStack
  let mbRes = listToMaybe $ take 1 rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ stack@SlackResponseStack {..} _ _ _ _) ->
    s { mockSlackResponseStack = stack { postMessageRes = newRs } }
  maybe throwTooManyRequests pure mbRes

respondToGetUpdates :: HTTP.Request -> MockMonad ResponseLBS
respondToGetUpdates req = do
  modify $ \s@(MockIO _ _ _ _ _ _ _ stack@TelegramRequestStack {..}) -> s
    { mockTelegramRequestStack = stack { getUpdatesReq = req : getUpdatesReq }
    }
  mbRes <- getUpdatesRes <$> gets mockTelegramResponseStack
  maybe throwTooManyRequests pure mbRes

respondToSendMessage :: HTTP.Request -> MockMonad ResponseLBS
respondToSendMessage req = do
  modify $ \s@(MockIO _ _ _ _ _ _ _ stack@TelegramRequestStack {..}) -> s
    { mockTelegramRequestStack = stack { sendMessageReq = req : sendMessageReq }
    }
  rs <- sendMessageRes <$> gets mockTelegramResponseStack
  let mbRes = listToMaybe $ take 1 rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ _ _ _ stack@TelegramResponseStack {..} _) ->
    s { mockTelegramResponseStack = stack { sendMessageRes = newRs } }
  maybe throwTooManyRequests pure mbRes

respondToAnswerCallbackQuery :: HTTP.Request -> MockMonad ResponseLBS
respondToAnswerCallbackQuery req = do
  modify $ \s@(MockIO _ _ _ _ _ _ _ stack@TelegramRequestStack {..}) -> s
    { mockTelegramRequestStack = stack
                                   { answerCallbackQueryReq =
                                     req : answerCallbackQueryReq
                                   }
    }
  rs <- answerCallbackQueryRes <$> gets mockTelegramResponseStack
  let mbRes = listToMaybe $ take 1 rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ _ _ _ stack@TelegramResponseStack {..} _) ->
    s { mockTelegramResponseStack = stack { answerCallbackQueryRes = newRs } }
  maybe throwTooManyRequests pure mbRes
