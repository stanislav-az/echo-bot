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
  mockTelegramEnv :: BM.TelegramEnv,
  mockSlackResponseStack :: SlackResponseStack,
  mockSlackRequestStack :: SlackRequestStack
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

runTestSlack :: MonadThrow m => MockMonad () -> m SlackRequestStack
runTestSlack test = either throwM pure $ mockSlackRequestStack <$> resSlack
 where
  env = MockIO [] 0 getSlackEnv getTelegramEnv emptySlackResponseStack emptySlackRequestStack
  resSlack = runMock env $ catch test botExceptionHandler

testSlack :: SlackResponseStack -> MockMonad ()
testSlack resStack = do
  modify $ \s -> s {mockSlackResponseStack = resStack }
  botCycle slackBot

route
  :: HTTP.Request -> [(Path, MockMonad ResponseLBS)] -> MockMonad ResponseLBS
route req rs = fromMaybe (pure notFoundResponse) response
 where
  path     = HTTP.path req
  response = lookup path rs

routes :: HTTP.Request -> [(Path, MockMonad ResponseLBS)]
routes req =
  [ ("/api/conversations.history", respondToConHistory req)
  , ("/api/reactions.get"        , respondToGetReactions req)
  , ("/api/chat.postMessage"     , respondToPostMessage req)
  ]

respondToConHistory :: HTTP.Request -> MockMonad ResponseLBS
respondToConHistory req = do
  modify $ \s@(MockIO _ _ _ _ _ stack@SlackRequestStack {..}) -> s
    { mockSlackRequestStack = stack { conHistoryReq = req : conHistoryReq }
    }
  mbRes <- conHistoryRes <$> gets mockSlackResponseStack
  maybe throwTooManyRequests pure mbRes

respondToGetReactions :: HTTP.Request -> MockMonad ResponseLBS
respondToGetReactions req = do
  modify $ \s@(MockIO _ _ _ _ _ stack@SlackRequestStack {..}) -> s
    { mockSlackRequestStack = stack { getReactionsReq = req : getReactionsReq }
    }
  mbRes <- getReactionsRes <$> gets mockSlackResponseStack
  maybe throwTooManyRequests pure mbRes

throwTooManyRequests :: MockMonad ResponseLBS
throwTooManyRequests = throwM TooManyRequests >> pure tooManyRequestsResponse

respondToPostMessage :: HTTP.Request -> MockMonad ResponseLBS
respondToPostMessage req = do
  modify $ \s@(MockIO _ _ _ _ _ stack@SlackRequestStack {..}) -> s
    { mockSlackRequestStack = stack { postMessageReq = req : postMessageReq }
    }
  rs <- postMessageRes <$> gets mockSlackResponseStack
  let mbRes = listToMaybe $ take 1 rs
      newRs = drop 1 rs
  modify $ \s@(MockIO _ _ _ _ stack@SlackResponseStack {..} _) ->
    s { mockSlackResponseStack = stack { postMessageRes = newRs } }
  maybe throwTooManyRequests pure mbRes
