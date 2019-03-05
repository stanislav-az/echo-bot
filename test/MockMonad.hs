{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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

data MockIO = MockIO {
  mockLog :: [MockLog],
  mockDelayCounter :: Integer,
  mockSlackEnv :: BM.SlackEnv,
  mockTelegramEnv :: BM.TelegramEnv
} deriving Show

data MockLog = Debug T.Text | Info T.Text | Warn T.Text | Error T.Text
  deriving (Eq, Show)

newtype MockMonad a = MockMonad {runMockMonad :: StateT MockIO Catch a}
  deriving (Functor, Applicative, Monad, MonadState MockIO, MonadThrow, MonadCatch )

runMock :: MockIO -> MockMonad a -> Either SomeException a
runMock env = runCatch . (`evalStateT` env) . runMockMonad

instance BC.MonadDelay MockMonad where
  delay i = modify $ \s@MockIO {..} ->
    s { mockDelayCounter = mockDelayCounter + fromIntegral i }

instance BC.MonadLogger MockMonad where
  logDebug e = modify $ \s@MockIO {..} -> s { mockLog = Debug e : mockLog }
  logInfo e = modify $ \s@MockIO {..} -> s { mockLog = Info e : mockLog }
  logWarn e = modify $ \s@MockIO {..} -> s { mockLog = Warn e : mockLog }
  logError e = modify $ \s@MockIO {..} -> s { mockLog = Error e : mockLog }

instance BC.MonadHTTP MockMonad where
  http req = pure $ route req routes

instance BC.HasSlackEnv MockMonad where
  sEnvToken     = BM.sToken <$> gets mockSlackEnv
  sEnvChannel   = BM.sChannel <$> gets mockSlackEnv
  sEnvHelpMsg   = BM.sHelpMsg <$> gets mockSlackEnv
  sEnvRepeatMsg = BM.sRepeatMsg <$> gets mockSlackEnv

instance BC.HasSlackMod MockMonad where
  sGetLastTimestamp   = BM.sLastTimestamp <$> gets mockSlackEnv
  sGetRepeatNumber    = BM.sRepeatNumber <$> gets mockSlackEnv
  sGetRepeatTimestamp = BM.sRepeatTimestamp <$> gets mockSlackEnv

  sPutLastTimestamp x = modify
    $ \s@(MockIO _ _ e _) -> s { mockSlackEnv = e { BM.sLastTimestamp = x } }
  sPutRepeatNumber x = modify
    $ \s@(MockIO _ _ e _) -> s { mockSlackEnv = e { BM.sRepeatNumber = x } }
  sPutRepeatTimestamp x = modify $ \s@(MockIO _ _ e _) ->
    s { mockSlackEnv = e { BM.sRepeatTimestamp = x } }

startSlackMock :: IO ()
startSlackMock = do
  tEnv <- makeTelegramEnv
  sEnv <- makeSlackEnv
  let env      = MockIO [] 0 sEnv tEnv
      resSlack = runMock env $ catches (goEchoBot slackBot) exceptionHandlers
  either throwM pure resSlack
