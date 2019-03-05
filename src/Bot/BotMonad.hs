{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot.BotMonad where

import           Control.Monad.State
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           Bot.BotClass
import           Control.Monad.Catch
import           Slack.Models
import           Telegram.Models

data TelegramEnv = TelegramEnv {
  tLastUpdateId :: Maybe Integer,
  tToken :: String,
  tHelpMsg :: T.Text,
  tRepeatMsg :: T.Text,
  tRepeatNumber :: Int,
  tRepeatMap :: HM.HashMap Integer Int
} deriving (Eq, Show)

data SlackEnv = SlackEnv {
  sLastTimestamp :: Maybe String,
  sToken :: String,
  sChannel :: String,
  sHelpMsg :: T.Text,
  sRepeatMsg :: T.Text,
  sRepeatNumber :: Int,
  sRepeatTimestamp :: Maybe String
} deriving (Eq, Show)

data BotException = NoParse ResponseBody | BadCallbackData CallbackData | ResponseException String
  deriving (Eq, Show)

type ResponseBody = String
type CallbackData = String

instance Exception BotException

newtype BotMonad env a = BotMonad {runBotMonad :: StateT env IO a}
  deriving ( Functor, Applicative, Monad, MonadIO, MonadState env, MonadThrow, MonadCatch )

type TelegramMonad a = BotMonad TelegramEnv a
type SlackMonad a = BotMonad SlackEnv a

runBot :: s -> BotMonad s a -> IO a
runBot env = (`evalStateT` env) . runBotMonad

instance MonadLogger (BotMonad e) where
  logDebug = liftIO . logDebug
  logInfo  = liftIO . logInfo
  logWarn  = liftIO . logWarn
  logError = liftIO . logError

instance MonadDelay (BotMonad e) where
  delay = liftIO . delay

instance MonadHTTP (BotMonad e) where
  http = liftIO . http

instance HasSlackEnv (BotMonad SlackEnv) where
  sEnvToken = gets sToken
  sEnvChannel = gets sChannel
  sEnvHelpMsg = gets sHelpMsg
  sEnvRepeatMsg = gets sRepeatMsg

instance HasSlackMod (BotMonad SlackEnv) where
  sGetLastTimestamp = gets sLastTimestamp 
  sGetRepeatNumber = gets sRepeatNumber 
  sGetRepeatTimestamp = gets sRepeatTimestamp 

  sPutLastTimestamp x = modify $ \s -> s {sLastTimestamp = x}
  sPutRepeatNumber x = modify $ \s -> s {sRepeatNumber = x}
  sPutRepeatTimestamp x = modify $ \s -> s {sRepeatTimestamp = x}
