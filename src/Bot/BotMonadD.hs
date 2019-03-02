{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.BotMonadD where

import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           Bot.BotClass

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

data BotError = NoParse ResponseBody | BadCallbackData CallbackData | ResponseError String
  deriving (Eq, Show)

type ResponseBody = String
type CallbackData = String

newtype BotMonad env a = BotMonad {runBotMonad :: StateT env (ExceptT BotError IO) a}
  deriving ( Functor, Applicative, Monad, MonadIO, MonadState env, MonadError BotError )

type TelegramMonad a = BotMonad TelegramEnv a

type SlackMonad a = BotMonad SlackEnv a

runTelegramBot :: TelegramEnv -> TelegramMonad a -> IO (Either BotError a)
runTelegramBot env = runExceptT . (`evalStateT` env) . runBotMonad

runSlackBot :: SlackEnv -> SlackMonad a -> IO (Either BotError a)
runSlackBot env = runExceptT . (`evalStateT` env) . runBotMonad

instance MonadLogger (BotMonad e) where
  logDebug = liftIO . logDebug
  logInfo  = liftIO . logInfo
  logWarn  = liftIO . logWarn
  logError = liftIO . logError
