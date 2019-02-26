{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bot.BotMonad where

import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM

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

data BotError = NoParse ResponseBody | BadCallbackData CallbackData | ResponseError ResponseStatus
    deriving (Eq, Show)

type ResponseStatus = String
type ResponseBody = String
type CallbackData = String

newtype BotMonad env a = BotMonad {runBotMonad :: StateT env (ExceptT BotError IO) a}
  deriving ( Functor, Applicative, Monad, MonadIO, MonadState env, MonadError BotError )

type TelegramMonad a = BotMonad TelegramEnv a

type SlackMonad a = BotMonad SlackEnv a

runTelegramBot
  :: Maybe Integer
  -> String
  -> T.Text
  -> T.Text
  -> Int
  -> HM.HashMap Integer Int
  -> TelegramMonad a
  -> IO (Either BotError (a, TelegramEnv))
runTelegramBot t1 t2 t3 t4 t5 t6 = runExceptT . (`runStateT` env) . runBotMonad
  where env = TelegramEnv t1 t2 t3 t4 t5 t6

runSlackBot
  :: Maybe String
  -> String
  -> String
  -> T.Text
  -> T.Text
  -> Int
  -> Maybe String
  -> BotMonad SlackEnv a
  -> IO (Either BotError (a, SlackEnv))
runSlackBot t1 t2 t3 t4 t5 t6 t7 = runExceptT . (`runStateT` env) . runBotMonad
  where env = SlackEnv t1 t2 t3 t4 t5 t6 t7

