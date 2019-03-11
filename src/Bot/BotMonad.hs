{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.BotMonad where

import Bot.BotClass
import Control.Monad.Catch
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (Text(..))
import Slack.Models
import Telegram.Models

data TelegramEnv = TelegramEnv
  { tTelegramConst :: TelegramConst
  , tLastUpdateId :: Maybe Integer
  , tRepeatMap :: HM.HashMap Integer Int
  , tHelpMsg :: T.Text
  , tRepeatMsg :: T.Text
  , tDefaultRepeatNumber :: Int
  } deriving (Eq, Show)

data SlackEnv = SlackEnv
  { sSlackConst :: SlackConst
  , sLastTimestamp :: Maybe String
  , sRepeatMap :: Maybe Int
  , sRepeatTimestamp :: Maybe String
  , sHelpMsg :: T.Text
  , sRepeatMsg :: T.Text
  , sDefaultRepeatNumber :: Int
  } deriving (Eq, Show)

data BotException
  = NoParse ResponseBody
  | BadCallbackData CallbackData
  | ResponseException String
  deriving (Eq, Show)

type ResponseBody = String

type CallbackData = String

instance Exception BotException

newtype BotMonad env a = BotMonad
  { runBotMonad :: StateT env IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState env
             , MonadThrow
             , MonadCatch
             )

type TelegramMonad a = BotMonad TelegramEnv a

type SlackMonad a = BotMonad SlackEnv a

runBot :: s -> BotMonad s a -> IO a
runBot env = (`evalStateT` env) . runBotMonad

instance MonadLogger (BotMonad e) where
  logDebug = liftIO . logDebug
  logInfo = liftIO . logInfo
  logWarn = liftIO . logWarn
  logError = liftIO . logError

instance MonadDelay (BotMonad e) where
  delay = liftIO . delay

instance MonadHTTP (BotMonad e) where
  http = liftIO . http

instance HasSlackConst (BotMonad SlackEnv) where
  getSlackConst = gets sSlackConst

instance HasTelegramConst (BotMonad TelegramEnv) where
  getTelegramConst = gets tTelegramConst

instance HasBotConst (BotMonad SlackEnv) where
  getBotConst =
    BotConst <$> gets sHelpMsg <*> gets sRepeatMsg <*> gets sDefaultRepeatNumber

instance HasBotConst (BotMonad TelegramEnv) where
  getBotConst =
    BotConst <$> gets tHelpMsg <*> gets tRepeatMsg <*> gets tDefaultRepeatNumber

instance MonadFlagState (BotMonad SlackEnv) SlackFlag where
  getFlag = gets sRepeatTimestamp
  putFlag flag = modify $ \s -> s {sRepeatTimestamp = flag}

instance MonadFlagState (BotMonad TelegramEnv) TelegramFlag where
  getFlag = pure Nothing
  putFlag _ = pure ()

instance MonadIterState (BotMonad SlackEnv) SlackIterator where
  getIterator = gets sLastTimestamp
  putIterator ts = modify $ \s -> s {sLastTimestamp = ts}

instance MonadIterState (BotMonad TelegramEnv) TelegramIterator where
  getIterator = gets tLastUpdateId
  putIterator uid = modify $ \s -> s {tLastUpdateId = uid}

instance MonadRepeatState (BotMonad SlackEnv) SlackRepeatMap where
  getRepeatMap = gets sRepeatMap
  putRepeatMap repeatMap = modify $ \s -> s {sRepeatMap = repeatMap}

instance MonadRepeatState (BotMonad TelegramEnv) TelegramRepeatMap where
  getRepeatMap = gets tRepeatMap
  putRepeatMap repeatMap = modify $ \s -> s {tRepeatMap = repeatMap}
