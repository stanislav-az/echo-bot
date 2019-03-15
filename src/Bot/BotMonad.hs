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
  { tBotConst :: BotConst
  , tTelegramConst :: TelegramConst
  , tLastMsg :: Maybe TelegramMessage
  , tRepeatMap :: HM.HashMap Integer Int
  } deriving (Eq, Show)

data SlackEnv = SlackEnv
  { sBotConst :: BotConst
  , sSlackConst :: SlackConst
  , sLastMsg :: Maybe SlackMessage
  , sFutureMsg :: Maybe SlackMessage
  , sRepeatMap :: Maybe Int
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

instance MonadSlackConst (BotMonad SlackEnv) where
  getSlackConst = gets sSlackConst

instance MonadTelegramConst (BotMonad TelegramEnv) where
  getTelegramConst = gets tTelegramConst

instance MonadBotConst (BotMonad SlackEnv) where
  getBotConst = gets sBotConst

instance MonadBotConst (BotMonad TelegramEnv) where
  getBotConst = gets tBotConst

instance MonadLastMsgState (BotMonad SlackEnv) SlackMessage where
  getLastMsg = gets sLastMsg
  putLastMsg lastMsg = modify $ \s -> s {sLastMsg = lastMsg}

instance MonadLastMsgState (BotMonad TelegramEnv) TelegramMessage where
  getLastMsg = gets tLastMsg
  putLastMsg lastMsg = modify $ \s -> s {tLastMsg = lastMsg}

instance MonadFutureMsgState (BotMonad SlackEnv) SlackMessage where
  getFutureMsg = gets sFutureMsg
  putFutureMsg futureMsg = modify $ \s -> s {sFutureMsg = futureMsg}

instance MonadFutureMsgState (BotMonad TelegramEnv) TelegramMessage where
  getFutureMsg = pure Nothing
  putFutureMsg _ = pure ()

instance MonadRepeatMapState (BotMonad SlackEnv) SlackRepeatMap where
  getRepeatMap = gets sRepeatMap
  putRepeatMap repeatMap = modify $ \s -> s {sRepeatMap = repeatMap}

instance MonadRepeatMapState (BotMonad TelegramEnv) TelegramRepeatMap where
  getRepeatMap = gets tRepeatMap
  putRepeatMap repeatMap = modify $ \s -> s {tRepeatMap = repeatMap}
