{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Bot.BotMonad where

import Bot.BotClass
import Control.Monad.Catch
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (Text(..))

data TelegramEnv = TelegramEnv
  { tTelegramConst :: TelegramConst
  , tLastUpdateId :: Maybe Integer
  , tRepeatMap :: HM.HashMap Integer Int
  } deriving (Eq, Show)

data SlackEnv = SlackEnv
  { sSlackConst :: SlackConst
  , sLastTimestamp :: Maybe String
  , sRepeatNumber :: Int
  , sRepeatTimestamp :: Maybe String
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

instance HasSlackMod (BotMonad SlackEnv) where
  sGetLastTimestamp = gets sLastTimestamp
  sGetRepeatNumber = gets sRepeatNumber
  sGetRepeatTimestamp = gets sRepeatTimestamp
  sPutLastTimestamp x = modify $ \s -> s {sLastTimestamp = x}
  sPutRepeatNumber x = modify $ \s -> s {sRepeatNumber = x}
  sPutRepeatTimestamp x = modify $ \s -> s {sRepeatTimestamp = x}

instance HasTelegramConst (BotMonad TelegramEnv) where
  getTelegramConst = gets tTelegramConst

instance HasTelegramMod (BotMonad TelegramEnv) where
  tGetLastUpdateId = gets tLastUpdateId
  tGetRepeatMap = gets tRepeatMap
  tPutLastUpdateId x = modify $ \s -> s {tLastUpdateId = x}
  tPutRepeatMap x = modify $ \s -> s {tRepeatMap = x}
