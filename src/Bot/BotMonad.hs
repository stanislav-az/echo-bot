{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.BotMonad where

import Bot.BotClass
import Control.Monad.Catch
import Control.Monad.State
import qualified Data.HashMap.Strict as HM (HashMap(..), insert, lookupDefault)
import qualified Data.Text as T (Text(..))
import Slack.BotClass
import Slack.Models
import Telegram.BotClass
import Telegram.Models

data BotException
  = NoParse ResponseBody
  | BadCallbackData CallbackData
  | ResponseException String
  | BotLogicMisuse String
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

data SlackEnv = SlackEnv
  { sBotStaticOptions :: BotStaticOptions
  , sSlackStaticOptions :: SlackStaticOptions
  , sLastMsg :: Maybe SlackMessage
  , sTimestamp :: Maybe String
  , sRepeatMap :: HM.HashMap T.Text Int
  } deriving (Eq, Show)

type SlackMonad a = BotMonad SlackEnv a

data TelegramEnv = TelegramEnv
  { tBotStaticOptions :: BotStaticOptions
  , tTelegramStaticOptions :: TelegramStaticOptions
  , tLastMsg :: Maybe TelegramMessage
  , tRepeatMap :: HM.HashMap T.Text Int
  } deriving (Eq, Show)

type TelegramMonad a = BotMonad TelegramEnv a

instance MonadSlackStaticOptions (BotMonad SlackEnv) where
  getSlackStaticOptions = gets sSlackStaticOptions

instance MonadBotStaticOptions (BotMonad SlackEnv) where
  getBotStaticOptions = gets sBotStaticOptions

instance MonadTimestampState (BotMonad SlackEnv) where
  getTimestamp = gets sTimestamp
  putTimestamp futureMsg = modify $ \s -> s {sTimestamp = futureMsg}

instance MonadLastMsgState (BotMonad SlackEnv) SlackMessage where
  getLastMsg = gets sLastMsg
  putLastMsg lastMsg = modify $ \s -> s {sLastMsg = lastMsg}

instance MonadRepeatMapState (BotMonad SlackEnv) HM.HashMap where
  getRepeatMap = gets sRepeatMap
  putRepeatMap repeatMap = modify $ \s -> s {sRepeatMap = repeatMap}
  lookupRepeatDefault _ v k = HM.lookupDefault v k <$> getRepeatMap
  insertRepeat _ k v = modifyRepeatMap $ HM.insert k v

instance MonadTelegramStaticOptions (BotMonad TelegramEnv) where
  getTelegramStaticOptions = gets tTelegramStaticOptions

instance MonadBotStaticOptions (BotMonad TelegramEnv) where
  getBotStaticOptions = gets tBotStaticOptions

instance MonadLastMsgState (BotMonad TelegramEnv) TelegramMessage where
  getLastMsg = gets tLastMsg
  putLastMsg lastMsg = modify $ \s -> s {tLastMsg = lastMsg}

instance MonadRepeatMapState (BotMonad TelegramEnv) HM.HashMap where
  getRepeatMap = gets tRepeatMap
  putRepeatMap repeatMap = modify $ \s -> s {tRepeatMap = repeatMap}
  lookupRepeatDefault _ v k = HM.lookupDefault v k <$> getRepeatMap
  insertRepeat _ k v = modifyRepeatMap $ HM.insert k v
