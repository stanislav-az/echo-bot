{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.BotClass where

import qualified Control.Concurrent as CC (threadDelay)
import qualified Control.Logger.Simple as L
  ( logDebug
  , logError
  , logInfo
  , logWarn
  )
import qualified Data.ByteString.Lazy as LB (ByteString(..))
import qualified Data.HashMap.Strict as HM (HashMap(..))
import qualified Data.Text as T (Text(..))
import qualified Network.HTTP.Simple as HTTP
  ( Request(..)
  , Response(..)
  , httpLBS
  )

class (Monad m) =>
      MonadLogger m
  where
  logDebug :: T.Text -> m ()
  logInfo :: T.Text -> m ()
  logWarn :: T.Text -> m ()
  logError :: T.Text -> m ()

instance MonadLogger IO where
  logDebug = L.logDebug
  logInfo = L.logInfo
  logWarn = L.logWarn
  logError = L.logError

class (Monad m) =>
      MonadDelay m
  where
  delay :: Int -> m ()

instance MonadDelay IO where
  delay = CC.threadDelay

class (Monad m) =>
      MonadHTTP m
  where
  http :: HTTP.Request -> m (HTTP.Response LB.ByteString)

instance MonadHTTP IO where
  http = HTTP.httpLBS

data BotConst = BotConst
  { helpMsg :: T.Text
  , repeatMsg :: T.Text
  , defaultRepeatNumber :: Int
  } deriving (Eq, Show)

class (Monad m) =>
      HasBotConst m
  where
  getBotConst :: m BotConst

data SlackConst = SlackConst
  { sConstToken :: String
  , sConstChannel :: String
  } deriving (Eq, Show)

class (Monad m) =>
      HasSlackConst m
  where
  getSlackConst :: m SlackConst

data TelegramConst = TelegramConst
  { tConstToken :: String
  } deriving (Eq, Show)

class (Monad m) =>
      HasTelegramConst m
  where
  getTelegramConst :: m TelegramConst

class (Monad m) =>
      MonadFlagState m flag
  where
  getFlag :: m (Maybe flag)
  putFlag :: Maybe flag -> m ()
  modifyFlag :: (Maybe flag -> Maybe flag) -> m ()
  modifyFlag f = getFlag >>= (putFlag . f)

class (Monad m) =>
      MonadIterState m iter
  where
  getIterator :: m (Maybe iter)
  putIterator :: Maybe iter -> m ()
  modifyIterator :: (Maybe iter -> Maybe iter) -> m ()
  modifyIterator f = getIterator >>= (putIterator . f)

class (Monad m) =>
      MonadRepeatState m rmap
  where
  getRepeatMap :: m rmap
  putRepeatMap :: rmap -> m ()
  modifyRepeatMap :: (rmap -> rmap) -> m ()
  modifyRepeatMap f = getRepeatMap >>= (putRepeatMap . f)
