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
import Data.Proxy
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

data BotStaticOptions = BotStaticOptions
  { helpMsg :: T.Text
  , repeatMsg :: T.Text
  , defaultRepeatNumber :: Int
  } deriving (Eq, Show)

class (Monad m) =>
      MonadBotStaticOptions m
  where
  getBotStaticOptions :: m BotStaticOptions

class (Monad m) =>
      MonadLastMsgState m msg
  where
  getLastMsg :: m (Maybe msg)
  putLastMsg :: Maybe msg -> m ()

class (Monad m) =>
      MonadRepeatMapState m map
  where
  getRepeatMap :: m (map T.Text Int)
  putRepeatMap :: map T.Text Int -> m ()
  modifyRepeatMap :: (map T.Text Int -> map T.Text Int) -> m ()
  modifyRepeatMap f = getRepeatMap >>= (putRepeatMap . f)
  lookupRepeatDefault :: Proxy map -> Int -> T.Text -> m Int
  insertRepeat :: Proxy map -> T.Text -> Int -> m ()
