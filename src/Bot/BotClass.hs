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
import qualified Data.HashMap.Strict as HM (HashMap(..), insert, lookupDefault)
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
      MonadBotConst m
  where
  getBotConst :: m BotConst

class (Monad m) =>
      MonadLastMsgState m msg
  where
  getLastMsg :: m (Maybe msg)
  putLastMsg :: Maybe msg -> m ()

class (Monad m) =>
      MonadRepeatMapState m
  where
  getRepeatMap :: m (HM.HashMap T.Text Int)
  putRepeatMap :: HM.HashMap T.Text Int -> m ()
  modifyRepeatMap :: (HM.HashMap T.Text Int -> HM.HashMap T.Text Int) -> m ()
  modifyRepeatMap f = getRepeatMap >>= (putRepeatMap . f)
  lookupRepeatDefault :: Int -> T.Text -> m Int
  lookupRepeatDefault v k = HM.lookupDefault v k <$> getRepeatMap
  insertRepeat :: T.Text -> Int -> m ()
  insertRepeat k v = modifyRepeatMap $ HM.insert k v
