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

data SlackConst = SlackConst
  { sConstToken :: String
  , sConstChannel :: String
  , sConstHelpMsg :: T.Text
  , sConstRepeatMsg :: T.Text
  } deriving (Eq, Show)

class (Monad m) =>
      HasSlackConst m
  where
  getSlackConst :: m SlackConst

class (Monad m) =>
      HasSlackMod m
  where
  sGetLastTimestamp :: m (Maybe String)
  sGetRepeatNumber :: m Int
  sGetRepeatTimestamp :: m (Maybe String)
  sPutLastTimestamp :: Maybe String -> m ()
  sPutRepeatNumber :: Int -> m ()
  sPutRepeatTimestamp :: Maybe String -> m ()
  sModLastTimestamp :: (Maybe String -> Maybe String) -> m ()
  sModLastTimestamp f = sGetLastTimestamp >>= (sPutLastTimestamp . f)
  sModRepeatNumber :: (Int -> Int) -> m ()
  sModRepeatNumber f = sGetRepeatNumber >>= (sPutRepeatNumber . f)
  sModRepeatTimestamp :: (Maybe String -> Maybe String) -> m ()
  sModRepeatTimestamp f = sGetRepeatTimestamp >>= (sPutRepeatTimestamp . f)

data TelegramConst = TelegramConst
  { tConstToken :: String
  , tConstHelpMsg :: T.Text
  , tConstRepeatMsg :: T.Text
  , tConstRepeatNumber :: Int
  } deriving (Eq, Show)

class (Monad m) =>
      HasTelegramConst m
  where
  getTelegramConst :: m TelegramConst

class (Monad m) =>
      HasTelegramMod m
  where
  tGetLastUpdateId :: m (Maybe Integer)
  tGetRepeatMap :: m (HM.HashMap Integer Int)
  tPutLastUpdateId :: Maybe Integer -> m ()
  tPutRepeatMap :: HM.HashMap Integer Int -> m ()
  tModLastUpdateId :: (Maybe Integer -> Maybe Integer) -> m ()
  tModLastUpdateId f = tGetLastUpdateId >>= (tPutLastUpdateId . f)
  tModRepeatMap :: (HM.HashMap Integer Int -> HM.HashMap Integer Int) -> m ()
  tModRepeatMap f = tGetRepeatMap >>= (tPutRepeatMap . f)
