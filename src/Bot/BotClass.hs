module Bot.BotClass where

import qualified Network.HTTP.Simple           as HTTP
import qualified Data.Text                     as T
                                                ( Text(..) )
import qualified Data.ByteString.Lazy          as LB
import qualified Control.Logger.Simple         as L
                                                ( logDebug
                                                , logInfo
                                                , logWarn
                                                , logError
                                                )
import qualified Control.Concurrent            as CC
                                                ( threadDelay )
import           Bot.Request

class (Monad m) => MonadLogger m where
  logDebug :: T.Text -> m ()
  logInfo  :: T.Text -> m ()
  logWarn  :: T.Text -> m ()
  logError :: T.Text -> m ()

instance MonadLogger IO where
  logDebug = L.logDebug
  logInfo  = L.logInfo
  logWarn  = L.logWarn
  logError = L.logError

class (Monad m) => MonadDelay m where
  delay :: Int -> m ()

instance MonadDelay IO where
  delay = CC.threadDelay

class MonadHTTP m where
  http :: Request -> m (HTTP.Response LB.ByteString)

instance  MonadHTTP IO where
  http = HTTP.httpLBS . unwrapRequest
