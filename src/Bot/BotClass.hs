module Bot.BotClass where

import qualified Data.Text                     as T
                                                ( Text(..) )
import qualified Control.Logger.Simple         as L
                                                ( logDebug
                                                , logInfo
                                                , logWarn
                                                , logError
                                                )

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
