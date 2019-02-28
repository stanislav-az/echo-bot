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

-- data EchoBot  msg = EchoBot{
--   getUpdates :: Int -> IO [msg],
--   getReactions :: Int -> IO Int,
--   sendHelp :: T.Text -> IO (),
--   sendMsgBack :: msg -> IO ()
-- }

-- telegramBot :: EchoBot TelegramMessage
-- telegramBot = EchoBot { getUpdates = \uid -> pure [] }
