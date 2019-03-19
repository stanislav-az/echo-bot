{-# LANGUAGE OverloadedStrings #-}

module Config where

import Bot.BotClass (BotConst(..), SlackConst(..), TelegramConst(..))
import Bot.BotMonad (SlackEnv(..), TelegramEnv(..))
import qualified Control.Logger.Simple as L (LogConfig(..))
import qualified Data.Configurator as C
  ( autoConfig
  , autoReload
  , lookup
  , require
  )
import qualified Data.Configurator.Types as C
  ( Config(..)
  , Configured(..)
  , Name(..)
  , Value(..)
  , Worth(..)
  )
import qualified Data.HashMap.Strict as HM (empty)
import qualified Data.Text as T (Text(..), init, replace)
import qualified System.Directory as D (createDirectoryIfMissing)
import qualified System.FilePath.Posix as D (takeDirectory)

data Bot
  = Telegram
  | Slack
  deriving (Show, Eq)

instance C.Configured Bot where
  convert (C.String "Telegram") = Just Telegram
  convert (C.String "Slack") = Just Slack
  convert _ = Nothing

loadConfig :: IO C.Config
loadConfig = fst <$> C.autoReload C.autoConfig [C.Required "./config/bot.local"]

getByName :: C.Configured a => C.Config -> C.Name -> IO a
getByName = C.require

getMaybe :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
getMaybe = C.lookup

getLogConfig :: C.Config -> IO L.LogConfig
getLogConfig conf = do
  logToStdout <- getByName conf "logging.log_to_stdout"
  logToFile <- getMaybe conf "logging.log_to_file"
  let logDir = D.takeDirectory <$> logToFile
  maybe (pure ()) (D.createDirectoryIfMissing True) logDir
  pure $ L.LogConfig {L.lc_file = logToFile, L.lc_stderr = logToStdout}

makeTelegramEnv :: IO TelegramEnv
makeTelegramEnv = do
  conf <- loadConfig
  token <- getByName conf "telegram.token"
  hMsg <- getTelegramHelpMsg conf
  rMsg <- getRepeatMsg conf
  rNum <- getByName conf "repeat_number"
  pure $
    TelegramEnv
      { tBotConst = BotConst hMsg rMsg rNum
      , tTelegramConst = TelegramConst token
      , tLastMsg = Nothing
      , tRepeatMap = HM.empty
      }

makeSlackEnv :: IO SlackEnv
makeSlackEnv = do
  conf <- loadConfig
  token <- getByName conf "slack.token"
  channel <- getByName conf "slack.channel"
  hMsg <- getSlackHelpMsg conf
  rMsg <- getRepeatMsg conf
  rNum <- getByName conf "repeat_number"
  pure $
    SlackEnv
      { sBotConst = BotConst hMsg rMsg rNum
      , sSlackConst = SlackConst token channel
      , sLastMsg = Nothing
      , sTimestamp = Nothing
      , sRepeatMap = HM.empty
      }

getSlackHelpMsg :: C.Config -> IO T.Text
getSlackHelpMsg conf =
  T.replace "*" <$> getByName conf "slack.repeat_command" <*>
  getByName conf "help_msg"

getTelegramHelpMsg :: C.Config -> IO T.Text
getTelegramHelpMsg conf =
  T.replace "*" <$> getByName conf "telegram.repeat_command" <*>
  getByName conf "help_msg"

getRepeatMsg :: C.Config -> IO T.Text
getRepeatMsg conf = T.init <$> getByName conf "repeat_msg"
