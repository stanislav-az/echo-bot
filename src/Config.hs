{-# LANGUAGE OverloadedStrings #-}

module Config where

import Bot.BotMonad
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
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Slack.Models
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
  hMsg <- getByName conf "telegram.help_msg"
  rMsg <- getByName conf "telegram.repeat_msg"
  rNum <- getByName conf "telegram.repeat_number"
  pure $
    TelegramEnv
      { tLastUpdateId = Nothing
      , tToken = token
      , tHelpMsg = hMsg
      , tRepeatMsg = rMsg
      , tRepeatNumber = rNum
      , tRepeatMap = HM.empty
      }

makeSlackEnv :: IO SlackEnv
makeSlackEnv = do
  conf <- loadConfig
  token <- getByName conf "slack.token"
  channel <- getByName conf "slack.channel"
  hMsg <- getByName conf "slack.help_msg"
  rMsg <- getByName conf "slack.repeat_msg"
  rNum <- getByName conf "slack.repeat_number"
  pure $
    SlackEnv
      { sLastTimestamp = Nothing
      , sToken = token
      , sChannel = channel
      , sHelpMsg = hMsg
      , sRepeatMsg = rMsg
      , sRepeatNumber = rNum
      , sRepeatTimestamp = Nothing
      }
