{-# LANGUAGE OverloadedStrings #-}

module MockConfig where

import Bot.BotMonad
import qualified Data.HashMap.Strict as HM

getTelegramEnv :: TelegramEnv
getTelegramEnv =
  TelegramEnv
    { tLastUpdateId = Nothing
    , tToken = "telegram_token"
    , tHelpMsg = "telegram_help_msg"
    , tRepeatMsg = "telegram_repeat_msg"
    , tRepeatNumber = 1
    , tRepeatMap = HM.empty
    }

getSlackEnv :: SlackEnv
getSlackEnv =
  SlackEnv
    { sLastTimestamp = Nothing
    , sToken = "slack_token"
    , sChannel = "slack_channel"
    , sHelpMsg = "slack_help_msg"
    , sRepeatMsg = "slack_repeat_msg"
    , sRepeatNumber = 1
    , sRepeatTimestamp = Nothing
    }
