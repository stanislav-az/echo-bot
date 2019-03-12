{-# LANGUAGE OverloadedStrings #-}

module MockConfig where

import Bot.BotClass (SlackConst(..), TelegramConst(..))
import Bot.BotMonad (SlackEnv(..), TelegramEnv(..))
import qualified Data.HashMap.Strict as HM (empty)

getTelegramEnv :: TelegramEnv
getTelegramEnv =
  TelegramEnv
    { tTelegramConst = TelegramConst "telegram_token"
    , tLastUpdateId = Nothing
    , tRepeatMap = HM.empty
    , tHelpMsg = "telegram_help_msg"
    , tRepeatMsg = "telegram_repeat_msg"
    , tDefaultRepeatNumber = 1
    }

getSlackEnv :: SlackEnv
getSlackEnv =
  SlackEnv
    { sSlackConst =
        SlackConst
          {sConstToken = "slack_token", sConstChannel = "slack_channel"}
    , sLastTimestamp = Nothing
    , sRepeatMap = Nothing
    , sRepeatTimestamp = Nothing
    , sHelpMsg = "slack_help_msg"
    , sRepeatMsg = "slack_repeat_msg"
    , sDefaultRepeatNumber = 1
    }
