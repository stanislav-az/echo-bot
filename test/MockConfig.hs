{-# LANGUAGE OverloadedStrings #-}

module MockConfig where

import Bot.BotClass (SlackConst(..), TelegramConst(..))
import Bot.BotMonad (SlackEnv(..), TelegramEnv(..))
import qualified Data.HashMap.Strict as HM (empty)

getTelegramEnv :: TelegramEnv
getTelegramEnv =
  TelegramEnv
    { tTelegramConst =
        TelegramConst
          { tConstToken = "telegram_token"
          , tConstHelpMsg = "telegram_help_msg"
          , tConstRepeatMsg = "telegram_repeat_msg"
          , tConstRepeatNumber = 1
          }
    , tLastUpdateId = Nothing
    , tRepeatMap = HM.empty
    }

getSlackEnv :: SlackEnv
getSlackEnv =
  SlackEnv
    { sSlackConst =
        SlackConst
          { sConstToken = "slack_token"
          , sConstChannel = "slack_channel"
          , sConstHelpMsg = "slack_help_msg"
          , sConstRepeatMsg = "slack_repeat_msg"
          }
    , sLastTimestamp = Nothing
    , sRepeatNumber = 1
    , sRepeatTimestamp = Nothing
    }
