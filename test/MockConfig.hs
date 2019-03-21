{-# LANGUAGE OverloadedStrings #-}

module MockConfig where

import Bot.BotClass (BotStaticOptions(..))
import Bot.BotMonad (SlackEnv(..), TelegramEnv(..))
import qualified Data.HashMap.Strict as HM (empty)
import Slack.BotClass (SlackStaticOptions(..))
import Telegram.BotClass (TelegramStaticOptions(..))

getTelegramEnv :: TelegramEnv
getTelegramEnv =
  TelegramEnv
    { tBotStaticOptions =
        BotStaticOptions
          { helpMsg = "telegram_help_msg"
          , repeatMsg = "telegram_repeat_msg"
          , defaultRepeatNumber = 1
          }
    , tTelegramStaticOptions = TelegramStaticOptions "telegram_token"
    , tLastMsg = Nothing
    , tRepeatMap = HM.empty
    }

getSlackEnv :: SlackEnv
getSlackEnv =
  SlackEnv
    { sBotStaticOptions =
        BotStaticOptions
          { helpMsg = "slack_help_msg"
          , repeatMsg = "slack_repeat_msg"
          , defaultRepeatNumber = 1
          }
    , sSlackStaticOptions =
        SlackStaticOptions
          {sConstToken = "slack_token", sConstChannel = "slack_channel"}
    , sLastMsg = Nothing
    , sTimestamp = Nothing
    , sRepeatMap = HM.empty
    }
