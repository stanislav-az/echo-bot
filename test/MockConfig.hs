{-# LANGUAGE OverloadedStrings #-}

module MockConfig where

import Bot.BotClass (BotConst(..))
import Bot.BotMonad (SlackEnv(..), TelegramEnv(..))
import qualified Data.HashMap.Strict as HM (empty)
import Slack.BotClass (SlackConst(..))
import Telegram.BotClass (TelegramConst(..))

getTelegramEnv :: TelegramEnv
getTelegramEnv =
  TelegramEnv
    { tBotConst =
        BotConst
          { helpMsg = "telegram_help_msg"
          , repeatMsg = "telegram_repeat_msg"
          , defaultRepeatNumber = 1
          }
    , tTelegramConst = TelegramConst "telegram_token"
    , tLastMsg = Nothing
    , tRepeatMap = HM.empty
    }

getSlackEnv :: SlackEnv
getSlackEnv =
  SlackEnv
    { sBotConst =
        BotConst
          { helpMsg = "slack_help_msg"
          , repeatMsg = "slack_repeat_msg"
          , defaultRepeatNumber = 1
          }
    , sSlackConst =
        SlackConst
          {sConstToken = "slack_token", sConstChannel = "slack_channel"}
    , sLastMsg = Nothing
    , sTimestamp = Nothing
    , sRepeatMap = HM.empty
    }
