{-# LANGUAGE OverloadedStrings #-}

module Logging where

import Bot.BotClass
import qualified Data.Text as T

logChatMessage :: MonadLogger m => T.Text -> T.Text -> m ()
logChatMessage chat msg =
  logDebug $ "A message was sent... To: " <> chat <> " Text: " <> msg

logChatRepeat :: MonadLogger m => T.Text -> T.Text -> m ()
logChatRepeat chat repeat =
  logDebug $
  "A number of repeats was changed... For: " <> chat <> " To: " <> repeat
