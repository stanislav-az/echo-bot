{-# LANGUAGE FlexibleContexts #-}

module Errors where

import           Control.Monad.Except
import           Control.Monad.State
import           Telegram.Bot
import           Slack.Bot
import           Data.Text
import           Data.HashMap.Strict
import           Bot
import           Bot.BotMonad
import           Bot.BotClass
import           Helpers
import           Network.HTTP.Simple

botErrorHandler :: MonadLogger m => BotError -> m ()
botErrorHandler = logWarn . texify

checkResponseStatus :: (MonadError BotError f, Show a) => Response a -> f ()
checkResponseStatus response =
  unless (isOkResponse response) $ throwError $ ResponseError
    (status ++ " " ++ body)
 where
  status = show $ getResponseStatus response
  body   = show $ getResponseBody response

throwParseError :: (MonadError BotError f, Show a) => a -> f ()
throwParseError = throwError . NoParse . show
