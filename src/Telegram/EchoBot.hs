{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.EchoBot
  ( telegramBot
  ) where

import Bot.BotClass
import Bot.BotMonad (BotException(..))
import Bot.EchoBot (BotMessage(..), EchoBot(..))
import Bot.Exception
  ( checkResponseStatus
  , throwBotLogicMisuse
  , throwParseException
  )
import Control.Monad (replicateM_, when)
import Control.Monad.Catch
import qualified Data.Aeson as JSON (decode)
import qualified Data.HashMap.Strict as HM (insert, lookupDefault)
import qualified Data.HashMap.Strict as HM (HashMap(..))
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T (Text(..), pack)
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)
import qualified Network.HTTP.Simple as HTTP (getResponseBody)
import qualified Safe (lastMay)
import Telegram.BotClass
import Telegram.Models
import Telegram.Requests
import Telegram.Serializer
import qualified Text.Read as T (readMaybe)

telegramBot ::
     (MonadHTTP m, MonadThrow m, MonadTelegramStaticOptions m)
  => EchoBot m TelegramMessage (HM.HashMap T.Text Int)
telegramBot =
  EchoBot
    { getUpdates = tGetUpdates
    , findLastMsg = tFindLastMsg
    , routeMsg = tRouteMsg
    , sendMsg = tSendMsg
    , putHelpTextInMsg = tReplaceMsgText False
    , putRepeatTextInMsg = tReplaceMsgText True
    , parseToRepeatNumber = tParseToRepeatNumber
    , convertToTextualChat = tConvertToTextualChat
    , convertToTextualMsg = tConvertToTextualMsg
    }

tFindLastMsg :: [TelegramMessage] -> Maybe TelegramMessage
tFindLastMsg = fmap modifyUpdateId . Safe.lastMay
  where
    modifyUpdateId tc@Callback {..} = tc {tcUpdateId = tcUpdateId + 1}
    modifyUpdateId tm@Message {..} = tm {tmUpdateId = tmUpdateId + 1}

tGetUpdates ::
     (MonadTelegramStaticOptions m, MonadHTTP m, MonadThrow m)
  => Maybe TelegramMessage
  -> m [TelegramMessage]
tGetUpdates lastMsg = do
  token <- tConstToken <$> getTelegramStaticOptions
  let getUpdates = makeGetUpdates token $ fmap getUpdateId lastMsg
  response <- http getUpdates
  checkResponseStatus response
  let unparsed = HTTP.getResponseBody response
      parsed = JSON.decode unparsed
  tResponse <- maybe (throwParseException unparsed) pure parsed
  pure $ tResponseToMsgs tResponse

tRouteMsg :: TelegramMessage -> BotMessage
tRouteMsg Message {..} =
  case tmText of
    "/help" -> HelpMsg
    "/repeat" -> RepeatMsg
    _ -> PlainMsg
tRouteMsg _ = ReactionMsg

tSendMsg ::
     (MonadTelegramStaticOptions m, MonadHTTP m, MonadThrow m)
  => TelegramMessage
  -> m ()
tSendMsg msg@Message {..} = do
  token <- tConstToken <$> getTelegramStaticOptions
  let req =
        if tmHasKeyboard
          then makeCallbackQuery token tmChatId tmText
          else makeSendMessage token tmChatId tmText
  response <- http req
  checkResponseStatus response
tSendMsg _ = throwBotLogicMisuse "Could not send this TelegramMessage"

tParseToRepeatNumber ::
     (MonadThrow m, MonadTelegramStaticOptions m, MonadHTTP m)
  => TelegramMessage
  -> m (Maybe Int)
tParseToRepeatNumber Callback {..} = do
  let btn = T.readMaybe tcData :: Maybe Int
  when (isNothing btn) $ throwM $ BadCallbackData tcData
  token <- tConstToken <$> getTelegramStaticOptions
  let req = makeAnswerCallbackQuery token tcId $ T.pack tcData
  response <- http req
  checkResponseStatus response
  pure btn

tReplaceMsgText ::
     MonadThrow m => Bool -> T.Text -> TelegramMessage -> m TelegramMessage
tReplaceMsgText hasKeyboard text tm@Message {..} =
  pure tm {tmText = text, tmHasKeyboard = hasKeyboard}
tReplaceMsgText _ _ _ =
  throwBotLogicMisuse "This TelegramMessage has callback data"

tConvertToTextualChat :: Monad m => TelegramMessage -> m T.Text
tConvertToTextualChat Message {..} = pure $ textify tmChatId
tConvertToTextualChat Callback {..} = pure $ textify tcChatId

tConvertToTextualMsg :: MonadThrow m => TelegramMessage -> m T.Text
tConvertToTextualMsg Message {..} = pure tmText
tConvertToTextualMsg _ =
  throwBotLogicMisuse "This TelegramMessage has callback data"
