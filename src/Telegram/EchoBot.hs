{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.EchoBot
  ( telegramBot
  ) where

import Bot.BotClass
import Bot.BotMonad (BotException(..))
import Bot.EchoBot (BotMessage(..), EchoBot(..))
import Bot.Exception (checkResponseStatus, throwParseException)
import Control.Monad (replicateM_, when)
import Control.Monad.Catch
import qualified Data.Aeson as JSON (decode)
import qualified Data.HashMap.Strict as HM (insert, lookupDefault)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T (Text(..), pack)
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)
import qualified Network.HTTP.Simple as HTTP (getResponseBody)
import Serializer.Telegram
import Telegram.Models
import Telegram.Requests
import qualified Text.Read as T (readMaybe)

telegramBot ::
     (MonadHTTP m, MonadThrow m, HasTelegramConst m)
  => EchoBot m TelegramMessage TelegramReaction TelegramFlag TelegramAnticipation TelegramIterator TelegramRepeatMap
telegramBot =
  EchoBot
    { getUpdates = tGetUpdates
    , routeMsg = tRouteMsg
    , sendMsg = tSendMsg False
    , sendRepeatMsg = tSendMsg True
    , iteratorTransformation = tIteratorTransformation
    , repeatMapTransformation = tRepeatMapTransformation
    , parseReaction = tParseReaction
    , parseAnticipation = tParseAnticipation
    , getCurrentRepeatNumber = tGetCurrentRepeatNumber
    , replaceMsgText = tReplaceMsgText
    , getTextualChat = tGetTextualChat
    , getTextualMsg = tGetTextualMsg
    }

tGetUpdates ::
     (HasTelegramConst m, MonadHTTP m, MonadThrow m)
  => Maybe TelegramFlag
  -> Maybe Integer
  -> m ([TelegramMessage], [TelegramReaction])
tGetUpdates _ offset = do
  token <- tConstToken <$> getTelegramConst
  let getUpdates = makeGetUpdates offset token
  response <- http getUpdates
  checkResponseStatus response
  let unparsed = HTTP.getResponseBody response
      parsed = JSON.decode unparsed :: Maybe TResponse
  tResponse <-
    maybe (throwParseException unparsed >> pure emptyTResponse) pure parsed
  pure $ tResponseToModels tResponse

tIteratorTransformation ::
     Either TelegramReaction TelegramMessage -> Maybe Integer -> Maybe Integer
tIteratorTransformation rm tLastUpdateId = maybe ifNothing ifJust tLastUpdateId
  where
    uid = either trUpdateId tmUpdateId rm
    ifNothing = Just $ uid + 1
    ifJust offset
      | uid + 1 > offset = ifNothing
      | otherwise = tLastUpdateId

tRepeatMapTransformation ::
     Int -> TelegramReaction -> TelegramRepeatMap -> TelegramRepeatMap
tRepeatMapTransformation repeat TelegramReaction {..} =
  HM.insert trChatId repeat

tRouteMsg :: TelegramMessage -> BotMessage
tRouteMsg TelegramMessage {..} =
  case tmText of
    "/help" -> HelpMsg
    "/repeat" -> RepeatMsg
    _ -> Msg

tSendMsg ::
     (HasTelegramConst m, MonadHTTP m, MonadThrow m)
  => Bool
  -> TelegramMessage
  -> m ()
tSendMsg hasKeyboard msg@TelegramMessage {..} = do
  token <- tConstToken <$> getTelegramConst
  let req =
        if hasKeyboard
          then makeCallbackQuery token msg
          else makeSendMessage token msg
  response <- http req
  checkResponseStatus response

tParseReaction ::
     (MonadThrow m, HasTelegramConst m, MonadHTTP m)
  => TelegramReaction
  -> m (Maybe Int)
tParseReaction tr@TelegramReaction {..} = do
  let btn = T.readMaybe trCallbackData :: Maybe Int
  when (isNothing btn) $ throwM $ BadCallbackData trCallbackData
  token <- tConstToken <$> getTelegramConst
  let req = makeAnswerCallbackQuery token tr
  response <- http req
  checkResponseStatus response
  pure btn

tParseAnticipation :: Monad m => TelegramAnticipation -> m (Maybe TelegramFlag)
tParseAnticipation _ = pure Nothing

tGetCurrentRepeatNumber :: Int -> TelegramRepeatMap -> TelegramMessage -> Int
tGetCurrentRepeatNumber r repeatMap TelegramMessage {..} =
  HM.lookupDefault r tmChatId repeatMap

tReplaceMsgText :: T.Text -> TelegramMessage -> TelegramMessage
tReplaceMsgText text tm = tm {tmText = text}

tGetTextualChat ::
     Monad m => Either TelegramReaction TelegramMessage -> m T.Text
tGetTextualChat rm = pure $ textify chat
  where
    chat = either trChatId tmChatId rm

tGetTextualMsg :: TelegramMessage -> T.Text
tGetTextualMsg = tmText
