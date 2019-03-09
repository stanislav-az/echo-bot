{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.EchoBot
  ( telegramBot
  ) where

import Bot.BotClass
import Bot.BotMonad (BotException(..))
import Bot.EchoBot (EchoBot(..))
import Bot.Exception (checkResponseStatus, throwParseException)
import Control.Monad (replicateM_, when)
import Control.Monad.Catch
import qualified Data.Aeson as JSON (decode)
import qualified Data.HashMap.Strict as HM (insert, lookupDefault)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T (pack)
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)
import qualified Network.HTTP.Simple as HTTP (getResponseBody)
import Serializer.Telegram
import Telegram.Models
import Telegram.Requests
import qualified Text.Read as T (readMaybe)

telegramBot ::
     ( MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     , HasTelegramConst m
     , HasTelegramMod m
     )
  => EchoBot m TelegramMessage TelegramReaction
telegramBot =
  EchoBot
    { getUpdates = tGetUpdates
    , handleMsg = tHandleMsg
    , handleReaction = tHandleReaction
    }

tGetUpdates ::
     (MonadHTTP m, MonadThrow m, HasTelegramConst m, HasTelegramMod m)
  => m ([TelegramMessage], [TelegramReaction])
tGetUpdates = do
  offset <- tGetLastUpdateId
  token <- tConstToken <$> getTelegramConst
  let getUpdates = makeGetUpdates offset token
  response <- http getUpdates
  checkResponseStatus response
  let unparsed = HTTP.getResponseBody response
      parsed = JSON.decode unparsed :: Maybe TResponse
  tResponse <-
    maybe (throwParseException unparsed >> pure emptyTResponse) pure parsed
  pure $ tResponseToModels tResponse

tModifyIterator :: Integer -> Maybe Integer -> Maybe Integer
tModifyIterator uid tLastUpdateId = maybe ifNothing ifJust tLastUpdateId
  where
    ifNothing = Just $ uid + 1
    ifJust offset
      | uid + 1 > offset = ifNothing
      | otherwise = tLastUpdateId

tHandleMsg ::
     ( MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     , HasTelegramConst m
     , HasTelegramMod m
     )
  => TelegramMessage
  -> m ()
tHandleMsg (TelegramMessage uid chatId "/help") = do
  mText <- tConstHelpMsg <$> getTelegramConst
  tSendMsg (TelegramMessage uid chatId mText) False
  tModLastUpdateId $ tModifyIterator uid
tHandleMsg (TelegramMessage uid chatId "/repeat") = do
  rMsg <- tConstRepeatMsg <$> getTelegramConst
  r <- tConstRepeatNumber <$> getTelegramConst
  chatsRepeat <- tGetRepeatMap
  let currR = HM.lookupDefault r chatId chatsRepeat
      rnMsg = TelegramMessage uid chatId $ rMsg <> textify currR
  tSendMsg rnMsg True
  tModLastUpdateId $ tModifyIterator uid
tHandleMsg msg@TelegramMessage {..} = do
  r <- tConstRepeatNumber <$> getTelegramConst
  chatsRepeat <- tGetRepeatMap
  let currR = HM.lookupDefault r tmChatId chatsRepeat
  replicateM_ currR $ tSendMsg msg False
  tModLastUpdateId $ tModifyIterator tmUpdateId

tHandleReaction ::
     ( MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     , HasTelegramConst m
     , HasTelegramMod m
     )
  => TelegramReaction
  -> m ()
tHandleReaction tr@TelegramReaction {..} = do
  chatsRepeat <- tGetRepeatMap
  let btn = T.readMaybe trCallbackData :: Maybe Int
  when (isNothing btn) $ throwM $ BadCallbackData trCallbackData
  token <- tConstToken <$> getTelegramConst
  let chatsRepeat' = HM.insert trChatId (fromJust btn) chatsRepeat
      req = makeAnswerCallbackQuery token tr
  response <- http req
  checkResponseStatus response
  tPutRepeatMap chatsRepeat'
  logChatRepeat (textify trChatId) (T.pack trCallbackData)
  tModLastUpdateId $ tModifyIterator trUpdateId

tSendMsg ::
     (MonadHTTP m, MonadThrow m, MonadLogger m, HasTelegramConst m)
  => TelegramMessage
  -> Bool
  -> m ()
tSendMsg msg@TelegramMessage {..} hasKeyboard = do
  token <- tConstToken <$> getTelegramConst
  let req =
        if hasKeyboard
          then makeCallbackQuery token msg
          else makeSendMessage token msg
  response <- http req
  checkResponseStatus response
  logChatMessage (textify tmChatId) tmText
