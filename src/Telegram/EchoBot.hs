{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.EchoBot
  ( telegramBot
  )
where

import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           Bot.EchoBot
import           Telegram.Models
import           Control.Monad.State
import           Control.Monad.Catch
import           Bot.BotClass
import           Bot.BotMonad
import           Bot.Exception
import           Telegram.Requests
import           Network.HTTP.Simple
import           Serializer.Telegram
import           Data.Aeson
import           Logging
import           Helpers
import           Text.Read                      ( readMaybe )
import           Data.Maybe

telegramBot
  :: ( MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     , HasTelegramEnv m
     , HasTelegramMod m
     )
  => EchoBot m TelegramMessage TelegramReaction
telegramBot = EchoBot { getUpdates     = tGetUpdates
                      , handleMsg      = tHandleMsg
                      , handleReaction = tHandleReaction
                      }

tGetUpdates
  :: (MonadHTTP m, MonadThrow m, HasTelegramEnv m, HasTelegramMod m)
  => m ([TelegramMessage], [TelegramReaction])
tGetUpdates = do
  offset <- tGetLastUpdateId
  token  <- tEnvToken
  let getUpdates = makeGetUpdates offset token
  response <- http getUpdates
  checkResponseStatus response
  let unparsed = getResponseBody response
      parsed   = decode unparsed :: Maybe TResponse
  tResponse <- maybe (throwParseException unparsed >> pure emptyTResponse)
                     pure
                     parsed
  pure $ tResponseToModels tResponse

tModifyIterator :: Integer -> Maybe Integer -> Maybe Integer
tModifyIterator uid tLastUpdateId = maybe ifNothing ifJust tLastUpdateId
 where
  ifNothing = Just $ uid + 1
  ifJust offset | uid + 1 > offset = ifNothing
                | otherwise        = tLastUpdateId

tHandleMsg
  :: ( MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     , HasTelegramEnv m
     , HasTelegramMod m
     )
  => TelegramMessage
  -> m ()
tHandleMsg (TelegramMessage uid chatId "/help") = do
  mText <- tEnvHelpMsg
  tSendMsg (TelegramMessage uid chatId mText) False
  tModLastUpdateId $ tModifyIterator uid
tHandleMsg (TelegramMessage uid chatId "/repeat") = do
  rMsg        <- tEnvRepeatMsg
  r           <- tEnvRepeatNumber
  chatsRepeat <- tGetRepeatMap
  let currR = HM.lookupDefault r chatId chatsRepeat
      rnMsg = TelegramMessage uid chatId $ rMsg <> texify currR
  tSendMsg rnMsg True
  tModLastUpdateId $ tModifyIterator uid
tHandleMsg msg@TelegramMessage {..} = do
  r           <- tEnvRepeatNumber
  chatsRepeat <- tGetRepeatMap
  let currR = HM.lookupDefault r tmChatId chatsRepeat
  replicateM_ currR $ tSendMsg msg False
  tModLastUpdateId $ tModifyIterator tmUpdateId

tHandleReaction
  :: ( MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     , HasTelegramEnv m
     , HasTelegramMod m
     )
  => TelegramReaction
  -> m ()
tHandleReaction tr@TelegramReaction {..} = do
  chatsRepeat <- tGetRepeatMap
  let btn = readMaybe trCallbackData :: Maybe Int
  when (isNothing btn) $ throwM $ BadCallbackData trCallbackData
  token <- tEnvToken
  let chatsRepeat' = HM.insert trChatId (fromJust btn) chatsRepeat
      req          = makeAnswerCallbackQuery token tr
  response <- http req
  checkResponseStatus response
  tPutRepeatMap chatsRepeat'
  logChatRepeat (texify trChatId) (T.pack trCallbackData)
  tModLastUpdateId $ tModifyIterator trUpdateId

tSendMsg
  :: (MonadHTTP m, MonadThrow m, MonadLogger m, HasTelegramEnv m)
  => TelegramMessage
  -> Bool
  -> m ()
tSendMsg msg@TelegramMessage {..} hasKeyboard = do
  token <- tEnvToken
  let req = if hasKeyboard
        then makeCallbackQuery token msg
        else makeSendMessage token msg
  response <- http req
  checkResponseStatus response
  logChatMessage (texify tmChatId) tmText
