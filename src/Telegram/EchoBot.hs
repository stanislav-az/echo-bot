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
  :: (MonadState TelegramEnv m, MonadIO m, MonadThrow m, MonadLogger m)
  => EchoBot m TelegramMessage TelegramReaction
telegramBot = EchoBot { getUpdates     = tGetUpdates
                      , handleMsg      = tHandleMsg
                      , handleReaction = tHandleReaction
                      }

tGetUpdates
  :: (MonadState TelegramEnv m, MonadIO m, MonadThrow m)
  => m ([TelegramMessage], [TelegramReaction])
tGetUpdates = do
  offset <- gets tLastUpdateId
  token  <- gets tToken
  let getUpdates = makeGetUpdates offset token
  response <- httpLBS getUpdates
  checkResponseStatus response
  let unparsed = getResponseBody response
      parsed   = decode unparsed :: Maybe TResponse
  tResponse <- maybe (throwParseException unparsed >> pure emptyTResponse)
                     pure
                     parsed
  let models  = tResponseToModels tResponse offset
      lastUId = tResponseToUpdateId tResponse offset
  modify $ \s -> s { tLastUpdateId = lastUId }
  pure models

tHandleMsg
  :: (MonadState TelegramEnv m, MonadIO m, MonadThrow m, MonadLogger m)
  => TelegramMessage
  -> m ()
tHandleMsg (TelegramMessage chatId "/help") =
  gets tHelpMsg >>= \mText -> tSendMsg (TelegramMessage chatId mText) False
tHandleMsg (TelegramMessage chatId "/repeat") = do
  rMsg        <- gets tRepeatMsg
  r           <- gets tRepeatNumber
  chatsRepeat <- gets tRepeatMap
  let currR = HM.lookupDefault r chatId chatsRepeat
      rnMsg = TelegramMessage chatId $ rMsg <> texify currR
  tSendMsg rnMsg True
tHandleMsg msg@TelegramMessage {..} = do
  r           <- gets tRepeatNumber
  chatsRepeat <- gets tRepeatMap
  let currR = HM.lookupDefault r tmChatId chatsRepeat
  replicateM_ currR $ tSendMsg msg False

tHandleReaction
  :: (MonadState TelegramEnv m, MonadIO m, MonadThrow m, MonadLogger m)
  => TelegramReaction
  -> m ()
tHandleReaction tr@TelegramReaction {..} = do
  chatsRepeat <- gets tRepeatMap
  let btn = readMaybe trCallbackData :: Maybe Int
  when (isNothing btn) $ throwM $ BadCallbackData trCallbackData
  token <- gets tToken
  let chatsRepeat' = HM.insert trChatId (fromJust btn) chatsRepeat
      req          = makeAnswerCallbackQuery token tr
  response <- httpLBS req
  checkResponseStatus response
  logChatRepeat (texify trChatId) (T.pack trCallbackData)
  modify $ \s -> s { tRepeatMap = chatsRepeat' }

tSendMsg
  :: (MonadState TelegramEnv m, MonadIO m, MonadThrow m, MonadLogger m)
  => TelegramMessage
  -> Bool
  -> m ()
tSendMsg msg@TelegramMessage {..} hasKeyboard = do
  token <- gets tToken
  let req = if hasKeyboard
        then makeCallbackQuery token msg
        else makeSendMessage token msg
  response <- httpLBS req
  checkResponseStatus response
  logChatMessage (texify tmChatId) tmText
