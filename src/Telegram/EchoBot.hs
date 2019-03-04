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
  :: ( MonadState TelegramEnv m
     , MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     
     )
  => EchoBot m TelegramMessage TelegramReaction
telegramBot = EchoBot { getUpdates     = tGetUpdates
                      , handleMsg      = tHandleMsg
                      , handleReaction = tHandleReaction
                      }

tGetUpdates
  :: (MonadState TelegramEnv m, MonadHTTP m, MonadThrow m)
  => m ([TelegramMessage], [TelegramReaction])
tGetUpdates = do
  offset <- gets tLastUpdateId
  token  <- gets tToken
  let getUpdates = makeGetUpdates offset token
  response <- http getUpdates
  checkResponseStatus response
  let unparsed = getResponseBody response
      parsed   = decode unparsed :: Maybe TResponse
  tResponse <- maybe (throwParseException unparsed >> pure emptyTResponse)
                     pure
                     parsed
  pure $ tResponseToModels tResponse

tModifyIterator :: (MonadState TelegramEnv m) => Integer -> TelegramEnv -> m ()
tModifyIterator uid env@TelegramEnv {..} = maybe ifNothing ifJust tLastUpdateId
 where
  ifNothing = put env { tLastUpdateId = Just $ uid + 1 }
  ifJust offset | uid + 1 > offset = ifNothing
                | otherwise        = pure ()

tHandleMsg
  :: ( MonadState TelegramEnv m
     , MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     
     )
  => TelegramMessage
  -> m ()
tHandleMsg (TelegramMessage uid chatId "/help") = do
  mText <- gets tHelpMsg
  tSendMsg (TelegramMessage uid chatId mText) False
  get >>= tModifyIterator uid
tHandleMsg (TelegramMessage uid chatId "/repeat") = do
  rMsg        <- gets tRepeatMsg
  r           <- gets tRepeatNumber
  chatsRepeat <- gets tRepeatMap
  let currR = HM.lookupDefault r chatId chatsRepeat
      rnMsg = TelegramMessage uid chatId $ rMsg <> texify currR
  tSendMsg rnMsg True
  get >>= tModifyIterator uid
tHandleMsg msg@TelegramMessage {..} = do
  r           <- gets tRepeatNumber
  chatsRepeat <- gets tRepeatMap
  let currR = HM.lookupDefault r tmChatId chatsRepeat
  replicateM_ currR $ tSendMsg msg False
  get >>= tModifyIterator tmUpdateId

tHandleReaction
  :: (MonadState TelegramEnv m, MonadHTTP m, MonadThrow m, MonadLogger m)
  => TelegramReaction
  -> m ()
tHandleReaction tr@TelegramReaction {..} = do
  chatsRepeat <- gets tRepeatMap
  let btn = readMaybe trCallbackData :: Maybe Int
  when (isNothing btn) $ throwM $ BadCallbackData trCallbackData
  token <- gets tToken
  let chatsRepeat' = HM.insert trChatId (fromJust btn) chatsRepeat
      req          = makeAnswerCallbackQuery token tr
  response <- http req
  checkResponseStatus response
  logChatRepeat (texify trChatId) (T.pack trCallbackData)
  modify $ \s -> s { tRepeatMap = chatsRepeat' }
  get >>= tModifyIterator trUpdateId

tSendMsg
  :: ( MonadState TelegramEnv m
     , MonadHTTP m
     , MonadThrow m
     , MonadLogger m
     
     )
  => TelegramMessage
  -> Bool
  -> m ()
tSendMsg msg@TelegramMessage {..} hasKeyboard = do
  token <- gets tToken
  let req = if hasKeyboard
        then makeCallbackQuery token msg
        else makeSendMessage token msg
  response <- http req
  checkResponseStatus response
  logChatMessage (texify tmChatId) tmText
