{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.EchoBot
  ( slackBot
  ) where

import Bot.BotClass
import Bot.EchoBot (EchoBot(..))
import Bot.Exception (checkResponseStatus, throwParseException)
import Control.Monad (replicateM_, when)
import Control.Monad.Catch
import qualified Data.Aeson as JSON (decode)
import qualified Data.ByteString.Lazy as LB (ByteString(..))
import Data.Maybe (isJust, isNothing, listToMaybe)
import qualified Data.Text as T (pack)
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)
import qualified Network.HTTP.Simple as HTTP (getResponseBody)
import Serializer.Slack
import Slack.Models
import Slack.Requests

slackBot ::
     (MonadHTTP m, MonadThrow m, MonadLogger m, HasSlackConst m, HasSlackMod m)
  => EchoBot m SlackMessage SlackReaction
slackBot =
  EchoBot
    { getUpdates = sGetUpdates
    , handleMsg = sHandleMsg
    , handleReaction = sHandleReaction
    }

sGetUpdates ::
     (MonadHTTP m, MonadThrow m, HasSlackConst m, HasSlackMod m)
  => m ([SlackMessage], [SlackReaction])
sGetUpdates = (,) <$> sAcquireMessages <*> sAcquireReactions

sAcquireMessages ::
     (MonadHTTP m, MonadThrow m, HasSlackConst m, HasSlackMod m)
  => m [SlackMessage]
sAcquireMessages = do
  timestamp <- sGetLastTimestamp
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let conHistory = makeConHistory timestamp token channel
  responseHistory <- http conHistory
  checkResponseStatus responseHistory
  let unparsedHistory = HTTP.getResponseBody responseHistory
      parsedHistory = JSON.decode unparsedHistory
  sResponse <-
    maybe
      (throwParseException unparsedHistory >> pure emptySResponse)
      pure
      parsedHistory
  pure $ sResponseToMsgs sResponse

sAcquireReactions ::
     (MonadHTTP m, MonadThrow m, HasSlackConst m, HasSlackMod m)
  => m [SlackReaction]
sAcquireReactions = sGetRepeatTimestamp >>= maybe (pure []) acquire
  where
    acquire repeatTs = do
      token <- sConstToken <$> getSlackConst
      channel <- sConstChannel <$> getSlackConst
      let getReactions = makeGetReactions token channel repeatTs
      responseReactions <- http getReactions
      checkResponseStatus responseReactions
      let unparsedReactions = HTTP.getResponseBody responseReactions
          parsedReactions = JSON.decode unparsedReactions
      sPostResponse <-
        maybe
          (throwParseException unparsedReactions >> pure emptySPostResponse)
          pure
          parsedReactions
      when (hasReactions sPostResponse) $ sPutRepeatTimestamp Nothing
      pure $ sPostResponseToReactions sPostResponse

sHandleMsg ::
     (MonadHTTP m, MonadThrow m, MonadLogger m, HasSlackConst m, HasSlackMod m)
  => SlackMessage
  -> m ()
sHandleMsg (SlackMessage ts "_help") = do
  hMsg <- sConstHelpMsg <$> getSlackConst 
  sSendMsg $ SlackMessage ts hMsg
  sPutLastTimestamp $ Just ts
sHandleMsg (SlackMessage ts "_repeat") = do
  rMsg <- sConstRepeatMsg <$> getSlackConst 
  r <- sGetRepeatNumber
  let rnMsg = SlackMessage ts (rMsg <> textify r)
  unparsed <- sSendMsg rnMsg
  let parsed = JSON.decode unparsed
      repeatTs = sMessageTimestamp <$> (parsed >>= sPostResponseMsg)
  when (isNothing repeatTs) $ throwParseException unparsed
  sPutRepeatTimestamp repeatTs
  sPutLastTimestamp $ Just ts
sHandleMsg msg@SlackMessage {..} = do
  r <- sGetRepeatNumber
  replicateM_ r $ sSendMsg msg
  sPutLastTimestamp $ Just smTimestamp

sHandleReaction ::
     (MonadLogger m, HasSlackConst m, HasSlackMod m) => SlackReaction -> m ()
sHandleReaction SlackReaction {..} = do
  sPutRepeatNumber srRepeatNumber
  chat <- T.pack . sConstChannel <$> getSlackConst
  logChatRepeat chat (textify srRepeatNumber)

sSendMsg ::
     (MonadHTTP m, MonadThrow m, MonadLogger m, HasSlackConst m)
  => SlackMessage
  -> m LB.ByteString
sSendMsg msg@SlackMessage {..} = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let postMessage = makePostMessage token channel msg
  response <- http postMessage
  checkResponseStatus response
  let chat = T.pack channel
  logChatMessage chat smText
  pure $ HTTP.getResponseBody response

hasReactions :: SPostResponse -> Bool
hasReactions sPostResponse = isJust mbReaction
  where
    mbReaction =
      sPostResponseMsg sPostResponse >>= sMessageReactions >>= listToMaybe
