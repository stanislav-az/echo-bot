{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.EchoBot
  ( slackBot
  )
where

import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as LB
import           Bot.EchoBot
import           Bot.Exception
import           Control.Monad.Catch
import           Slack.Requests
import           Slack.Models
import           Control.Monad.State
import           Bot.BotClass
import           Bot.BotMonad
import           Network.HTTP.Simple
import           Serializer.Slack
import           Data.Aeson
import           Data.Maybe
import           Helpers
import           Logging

slackBot
  :: (MonadHTTP m, MonadThrow m, MonadLogger m, HasSlackEnv m, HasSlackMod m)
  => EchoBot m SlackMessage SlackReaction
slackBot = EchoBot { getUpdates     = sGetUpdates
                   , handleMsg      = sHandleMsg
                   , handleReaction = sHandleReaction
                   }

sGetUpdates
  :: (MonadHTTP m, MonadThrow m, HasSlackEnv m, HasSlackMod m)
  => m ([SlackMessage], [SlackReaction])
sGetUpdates =
  (,)
    <$> sAcquireMessages
    <*> (sGetRepeatTimestamp >>= maybe (pure []) sAcquireReactions)

sAcquireMessages
  :: (MonadHTTP m, MonadThrow m, HasSlackEnv m, HasSlackMod m)
  => m [SlackMessage]
sAcquireMessages = do
  timestamp <- sGetLastTimestamp
  token     <- sEnvToken
  channel   <- sEnvChannel
  let conHistory = makeConHistory timestamp token channel
  responseHistory <- http conHistory
  checkResponseStatus responseHistory
  let unparsedHistory = getResponseBody responseHistory
      parsedHistory   = decode unparsedHistory :: Maybe SResponse
  sResponse <- maybe
    (throwParseException unparsedHistory >> pure emptySResponse)
    pure
    parsedHistory
  pure $ sResponseToMsgs sResponse

sAcquireReactions
  :: (MonadHTTP m, MonadThrow m, HasSlackEnv m, HasSlackMod m)
  => String
  -> m [SlackReaction]
sAcquireReactions repeatTs = do
  token   <- sEnvToken
  channel <- sEnvChannel
  let getReactions = makeGetReactions token channel repeatTs
  responseReactions <- http getReactions
  checkResponseStatus responseReactions
  let unparsedReactions = getResponseBody responseReactions
      parsedReactions   = decode unparsedReactions :: Maybe SPostResponse
  sPostResponse <- maybe
    (throwParseException unparsedReactions >> pure emptySPostResponse)
    pure
    parsedReactions
  when (hasReactions sPostResponse) $ sPutRepeatTimestamp Nothing
  pure $ sPostResponseToReactions sPostResponse

sHandleMsg
  :: (MonadHTTP m, MonadThrow m, MonadLogger m, HasSlackEnv m, HasSlackMod m)
  => SlackMessage
  -> m ()
sHandleMsg (SlackMessage ts "_help") = do
  hMsg <- sEnvHelpMsg
  sSendMsg $ SlackMessage ts hMsg
  sPutLastTimestamp $ Just ts
sHandleMsg (SlackMessage ts "_repeat") = do
  rMsg <- sEnvRepeatMsg
  r    <- sGetRepeatNumber
  let rnMsg = SlackMessage ts (rMsg <> texify r)
  unparsed <- sSendMsg rnMsg
  let parsed   = decode unparsed :: Maybe SPostResponse
      repeatTs = sMessageTimestamp <$> (parsed >>= sPostResponseMsg)
  when (isNothing repeatTs) $ throwParseException unparsed
  sPutRepeatTimestamp repeatTs
  sPutLastTimestamp $ Just ts
sHandleMsg msg@SlackMessage {..} = do
  r <- sGetRepeatNumber
  replicateM_ r $ sSendMsg msg
  sPutLastTimestamp $ Just smTimestamp

sHandleReaction
  :: (MonadLogger m, HasSlackEnv m, HasSlackMod m) => SlackReaction -> m ()
sHandleReaction SlackReaction {..} = do
  sPutRepeatNumber srRepeatNumber
  chat <- T.pack <$> sEnvChannel
  logChatRepeat chat (texify srRepeatNumber)

sSendMsg
  :: (MonadHTTP m, MonadThrow m, MonadLogger m, HasSlackEnv m)
  => SlackMessage
  -> m LB.ByteString
sSendMsg msg@SlackMessage {..} = do
  token   <- sEnvToken
  channel <- sEnvChannel
  let postMessage = makePostMessage token channel msg
  response <- http postMessage
  checkResponseStatus response
  let chat = T.pack channel
  logChatMessage chat smText
  pure $ getResponseBody response

hasReactions :: SPostResponse -> Bool
hasReactions sPostResponse = isJust mbReaction
 where
  mbReaction =
    sPostResponseMsg sPostResponse >>= sMessageReactions >>= listToMaybe
