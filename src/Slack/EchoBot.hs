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
  :: (MonadState SlackEnv m, MonadIO m, MonadThrow m, MonadLogger m)
  => EchoBot m SlackMessage SlackReaction
slackBot = EchoBot { getUpdates     = sGetUpdates
                   , handleMsg      = sHandleMsg
                   , handleReaction = sHandleReaction
                   }

sGetUpdates
  :: (MonadState SlackEnv m, MonadIO m, MonadThrow m)
  => m ([SlackMessage], [SlackReaction])
sGetUpdates =
  (,)
    <$> sAcquireMessages
    <*> (gets sRepeatTimestamp >>= maybe (pure []) sAcquireReactions)

sAcquireMessages
  :: (MonadState SlackEnv m, MonadIO m, MonadThrow m) => m [SlackMessage]
sAcquireMessages = do
  timestamp <- gets sLastTimestamp
  token     <- gets sToken
  channel   <- gets sChannel
  let conHistory = makeConHistory timestamp token channel
  responseHistory <- httpLBS conHistory
  checkResponseStatus responseHistory
  let unparsedHistory = getResponseBody responseHistory
      parsedHistory   = decode unparsedHistory :: Maybe SResponse
  sResponse <- maybe
    (throwParseException unparsedHistory >> pure emptySResponse)
    pure
    parsedHistory
  let ms     = sResponseToMsgs sResponse
      lastTs = sResponseToTimestamp sResponse timestamp
  modify $ \s -> s { sLastTimestamp = lastTs }
  pure ms

sAcquireReactions
  :: (MonadState SlackEnv m, MonadIO m, MonadThrow m)
  => String
  -> m [SlackReaction]
sAcquireReactions repeatTs = do
  token   <- gets sToken
  channel <- gets sChannel
  let getReactions = makeGetReactions token channel repeatTs
  responseReactions <- httpLBS getReactions
  checkResponseStatus responseReactions
  let unparsedReactions = getResponseBody responseReactions
      parsedReactions   = decode unparsedReactions :: Maybe SPostResponse
  sPostResponse <- maybe
    (throwParseException unparsedReactions >> pure emptySPostResponse)
    pure
    parsedReactions
  pure $ sPostResponseToReactions sPostResponse

sHandleMsg
  :: (MonadState SlackEnv m, MonadIO m, MonadThrow m, MonadLogger m)
  => SlackMessage
  -> m ()
sHandleMsg (SlackMessage "_help"  ) = gets sHelpMsg >>= sSendMsg >> pure ()
sHandleMsg (SlackMessage "_repeat") = do
  rMsg <- gets sRepeatMsg
  r    <- gets sRepeatNumber
  let rnMsg = SlackMessage $ (smText rMsg) <> (texify r)
  unparsed <- sSendMsg rnMsg
  let parsed   = decode unparsed :: Maybe SPostResponse
      repeatTs = sMessageTimestamp <$> (parsed >>= sPostResponseMsg)
  when (isNothing repeatTs) $ throwParseException unparsed
  modify $ \s -> s { sRepeatTimestamp = repeatTs }
sHandleMsg msg = gets sRepeatNumber >>= (\r -> replicateM_ r $ sSendMsg msg)

sHandleReaction
  :: (MonadState SlackEnv m, MonadLogger m) => SlackReaction -> m ()
sHandleReaction SlackReaction {..} = do
  modify
    $ \s -> s { sRepeatNumber = srRepeatNumber, sRepeatTimestamp = Nothing }
  chat <- T.pack <$> gets sChannel
  logChatRepeat chat (texify srRepeatNumber)

sSendMsg
  :: (MonadState SlackEnv m, MonadIO m, MonadThrow m, MonadLogger m)
  => SlackMessage
  -> m LB.ByteString
sSendMsg msg@SlackMessage {..} = do
  token   <- gets sToken
  channel <- gets sChannel
  let postMessage = makePostMessage token channel msg
  response <- httpLBS postMessage
  checkResponseStatus response
  let chat = T.pack channel
  logChatMessage chat smText
  pure $ getResponseBody response

