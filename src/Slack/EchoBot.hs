{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.EchoBot
  ( slackBot
  ) where

import Bot.BotClass
import Bot.EchoBot (BotMessage(..), EchoBot(..))
import Bot.Exception
  ( checkResponseStatus
  , throwBotLogicMisuse
  , throwParseException
  )
import Control.Monad (replicateM_, unless, when)
import Control.Monad.Catch
import qualified Data.Aeson as JSON (decode)
import qualified Data.ByteString.Lazy as LB (ByteString(..))
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Text as T (Text(..), pack)
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)
import qualified Network.HTTP.Simple as HTTP (Response(..), getResponseBody)
import qualified Safe (lastMay)
import Serializer.Slack
import Slack.Models
import Slack.Requests

slackBot ::
     (MonadHTTP m, MonadThrow m, MonadSlackConst m, MonadTimestampState m)
  => EchoBot m SlackMessage
slackBot =
  EchoBot
    { getUpdates = sGetUpdates
    , findLastMsg = sFindLastMsg
    , routeMsg = sRouteMsg
    , sendMsg = sSendMsg
    , putHelpTextInMsg = sPutHelpTextInMsg
    , putRepeatTextInMsg = sPutRepeatTextInMsg
    , parseToRepeatNumber = sParseToRepeatNumber
    , convertToTextualChat = sConvertToTextualChat
    , convertToTextualMsg = sConvertToTextualMsg
    }

sFindLastMsg :: [SlackMessage] -> Maybe SlackMessage
sFindLastMsg = Safe.lastMay . filter sIsMessage

sGetUpdates ::
     (MonadSlackConst m, MonadHTTP m, MonadThrow m, MonadTimestampState m)
  => Maybe SlackMessage
  -> m [SlackMessage]
sGetUpdates lastMsg = (++) <$> sAcquireMessages lastMsg <*> sAcquireReactions

sAcquireMessages ::
     (MonadSlackConst m, MonadHTTP m, MonadThrow m)
  => Maybe SlackMessage
  -> m [SlackMessage]
sAcquireMessages (Just Reaction {..}) =
  throwBotLogicMisuse "This SlackMessage has no timestamp"
sAcquireMessages mbMsg = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let conHistory = makeConHistory token channel $ fmap smTimestamp mbMsg
  responseHistory <- http conHistory
  checkResponseStatus responseHistory
  let unparsedHistory = HTTP.getResponseBody responseHistory
      parsedHistory = JSON.decode unparsedHistory
  sResponse <- maybe (throwParseException unparsedHistory) pure parsedHistory
  pure $ sResponseToMsgs sResponse

sAcquireReactions ::
     (MonadSlackConst m, MonadHTTP m, MonadThrow m, MonadTimestampState m)
  => m [SlackMessage]
sAcquireReactions = getTimestamp >>= maybe (pure []) acquire
  where
    acquire timestamp = do
      token <- sConstToken <$> getSlackConst
      channel <- sConstChannel <$> getSlackConst
      let getReactions = makeGetReactions token channel timestamp
      responseReactions <- http getReactions
      checkResponseStatus responseReactions
      let unparsedReactions = HTTP.getResponseBody responseReactions
          parsedReactions = JSON.decode unparsedReactions
      sPostResponse <-
        maybe (throwParseException unparsedReactions) pure parsedReactions
      let rs = sPostResponseToReactions sPostResponse
      unless (null rs) $ putTimestamp Nothing
      pure rs

sRouteMsg :: SlackMessage -> BotMessage
sRouteMsg Message {..} =
  case smText of
    "_help" -> HelpMsg
    "_repeat" -> RepeatMsg
    _ -> PlainMsg
sRouteMsg _ = ReactionMsg

sPutHelpTextInMsg :: MonadThrow m => T.Text -> SlackMessage -> m SlackMessage
sPutHelpTextInMsg text sm@Message {..} = pure sm {smText = text}
sPutHelpTextInMsg _ _ = throwBotLogicMisuse "This SlackMessage has no text"

sPutRepeatTextInMsg :: MonadThrow m => T.Text -> SlackMessage -> m SlackMessage
sPutRepeatTextInMsg text sm@Message {..} =
  pure sm {smText = text, smIsRepeatMsg = True}
sPutRepeatTextInMsg _ _ = throwBotLogicMisuse "This SlackMessage has no text"

sConvertToTextualChat :: MonadSlackConst m => SlackMessage -> m T.Text
sConvertToTextualChat _ = T.pack . sConstChannel <$> getSlackConst

sConvertToTextualMsg :: MonadThrow m => SlackMessage -> m T.Text
sConvertToTextualMsg Message {..} = pure smText
sConvertToTextualMsg _ = throwBotLogicMisuse "This SlackMessage has no text"

sSendMsg ::
     (MonadSlackConst m, MonadHTTP m, MonadThrow m, MonadTimestampState m)
  => SlackMessage
  -> m ()
sSendMsg Message {..} = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let postMessage = makePostMessage token channel smText
  response <- http postMessage
  checkResponseStatus response
  let chat = T.pack channel
  when smIsRepeatMsg $ sParseSendMsgResponse response
sSendMsg _ = throwBotLogicMisuse "Could not send this SlackMessage"

sParseToRepeatNumber :: MonadThrow m => SlackMessage -> m (Maybe Int)
sParseToRepeatNumber Reaction {..} = pure slackRepeat
  where
    slackRepeat =
      case srName of
        "one" -> Just 1
        "two" -> Just 2
        "three" -> Just 3
        "four" -> Just 4
        "five" -> Just 5
        _ -> Nothing
sParseToRepeatNumber _ = throwBotLogicMisuse "Could not parse this SlackMessage"

sParseSendMsgResponse ::
     (MonadThrow m, MonadTimestampState m)
  => HTTP.Response LB.ByteString
  -> m ()
sParseSendMsgResponse response = do
  let msgResponseBody = HTTP.getResponseBody response
      parsed = JSON.decode msgResponseBody
      timestamp = parsed >>= sPostResponseToTimestamp
  when (isNothing timestamp) $ throwParseException msgResponseBody
  putTimestamp timestamp
