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
import Control.Monad (replicateM_, when)
import Control.Monad.Catch
import qualified Data.Aeson as JSON (decode)
import qualified Data.ByteString.Lazy as LB (ByteString(..))
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Text as T (Text(..), pack)
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)
import qualified Network.HTTP.Simple as HTTP (getResponseBody)
import qualified Safe (lastMay)
import Serializer.Slack
import Slack.Models
import Slack.Requests

slackBot ::
     (MonadHTTP m, MonadThrow m, MonadSlackConst m)
  => EchoBot m SlackMessage SlackResponse SlackRepeatMap
slackBot =
  EchoBot
    { getUpdates = sGetUpdates
    , hasFutureMsg = sHasFutureMsg
    , findLastMsg = sFindLastMsg
    , routeMsg = sRouteMsg
    , sendMsg = sSendMsg
    , parseSendMsgResponse = sParseSendMsgResponse
    , putHelpTextInMsg = sReplaceMsgText
    , putRepeatTextInMsg = sReplaceMsgText
    , repeatMapTransformation = sRepeatMapTransformation
    , getCurrentRepeatNumber = sGetCurrentRepeatNumber
    , parseToRepeatNumber = sParseToRepeatNumber
    , convertToTextualChat = sConvertToTextualChat
    , convertToTextualMsg = sConvertToTextualMsg
    }

sHasFutureMsg :: [SlackMessage] -> Bool
sHasFutureMsg = any sIsReaction

sFindLastMsg :: [SlackMessage] -> Maybe SlackMessage
sFindLastMsg = Safe.lastMay . filter sIsMessage

sGetUpdates ::
     (MonadSlackConst m, MonadHTTP m, MonadThrow m)
  => Maybe SlackMessage
  -> Maybe SlackMessage
  -> m [SlackMessage]
sGetUpdates lastMsg futureMsg =
  (++) <$> sAcquireMessages lastMsg <*> sAcquireReactions futureMsg

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
     (MonadSlackConst m, MonadHTTP m, MonadThrow m)
  => Maybe SlackMessage
  -> m [SlackMessage]
sAcquireReactions Nothing = pure []
sAcquireReactions (Just Message {..}) = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let getReactions = makeGetReactions token channel smTimestamp
  responseReactions <- http getReactions
  checkResponseStatus responseReactions
  let unparsedReactions = HTTP.getResponseBody responseReactions
      parsedReactions = JSON.decode unparsedReactions
  sPostResponse <-
    maybe (throwParseException unparsedReactions) pure parsedReactions
  pure $ sPostResponseToReactions sPostResponse
sAcquireReactions _ = throwBotLogicMisuse "This SlackMessage has no timestamp"

sRouteMsg :: SlackMessage -> BotMessage
sRouteMsg Message {..} =
  case smText of
    "_help" -> HelpMsg
    "_repeat" -> RepeatMsg
    _ -> PlainMsg
sRouteMsg _ = ReactionMsg

sGetCurrentRepeatNumber ::
     MonadThrow m => Int -> Maybe Int -> SlackMessage -> m Int
sGetCurrentRepeatNumber r repeatMap Message {..} = pure $ fromMaybe r repeatMap
sGetCurrentRepeatNumber _ _ _ =
  throwBotLogicMisuse "This SlackMessage has reactions"

sReplaceMsgText :: MonadThrow m => T.Text -> SlackMessage -> m SlackMessage
sReplaceMsgText text sm@Message {..} = pure sm {smText = text}
sReplaceMsgText _ _ = throwBotLogicMisuse "This SlackMessage has no text"

sConvertToTextualChat :: MonadSlackConst m => SlackMessage -> m T.Text
sConvertToTextualChat _ = T.pack . sConstChannel <$> getSlackConst

sConvertToTextualMsg :: MonadThrow m => SlackMessage -> m T.Text
sConvertToTextualMsg Message {..} = pure smText
sConvertToTextualMsg _ = throwBotLogicMisuse "This SlackMessage has no text"

sSendMsg ::
     (MonadSlackConst m, MonadHTTP m, MonadThrow m)
  => SlackMessage
  -> m SlackResponse
sSendMsg Message {..} = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let postMessage = makePostMessage token channel smText
  response <- http postMessage
  checkResponseStatus response
  let chat = T.pack channel
  pure $ HTTP.getResponseBody response
sSendMsg _ = throwBotLogicMisuse "Could not send this SlackMessage"

sRepeatMapTransformation ::
     MonadThrow m => Int -> SlackMessage -> SlackRepeatMap -> m SlackRepeatMap
sRepeatMapTransformation repeat Reaction {..} _ = pure $ Just repeat
sRepeatMapTransformation _ _ _ =
  throwBotLogicMisuse "This SlackMessage has text"

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

sParseSendMsgResponse :: MonadThrow m => SlackResponse -> m (Maybe SlackMessage)
sParseSendMsgResponse msgResponseBody = do
  let parsed = JSON.decode msgResponseBody
      msg = parsed >>= sPostResponseToMsg
  when (isNothing msg) $ throwParseException msgResponseBody
  pure msg
