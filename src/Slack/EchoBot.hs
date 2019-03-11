{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.EchoBot
  ( slackBot
  ) where

import Bot.BotClass
import Bot.EchoBot (BotMessage(..), EchoBot(..))
import Bot.Exception (checkResponseStatus, throwParseException)
import Control.Monad (replicateM_, when)
import Control.Monad.Catch
import qualified Data.Aeson as JSON (decode)
import qualified Data.ByteString.Lazy as LB (ByteString(..))
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Text as T (Text(..), pack)
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)
import qualified Network.HTTP.Simple as HTTP (getResponseBody)
import Serializer.Slack
import Slack.Models
import Slack.Requests

slackBot ::
     (MonadHTTP m, MonadThrow m, MonadLogger m, HasSlackConst m)
  => EchoBot m SlackMessage SlackReaction SlackFlag SlackAnticipation SlackRepeat SlackIterator SlackRepeatMap
slackBot =
  EchoBot
    { getUpdates = sGetUpdates
    , routeMsg = sRouteMsg
    , sendMsg = sSendMsg
    , sendRepeatMsg = sSendRepeatMsg
    , iteratorTransformation = sIteratorTransformation
    , repeatMapTransformation = sRepeatMapTransformation
    , parseReaction = sParseReaction
    , parseAnticipation = sParseAnticipation
    , getCurrentRepeatNumber = sGetCurrentRepeatNumber
    , replaceMsgText = sReplaceMsgText
    , getTextualChat = sGetTextualChat
    , getTextualRepeat = sGetTextualRepeat
    , getTextualMsg = sGetTextualMsg
    }

sGetUpdates ::
     (HasSlackConst m, MonadHTTP m, MonadThrow m)
  => Maybe SlackFlag
  -> Maybe SlackIterator
  -> m ([SlackMessage], [SlackReaction])
sGetUpdates flag timestamp =
  (,) <$> sAcquireMessages timestamp <*> sAcquireReactions flag

sAcquireMessages ::
     (HasSlackConst m, MonadHTTP m, MonadThrow m)
  => Maybe SlackIterator
  -> m [SlackMessage]
sAcquireMessages timestamp = do
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
     (HasSlackConst m, MonadHTTP m, MonadThrow m)
  => Maybe SlackFlag
  -> m [SlackReaction]
sAcquireReactions Nothing = pure []
sAcquireReactions (Just sf) = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let getReactions = makeGetReactions token channel sf
  responseReactions <- http getReactions
  checkResponseStatus responseReactions
  let unparsedReactions = HTTP.getResponseBody responseReactions
      parsedReactions = JSON.decode unparsedReactions
  sPostResponse <-
    maybe
      (throwParseException unparsedReactions >> pure emptySPostResponse)
      pure
      parsedReactions
  pure $ sPostResponseToReactions sPostResponse

sRouteMsg :: SlackMessage -> BotMessage
sRouteMsg SlackMessage {..} =
  case smText of
    "_help" -> HelpMsg
    "_repeat" -> RepeatMsg
    _ -> Msg

sGetCurrentRepeatNumber :: Int -> SlackRepeatMap -> SlackMessage -> Int
sGetCurrentRepeatNumber r SlackRepeatMap {..} _ =
  fromMaybe r thisChatRepeatNumber

sReplaceMsgText :: T.Text -> SlackMessage -> SlackMessage
sReplaceMsgText text SlackMessage {..} = SlackMessage smTimestamp text

sGetTextualChat ::
     HasSlackConst m => Either SlackReaction SlackMessage -> m T.Text
sGetTextualChat _ = T.pack . sConstChannel <$> getSlackConst

sGetTextualRepeat :: SlackRepeat -> T.Text
sGetTextualRepeat SlackRepeat {..} = textify sRepeatNumber

sGetTextualMsg :: SlackMessage -> T.Text
sGetTextualMsg = smText

sSendMsg ::
     (HasSlackConst m, MonadHTTP m, MonadThrow m, MonadLogger m)
  => SlackMessage
  -> m ()
sSendMsg msg = sSendMsgGetResBody msg >> pure ()

sSendRepeatMsg ::
     (HasSlackConst m, MonadHTTP m, MonadThrow m, MonadLogger m)
  => SlackMessage
  -> m SlackAnticipation
sSendRepeatMsg msg = SlackAnticipation <$> sSendMsgGetResBody msg

sSendMsgGetResBody ::
     (HasSlackConst m, MonadHTTP m, MonadThrow m, MonadLogger m)
  => SlackMessage
  -> m LB.ByteString
sSendMsgGetResBody msg@SlackMessage {..} = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let postMessage = makePostMessage token channel msg
  response <- http postMessage
  checkResponseStatus response
  let chat = T.pack channel
  logChatMessage chat smText
  pure $ HTTP.getResponseBody response

sIteratorTransformation ::
     Either SlackReaction SlackMessage
  -> Maybe SlackIterator
  -> Maybe SlackIterator
sIteratorTransformation (Left _) = id
sIteratorTransformation (Right SlackMessage {..}) =
  \_ -> Just $ SlackIterator smTimestamp

sRepeatMapTransformation :: SlackRepeat -> SlackRepeatMap -> SlackRepeatMap
sRepeatMapTransformation SlackRepeat {..} _ =
  SlackRepeatMap $ Just sRepeatNumber

sParseReaction :: Monad m => SlackReaction -> m (Maybe SlackRepeat)
sParseReaction SlackReaction {..} = pure slackRepeat
  where
    slackRepeat =
      case srName of
        "one" -> Just $ SlackRepeat 1
        "two" -> Just $ SlackRepeat 1
        "three" -> Just $ SlackRepeat 1
        "four" -> Just $ SlackRepeat 1
        "five" -> Just $ SlackRepeat 1
        _ -> Nothing

sParseAnticipation :: (MonadThrow m) => SlackAnticipation -> m (Maybe SlackFlag)
sParseAnticipation SlackAnticipation {..} = do
  let parsed = JSON.decode saPostMsgResponseBody
      repeatTs = sMessageTimestamp <$> (parsed >>= sPostResponseMsg)
  when (isNothing repeatTs) $ throwParseException saPostMsgResponseBody
  pure $ fmap SlackFlag repeatTs
