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
     (MonadHTTP m, MonadThrow m, HasSlackConst m)
  => EchoBot m SlackMessage SlackReaction SlackFlag SlackAnticipation SlackIterator SlackRepeatMap
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
sAcquireReactions (Just flag) = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let getReactions = makeGetReactions token channel flag
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
sGetCurrentRepeatNumber r repeatMap _ = fromMaybe r repeatMap

sReplaceMsgText :: T.Text -> SlackMessage -> SlackMessage
sReplaceMsgText text sm = sm {smText = text}

sGetTextualChat ::
     HasSlackConst m => Either SlackReaction SlackMessage -> m T.Text
sGetTextualChat _ = T.pack . sConstChannel <$> getSlackConst

sGetTextualMsg :: SlackMessage -> T.Text
sGetTextualMsg = smText

sSendMsg :: (HasSlackConst m, MonadHTTP m, MonadThrow m) => SlackMessage -> m ()
sSendMsg msg = sSendMsgGetResBody msg >> pure ()

sSendRepeatMsg ::
     (HasSlackConst m, MonadHTTP m, MonadThrow m)
  => SlackMessage
  -> m SlackAnticipation
sSendRepeatMsg = sSendMsgGetResBody

sSendMsgGetResBody ::
     (HasSlackConst m, MonadHTTP m, MonadThrow m)
  => SlackMessage
  -> m SlackAnticipation
sSendMsgGetResBody msg@SlackMessage {..} = do
  token <- sConstToken <$> getSlackConst
  channel <- sConstChannel <$> getSlackConst
  let postMessage = makePostMessage token channel msg
  response <- http postMessage
  checkResponseStatus response
  let chat = T.pack channel
  pure $ HTTP.getResponseBody response

sIteratorTransformation ::
     Either SlackReaction SlackMessage
  -> Maybe SlackIterator
  -> Maybe SlackIterator
sIteratorTransformation (Left _) = id
sIteratorTransformation (Right SlackMessage {..}) = \_ -> Just smTimestamp

sRepeatMapTransformation :: Int -> SlackReaction -> SlackRepeatMap -> Maybe Int
sRepeatMapTransformation repeat _ _ = Just repeat

sParseReaction :: Monad m => SlackReaction -> m (Maybe Int)
sParseReaction SlackReaction {..} = pure slackRepeat
  where
    slackRepeat =
      case srName of
        "one" -> Just 1
        "two" -> Just 2
        "three" -> Just 3
        "four" -> Just 4
        "five" -> Just 5
        _ -> Nothing

sParseAnticipation :: (MonadThrow m) => SlackAnticipation -> m (Maybe SlackFlag)
sParseAnticipation msgResponseBody = do
  let parsed = JSON.decode msgResponseBody
      repeatTs = sMessageTimestamp <$> (parsed >>= sPostResponseMsg)
  when (isNothing repeatTs) $ throwParseException msgResponseBody
  pure repeatTs
