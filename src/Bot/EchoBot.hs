{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bot.EchoBot where

import Bot.BotClass
import Control.Applicative ((<|>))
import Control.Monad
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T (Text(..))
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)

data BotMessage
  = HelpMsg
  | RepeatMsg
  | PlainMsg
  | ReactionMsg
  deriving (Eq, Show)

data EchoBot m msg map = EchoBot
  { getUpdates :: Maybe msg -> m [msg]
  , findLastMsg :: [msg] -> Maybe msg
  , routeMsg :: msg -> BotMessage
  , sendMsg :: msg -> m ()
  , putHelpTextInMsg :: T.Text -> msg -> m msg
  , putRepeatTextInMsg :: T.Text -> msg -> m msg
  , parseToRepeatNumber :: msg -> m (Maybe Int)
  , convertToTextualChat :: msg -> m T.Text
  , convertToTextualMsg :: msg -> m T.Text
  }

goEchoBot ::
     ( MonadDelay m
     , MonadBotStaticOptions m
     , MonadLastMsgState m msg
     , MonadRepeatMapState m map
     , MonadLogger m
     )
  => EchoBot m msg (map T.Text Int)
  -> m ()
goEchoBot bot = forever $ botCycle bot

botCycle ::
     forall m msg map.
     ( MonadDelay m
     , MonadBotStaticOptions m
     , MonadLastMsgState m msg
     , MonadRepeatMapState m map
     , MonadLogger m
     )
  => EchoBot m msg (map T.Text Int)
  -> m ()
botCycle bot = do
  lastMsg <- getLastMsg
  ms <- getUpdates bot lastMsg
  let lastMsg' = findLastMsg bot ms <|> lastMsg
  putLastMsg lastMsg'
  forM_ ms processMsg
  delay 500000
  where
    processMsg msg =
      case routeMsg bot msg of
        HelpMsg -> processHelpMsg msg
        RepeatMsg -> processRepeatMsg msg
        PlainMsg -> processPlainMsg msg
        ReactionMsg -> processReact msg
    processHelpMsg msg = do
      helpText <- helpMsg <$> getBotStaticOptions
      hMsg <- putHelpTextInMsg bot helpText msg
      sendMsg bot hMsg
      chat <- convertToTextualChat bot msg
      logChatMessage chat helpText
    processPlainMsg msg = do
      repeat <- defaultRepeatNumber <$> getBotStaticOptions
      chat <- convertToTextualChat bot msg
      currRepeat <- lookupRepeatDefault @m @map Proxy repeat chat
      replicateM_ currRepeat $ sendMsg bot msg
      text <- convertToTextualMsg bot msg
      logChatMessage chat text
    processRepeatMsg msg = do
      repeat <- defaultRepeatNumber <$> getBotStaticOptions
      repeatText <- repeatMsg <$> getBotStaticOptions
      chat <- convertToTextualChat bot msg
      currRepeat <- lookupRepeatDefault @m @map Proxy repeat chat
      let modRepeatText = repeatText <> textify currRepeat
      rMsg <- putRepeatTextInMsg bot modRepeatText msg
      sendMsg bot rMsg
      logChatMessage chat modRepeatText
    processReact react = do
      mbRepeat <- parseToRepeatNumber bot react
      maybe (pure ()) (modifyAndLog react) mbRepeat
    modifyAndLog react repeat = do
      chat <- convertToTextualChat bot react
      insertRepeat @m @map Proxy chat repeat
      logChatRepeat chat $ textify repeat
