{-# LANGUAGE ScopedTypeVariables #-}

module Bot.EchoBot where

import Bot.BotClass
import Control.Applicative ((<|>))
import Control.Monad
import Data.Maybe
import qualified Data.Text as T (Text(..))
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)

data BotMessage
  = HelpMsg
  | RepeatMsg
  | PlainMsg
  | ReactionMsg
  deriving (Eq, Show)

data EchoBot m msg res rmap = EchoBot
  { getUpdates :: Maybe msg -> Maybe msg -> m [msg]
  , hasFutureMsg :: [msg] -> Bool
  , findLastMsg :: [msg] -> Maybe msg
  , routeMsg :: msg -> BotMessage
  , sendMsg :: msg -> m res
  , parseSendMsgResponse :: res -> m (Maybe msg)
  , putHelpTextInMsg :: T.Text -> msg -> m msg
  , putRepeatTextInMsg :: T.Text -> msg -> m msg
  , repeatMapTransformation :: Int -> msg -> rmap -> m rmap
  , getCurrentRepeatNumber :: Int -> rmap -> msg -> m Int
  , parseToRepeatNumber :: msg -> m (Maybe Int)
  , convertToTextualChat :: msg -> m T.Text
  , convertToTextualMsg :: msg -> m T.Text
  }

goEchoBot ::
     ( MonadDelay m
     , MonadBotConst m
     , MonadLastMsgState m msg
     , MonadFutureMsgState m msg
     , MonadRepeatMapState m rmap
     , MonadLogger m
     )
  => EchoBot m msg res rmap
  -> m ()
goEchoBot bot = forever $ botCycle bot

botCycle ::
     forall m msg res rmap.
     ( MonadDelay m
     , MonadBotConst m
     , MonadLastMsgState m msg
     , MonadFutureMsgState m msg
     , MonadRepeatMapState m rmap
     , MonadLogger m
     )
  => EchoBot m msg res rmap
  -> m ()
botCycle bot = do
  lastMsg <- getLastMsg
  futureMsg <- getFutureMsg
  ms <- getUpdates bot lastMsg futureMsg
  when (hasFutureMsg bot ms) $ putFutureMsg (Nothing :: Maybe msg)
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
      helpText <- helpMsg <$> getBotConst
      hMsg <- putHelpTextInMsg bot helpText msg
      sendMsg bot hMsg
      chat <- convertToTextualChat bot msg
      logChatMessage chat helpText
    processPlainMsg msg = do
      repeat <- defaultRepeatNumber <$> getBotConst
      repeatMap <- getRepeatMap
      currRepeat <- getCurrentRepeatNumber bot repeat repeatMap msg
      replicateM_ currRepeat $ sendMsg bot msg
      chat <- convertToTextualChat bot msg
      text <- convertToTextualMsg bot msg
      logChatMessage chat text
    processRepeatMsg msg = do
      repeat <- defaultRepeatNumber <$> getBotConst
      repeatMap <- getRepeatMap
      repeatText <- repeatMsg <$> getBotConst
      currRepeat <- getCurrentRepeatNumber bot repeat repeatMap msg
      let modRepeatText = repeatText <> textify currRepeat
      rMsg <- putRepeatTextInMsg bot modRepeatText msg
      sendMsg bot rMsg >>= parseSendMsgResponse bot >>= putFutureMsg
      chat <- convertToTextualChat bot msg
      logChatMessage chat modRepeatText
    processReact react = do
      mbRepeat <- parseToRepeatNumber bot react
      maybe (pure ()) (modifyAndLog react) mbRepeat
    modifyAndLog react repeat = do
      getRepeatMap >>= repeatMapTransformation bot repeat react >>= putRepeatMap
      chat <- convertToTextualChat bot react
      logChatRepeat chat $ textify repeat
