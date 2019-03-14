{-# LANGUAGE ScopedTypeVariables #-}

module Bot.EchoBot where

import Bot.BotClass
import Control.Monad
import Data.Maybe
import qualified Data.Text as T (Text(..))
import Ext.Data.Text (textify)
import Logging (logChatMessage, logChatRepeat)

data BotMessage
  = HelpMsg
  | RepeatMsg
  | Msg
  deriving (Eq, Show)

data EchoBot m msg react flag anti iter rmap = EchoBot
  { getUpdates :: Maybe flag -> Maybe iter -> m ([msg], [react])
  , routeMsg :: msg -> BotMessage
  , sendMsg :: msg -> m ()
  , sendRepeatMsg :: msg -> m anti
  , iteratorTransformation :: Either react msg -> Maybe iter -> Maybe iter
  , repeatMapTransformation :: Int -> react -> rmap -> rmap
  , parseReaction :: react -> m (Maybe Int)
  , parseAnticipation :: anti -> m (Maybe flag)
  , getCurrentRepeatNumber :: Int -> rmap -> msg -> Int
  , replaceMsgText :: T.Text -> msg -> msg
  , getTextualChat :: Either react msg -> m T.Text
  , getTextualMsg :: msg -> T.Text
  }

goEchoBot ::
     ( MonadDelay m
     , MonadFlagState m flag
     , MonadIterState m iter
     , MonadRepeatState m rmap
     , HasBotConst m
     , MonadLogger m
     )
  => EchoBot m msg react flag anti iter rmap
  -> m ()
goEchoBot bot = forever $ botCycle bot

botCycle ::
     forall m msg react flag anti rep iter rmap.
     ( MonadDelay m
     , MonadFlagState m flag
     , MonadIterState m iter
     , MonadRepeatState m rmap
     , HasBotConst m
     , MonadLogger m
     )
  => EchoBot m msg react flag anti iter rmap
  -> m ()
botCycle bot = do
  flag <- getFlag
  iter <- getIterator
  (ms, rs) <- getUpdates bot flag iter
  unless (null rs) $ putFlag (Nothing :: Maybe flag)
  forM_ rs processReact
  forM_ ms $ \msg -> do
    processMsg msg
    modifyIterator $ iteratorTransformation bot (Right msg)
  delay 500000
  where
    processMsg msg =
      case routeMsg bot msg of
        HelpMsg -> processHelpMsg msg
        RepeatMsg -> processRepeatMsg msg
        Msg -> processPlainMsg msg
    processHelpMsg msg = do
      helpText <- helpMsg <$> getBotConst
      let hMsg = replaceMsgText bot helpText msg
      sendMsg bot hMsg
      chat <- getTextualChat bot $ Right hMsg
      logChatMessage chat helpText
    processPlainMsg msg = do
      repeat <- defaultRepeatNumber <$> getBotConst
      repeatMap <- getRepeatMap
      let currRepeat = getCurrentRepeatNumber bot repeat repeatMap msg
          text = getTextualMsg bot msg
      replicateM_ currRepeat $ sendMsg bot msg
      chat <- getTextualChat bot $ Right msg
      logChatMessage chat text
    processRepeatMsg msg = do
      repeat <- defaultRepeatNumber <$> getBotConst
      repeatMap <- getRepeatMap
      repeatText <- repeatMsg <$> getBotConst
      let currRepeat = getCurrentRepeatNumber bot repeat repeatMap msg
          modRepeatText = repeatText <> textify currRepeat
          rMsg = replaceMsgText bot modRepeatText msg
      sendRepeatMsg bot rMsg >>= parseAnticipation bot >>= putFlag
      chat <- getTextualChat bot $ Right rMsg
      logChatMessage chat modRepeatText
    processReact react = do
      mbRepeat <- parseReaction bot react
      maybe (pure ()) (modifyAndLog react) mbRepeat
      modifyIterator $ iteratorTransformation bot (Left react)
    modifyAndLog react repeat = do
      modifyRepeatMap $ repeatMapTransformation bot repeat react
      chat <- getTextualChat bot $ Left react
      logChatRepeat chat $ textify repeat
