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

data EchoBot m msg react flag anti rep iter rmap = EchoBot
  { getUpdates :: Maybe flag -> Maybe iter -> m ([msg], [react])
  , routeMsg :: msg -> BotMessage
  , sendMsg :: msg -> m ()
  , sendRepeatMsg :: msg -> m anti
  , iteratorTransformation :: Either react msg -> Maybe iter -> Maybe iter
  , repeatMapTransformation :: rep -> rmap -> rmap
  , parseReaction :: react -> m (Maybe rep)
  , parseAnticipation :: anti -> m (Maybe flag)
  , getCurrentRepeatNumber :: Int -> rmap -> msg -> Int
  , replaceMsgText :: T.Text -> msg -> msg
  , getTextualChat :: Either react msg -> m T.Text
  , getTextualRepeat :: rep -> T.Text
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
  => EchoBot m msg react flag anti rep iter rmap
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
  => EchoBot m msg react flag anti rep iter rmap
  -> m ()
botCycle bot = do
  flag <- getFlag
  iter <- getIterator
  (ms, rs) <- getUpdates bot flag iter
  unless (null rs) $ putFlag (Nothing :: Maybe flag)
  forM_ rs processReact
  forM_ ms $ \msg -> do
    caseMsg msg
    modifyIterator $ iteratorTransformation bot (Right msg)
  delay 500000
  where
    caseMsg msg =
      case routeMsg bot msg of
        HelpMsg -> processHelpMsg msg
        RepeatMsg -> processRepeatMsg msg
        Msg -> processMsg msg
    processHelpMsg msg = do
      hText <- helpMsg <$> getBotConst
      let hMsg = replaceMsgText bot hText msg
      sendMsg bot hMsg
      chat <- getTextualChat bot $ Right hMsg
      logChatMessage chat hText
    processMsg msg = do
      r <- defaultRepeatNumber <$> getBotConst
      rMap <- getRepeatMap
      let currR = getCurrentRepeatNumber bot r rMap msg
          text = getTextualMsg bot msg
      replicateM_ currR $ sendMsg bot msg
      chat <- getTextualChat bot $ Right msg
      logChatMessage chat text
    processRepeatMsg msg = do
      r <- defaultRepeatNumber <$> getBotConst
      rMap <- getRepeatMap
      rText <- repeatMsg <$> getBotConst
      let currR = getCurrentRepeatNumber bot r rMap msg
          rnText = rText <> textify currR
          rMsg = replaceMsgText bot rnText msg
      sendRepeatMsg bot rMsg >>= parseAnticipation bot >>= putFlag
      chat <- getTextualChat bot $ Right rMsg
      logChatMessage chat rnText
    processReact react = do
      mbRep <- parseReaction bot react
      maybe (pure ()) (modifyAndLog react) mbRep
      modifyIterator $ iteratorTransformation bot (Left react)
    modifyAndLog react rep = do
      modifyRepeatMap $ repeatMapTransformation bot rep
      chat <- getTextualChat bot $ Left react
      logChatRepeat chat $ getTextualRepeat bot rep
