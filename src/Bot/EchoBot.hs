module Bot.EchoBot where

import Bot.BotClass
  ( MonadDelay(..)
  , MonadFlagState(..)
  , MonadIterState(..)
  , MonadRepeatState(..)
  )
import Control.Monad
import Data.Maybe

data BotMessage
  = HelpMsg
  | RepeatMsg
  | Msg
  deriving (Eq, Show)

data EchoBot m msg react flag anti rep iter rmap = EchoBot
  { getUpdates :: Maybe flag -> m ([msg], [react])
  , routeMsg :: msg -> BotMessage
  , handleMsg :: msg -> m (Int, msg)
  , sendMsg :: Int -> msg -> m ()
  , handleHelpMsg :: msg -> m msg
  , sendHelpMsg :: msg -> m ()
  , handleRepeatMsg :: msg -> m msg
  , sendRepeatMsg :: msg -> m anti
  , iteratorTransformation :: Either react msg -> Maybe iter -> Maybe iter
  , handleReaction :: react -> m rep
  , repeatMapTransformation :: rep -> rmap -> rmap
  , handleAnticipation :: anti -> m (Maybe flag)
  }

goEchoBot ::
     ( MonadDelay m
     , MonadFlagState m flag
     , MonadIterState m iter
     , MonadRepeatState m rmap
     )
  => EchoBot m msg react flag anti rep iter rmap
  -> m ()
goEchoBot bot = forever $ botCycle bot

botCycle ::
     ( MonadDelay m
     , MonadFlagState m flag
     , MonadIterState m iter
     , MonadRepeatState m rmap
     )
  => EchoBot m msg react flag anti rep iter rmap
  -> m ()
botCycle bot = do
  flag <- getFlag
  (ms, rs) <- getUpdates bot flag
  mapM_ caseMsg ms
  mapM_ processReact rs
  delay 500000
  where
    caseMsg msg =
      case routeMsg bot msg of
        HelpMsg -> processHelpMsg msg
        RepeatMsg -> processRepeatMsg msg
        Msg -> processMsg msg
    processHelpMsg msg =
      handleHelpMsg bot msg >>= sendHelpMsg bot >>
      modifyIterator (iteratorTransformation bot (Right msg))
    processMsg msg =
      handleMsg bot msg >>= uncurry (sendMsg bot) >>
      modifyIterator (iteratorTransformation bot (Right msg))
    processRepeatMsg msg =
      handleRepeatMsg bot msg >>= sendRepeatMsg bot >>= handleAnticipation bot >>=
      putFlag >>
      modifyIterator (iteratorTransformation bot (Right msg))
    processReact react = do
      r <- handleReaction bot react
      modifyRepeatMap (repeatMapTransformation bot r)
      modifyIterator (iteratorTransformation bot (Left react))
