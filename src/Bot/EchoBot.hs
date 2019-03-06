module Bot.EchoBot where

import           Control.Monad
import           Bot.BotClass

data EchoBot m msg react = EchoBot{
  getUpdates :: m ([msg],[react]),
  handleMsg :: msg -> m (),
  handleReaction :: react -> m ()
}

goEchoBot :: MonadDelay m => EchoBot m msg react -> m ()
goEchoBot bot = forever $ botCycle bot

botCycle :: MonadDelay m => EchoBot m msg react -> m ()
botCycle bot = getUpdates bot >>= \(ms, rs) -> do
  mapM_ (handleMsg bot)      ms
  mapM_ (handleReaction bot) rs
  delay 500000
