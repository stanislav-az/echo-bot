module Bot.EchoBot where

import           Control.Monad
import           Bot.BotClass

data EchoBot m msg react = EchoBot{
  getUpdates :: m ([msg],[react]),
  handleMsg :: msg -> m (),
  handleReaction :: react -> m ()
}

goEchoBot :: MonadDelay f => EchoBot f msg r -> f b
goEchoBot bot = forever oneCycle
 where
  oneCycle = getUpdates bot >>= \(ms, rs) -> do
    mapM_ (handleMsg bot)      ms
    mapM_ (handleReaction bot) rs
    delay 500000

