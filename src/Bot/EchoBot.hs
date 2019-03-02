module Bot.EchoBot where

import           Control.Monad
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class

data EchoBot m msg r = EchoBot{
  getUpdates :: m ([msg],[r]),
  handleMsg :: msg -> m (),
  handleReaction :: r -> m ()
}

goEchoBot :: MonadIO f => EchoBot f msg r -> f b
goEchoBot bot = forever oneCycle
 where
  oneCycle = getUpdates bot >>= \(ms, rs) -> do
    mapM_ (handleMsg bot)      ms
    mapM_ (handleReaction bot) rs
    liftIO $ threadDelay 500000

