module Bot.Exception where

import Bot.BotClass
import Bot.BotMonad
import Control.Monad
import Control.Monad.Catch
import Helpers
import Network.HTTP.Simple

exceptionHandlers :: MonadLogger m => [Handler m ()]
exceptionHandlers = [Handler botExceptionHandler, Handler otherExceptionHandler]

botExceptionHandler :: MonadLogger m => BotException -> m ()
botExceptionHandler = logWarn . texify

otherExceptionHandler :: MonadLogger m => SomeException -> m ()
otherExceptionHandler = logError . texify

checkResponseStatus :: (MonadThrow f, Show a) => Response a -> f ()
checkResponseStatus response =
  unless (isOkResponse response) $
  throwM $ ResponseException (status ++ " " ++ body)
  where
    status = show $ getResponseStatus response
    body = show $ getResponseBody response

throwParseException :: (MonadThrow f, Show a) => a -> f ()
throwParseException = throwM . NoParse . show
