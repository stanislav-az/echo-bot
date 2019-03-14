module Bot.Exception where

import Bot.BotClass (MonadLogger(..))
import Bot.BotMonad (BotException(..))
import Control.Monad (unless)
import Control.Monad.Catch
import Ext.Data.Text (textify)
import Ext.Network.HTTP.Types.Status (isOkResponse)
import qualified Network.HTTP.Simple as HTTP
  ( Response(..)
  , getResponseBody
  , getResponseStatus
  )

exceptionHandlers :: MonadLogger m => [Handler m ()]
exceptionHandlers = [Handler botExceptionHandler, Handler otherExceptionHandler]

botExceptionHandler :: MonadLogger m => BotException -> m ()
botExceptionHandler = logWarn . textify

otherExceptionHandler :: MonadLogger m => SomeException -> m ()
otherExceptionHandler = logError . textify

checkResponseStatus :: (MonadThrow f, Show a) => HTTP.Response a -> f ()
checkResponseStatus response =
  unless (isOkResponse response) $
  throwM $ ResponseException (status ++ " " ++ body)
  where
    status = show $ HTTP.getResponseStatus response
    body = show $ HTTP.getResponseBody response

throwParseException :: (MonadThrow f, Show a) => a -> f b
throwParseException = throwM . NoParse . show
