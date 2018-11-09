{-# LANGUAGE FlexibleContexts #-}
module Errors where

import Control.Monad.Except
import Control.Monad.State
import Logging
import Bot

data BotError = NoParse ResponseBody | ResponseError ResponseStatus
    deriving Show

type ResponseStatus = String
type ResponseBody = String

parsingErrorHandler :: BotError -> ExceptT BotError IO JResponse
parsingErrorHandler (NoParse body) = do 
    liftIO $ logError $ 
        "\tCould not parse response body\n" ++ "\tResponse body was: " ++ body
    return emptyJResponse

responseErrorHandler :: BotError -> ExceptT BotError IO ()
responseErrorHandler (ResponseError status) = liftIO $ logError $ 
    "\tResponse status was not ok\n" ++ "\tResponse status was: " ++ status