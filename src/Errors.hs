{-# LANGUAGE FlexibleContexts #-}
module Errors where

import Control.Monad.Except
import Control.Monad.State
import Logging
import Bot
import Data.Text
import Data.HashMap.Strict

data BotError = NoParse ResponseBody | ResponseError ResponseStatus
    deriving Show

type ResponseStatus = String
type ResponseBody = String

parsingErrorHandler :: BotError -> ExceptT BotError IO JResponse
parsingErrorHandler be@(NoParse body) = do 
    liftIO $ parsingError be
    return emptyJResponse
parsingErrorHandler be@(ResponseError status) = do
    liftIO $ responseError be
    return emptyJResponse

responseErrorHandler :: BotError -> 
    ExceptT BotError (StateT (Maybe Integer, String, Text, Text, Int, HashMap Integer Int) IO) ()
responseErrorHandler be@(ResponseError status) = liftIO $ responseError be
responseErrorHandler be@(NoParse body) = liftIO $ parsingError be

parsingError :: BotError -> IO ()
parsingError (NoParse body) = logError $ "\tCould not parse response body\n" ++ "\tResponse body was: " ++ body

responseError :: BotError -> IO ()
responseError (ResponseError status) = logError $ "\tResponse status was not ok\n" ++ "\tResponse status was: " ++ status