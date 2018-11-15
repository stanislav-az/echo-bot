{-# LANGUAGE FlexibleContexts #-}
module Errors where

import Control.Monad.Except
import Control.Monad.State
import Logging
import Bot
import Data.Text
import Data.HashMap.Strict

data BotError = NoParse ResponseBody | BadCallbackData CallbackData | ResponseError ResponseStatus 
    deriving Show

type ResponseStatus = String
type ResponseBody = String
type CallbackData = String

parsingErrorHandler :: BotError -> ExceptT BotError IO JResponse
parsingErrorHandler (NoParse body) = do 
    liftIO $ parsingError body
    return emptyJResponse
parsingErrorHandler (ResponseError status) = do
    liftIO $ responseError status
    return emptyJResponse
parsingErrorHandler (BadCallbackData badData) = do
    liftIO $ badCallbackError badData
    return emptyJResponse

responseErrorHandler :: BotError -> 
    ExceptT BotError (StateT (Maybe Integer, String, Text, Text, Int, HashMap Integer Int, Bool) IO) ()
responseErrorHandler (ResponseError status) = liftIO $ responseError status
responseErrorHandler (NoParse body) = liftIO $ parsingError body
responseErrorHandler (BadCallbackData badData) = liftIO $ badCallbackError badData

parsingError :: String -> IO ()
parsingError body = logError $ "\tCould not parse response body\n" ++ "\tResponse body was: " ++ body

responseError :: String -> IO ()
responseError status = logError $ "\tResponse status was not ok\n" ++ "\tResponse status was: " ++ status

badCallbackError :: String -> IO ()
badCallbackError badData = logError $ "\tReceived bad callback data\n" ++ "\tIt was: " ++ badData