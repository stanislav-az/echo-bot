{-# LANGUAGE FlexibleContexts #-}
module Errors where

import Control.Monad.Except
import Control.Monad.State
import Logging
import Telegram.Bot
import Data.Text
import Data.HashMap.Strict
import Bot

data BotError = NoParse ResponseBody | BadCallbackData CallbackData | ResponseError ResponseStatus 
    deriving Show

type ResponseStatus = String
type ResponseBody = String
type CallbackData = String

tParsingErrorHandler :: BotError -> ExceptT BotError IO JResponse
tParsingErrorHandler (NoParse body) = do 
    liftIO $ parsingError Telegram body
    return emptyJResponse
tParsingErrorHandler (ResponseError status) = do
    liftIO $ responseError Telegram status
    return emptyJResponse
tParsingErrorHandler (BadCallbackData badData) = do
    liftIO $ badCallbackError Telegram badData
    return emptyJResponse

tResponseErrorHandler :: BotError -> 
    ExceptT BotError (StateT (Maybe Integer, String, Text, Text, Int, HashMap Integer Int, Bool) IO) ()
tResponseErrorHandler (ResponseError status) = liftIO $ responseError Telegram status
tResponseErrorHandler (NoParse body) = liftIO $ parsingError Telegram body
tResponseErrorHandler (BadCallbackData badData) = liftIO $ badCallbackError Telegram badData

parsingError :: Bot -> String -> IO ()
parsingError bot body = (logError bot) $ "\tCould not parse response body\n" ++ "\tResponse body was: " ++ body

responseError :: Bot -> String -> IO ()
responseError bot status = (logError bot) $ "\tResponse status was not ok\n" ++ "\tResponse status was: " ++ status

badCallbackError :: Bot -> String -> IO ()
badCallbackError bot badData = (logError bot) $ "\tReceived bad callback data\n" ++ "\tIt was: " ++ badData