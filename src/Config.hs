{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import qualified Data.Text as T

tStandardRequest :: IO String
tStandardRequest = do
    config <- load [Required "./bot.config.local"]
    token <- require config "telegramToken"
    return $ "https://api.telegram.org/bot" ++ token ++ "/"  

slackConfig :: IO (String, String)
slackConfig = do
    config <- load [Required "./bot.config.local"]
    token <- require config "slackToken"
    channel <- require config "slackChannel"
    return (token, channel)

helpMsg :: IO T.Text
helpMsg = do
    config <- load [Required "./bot.config.local"]
    require config "helpMsg"

repeatMsg :: IO T.Text
repeatMsg = do
    config <- load [Required "./bot.config.local"]
    require config "repeatMsg"

defaultRepeat :: IO Int
defaultRepeat = do
    config <- load [Required "./bot.config.local"]
    require config "defaultRepeat"

debugLogging :: IO Bool
debugLogging = do
    config <- load [Required "./bot.config.local"]
    require config "debugLogging" 

    