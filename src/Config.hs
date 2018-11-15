{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import qualified Data.Text as T

standardRequest :: IO String
standardRequest = do
    config <- load [Required "./bot.config.local"]
    tokenText <- require config "token" -- possibility of adding error management
    return $ "https://api.telegram.org/bot" ++ tokenText ++ "/"  

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
