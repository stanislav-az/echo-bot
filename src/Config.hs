{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import qualified Data.Text as T

standardRequest :: IO String
standardRequest = do
    config <- load [Required "./bot.config.local"]
    tokenText <- require config ("token" :: T.Text) -- possibility of adding error management
    let tokenString = T.unpack tokenText
    return $ "https://api.telegram.org/bot" ++ tokenString ++ "/"  

helpMsg :: IO T.Text
helpMsg = do
    config <- load [Required "./bot.config.local"]
    require config ("helpMsg" :: T.Text)

repeatMsg :: IO T.Text
repeatMsg = do
    config <- load [Required "./bot.config.local"]
    require config ("repeatMsg" :: T.Text)

defaultRepeat :: IO Int
defaultRepeat = do
    config <- load [Required "./bot.config.local"]
    require config ("defaultRepeat" :: T.Text)

getRepeat :: Integer -> IO Int
getRepeat chatID = do
    config <- load [Required "./repeat~"]
    dRepeat <- defaultRepeat
    lookupDefault dRepeat config ("i" `T.append` (T.pack $ show chatID))