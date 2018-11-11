{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import qualified Data.Text as T

standardRequest :: IO String
standardRequest = do
    config <- load [Required "./bot.config.local"]
    tokenText <- require config ("token" :: T.Text)
    let tokenString = T.unpack tokenText
    return $ "https://api.telegram.org/bot" ++ tokenString ++ "/"  