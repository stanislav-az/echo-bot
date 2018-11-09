{-# LANGUAGE OverloadedStrings #-}
module Logging where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Helpers

logDebug :: T.Text -> IO ()
logDebug msg = do
    currTime <- getCurrTime
    let currTimeText = T.pack $ show currTime
        log = "DEBUG entry at " `T.append` currTimeText  `T.append` "\n" `T.append` msg  `T.append` "\n" 
    TIO.appendFile "./log/debug.log" log

logError :: String -> IO ()
logError msg = do
    currTime <- getCurrTime
    let log = "ERROR entry at " ++ show currTime  ++ "\n" ++ msg  ++ "\n"
    appendFile "./log/error.log" log