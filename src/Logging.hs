{-# LANGUAGE OverloadedStrings #-}
module Logging where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Helpers
import           Bot
import           System.Directory (createDirectoryIfMissing)
import           Config

logDebug :: Bot -> T.Text -> IO ()
logDebug bot msg = do
    currTime <- getCurrTime
    let currTimeText = T.pack $ show currTime
        botText = T.pack $ show bot
        log = botText `T.append` "DEBUG entry at " `T.append` currTimeText  `T.append` "\n" `T.append` msg  `T.append` "\n" 
    TIO.appendFile "./log/debug.log" log

logError :: Bot -> String -> IO ()
logError bot msg = do
    currTime <- getCurrTime
    let log = (show bot) ++ "ERROR entry at " ++ (show currTime)  ++ "\n" ++ msg  ++ "\n"
    appendFile "./log/error.log" log

logStart :: IO ()
logStart = do
    currTime <- getCurrTime
    createDirectoryIfMissing False "./log"
    dlog <- debugLogging
    appendFile "./log/debug.log" $ "LOG " ++ (if dlog then "START" else "DISABLED") ++ " at " ++ show currTime ++ "\n"
    appendFile "./log/error.log" $ "LOG START at " ++ show currTime ++ "\n"