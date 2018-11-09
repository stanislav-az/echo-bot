module Logging where

import Helpers

logDebug :: String -> IO ()
logDebug msg = do
    currTime <- getCurrTime
    let log = "DEBUG entry at " ++ show currTime  ++ "\n" ++ msg  ++ "\n"
    appendFile "./log/debug.log" log

logError :: String -> IO ()
logError msg = do
    currTime <- getCurrTime
    let log = "ERROR entry at " ++ show currTime  ++ "\n" ++ msg  ++ "\n"
    appendFile "./log/error.log" log