module Helpers where

import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Time.Clock (UTCTime(..))
import Network.HTTP.Simple

getCurrTime :: IO UTCTime
getCurrTime = do
    sysTime <- getSystemTime
    return $ systemToUTCTime sysTime

isOkResponse :: Response a -> Bool
isOkResponse response = case getResponseStatusCode response of
    200 -> True
    _   -> False

myID :: a -> a
myID = id