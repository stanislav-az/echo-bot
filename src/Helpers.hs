module Helpers where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T (Text(..), pack)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Network.HTTP.Simple

getCurrTime :: IO UTCTime
getCurrTime = systemToUTCTime <$> getSystemTime

isOkResponse :: Response a -> Bool
isOkResponse response =
  case getResponseStatusCode response of
    200 -> True
    _ -> False

myID :: a -> a
myID = id

texify :: (Show a) => a -> T.Text
texify = T.pack . show

toQueryItem :: String -> Maybe B8.ByteString
toQueryItem = Just . B8.pack

showToQueryItem :: Show a => a -> Maybe B8.ByteString
showToQueryItem = Just . B8.pack . show
