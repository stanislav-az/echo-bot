module Helpers where

import           Data.Time.Clock.System         ( getSystemTime
                                                , systemToUTCTime
                                                )
import           Data.Time.Clock                ( UTCTime(..) )
import           Network.HTTP.Simple
import qualified Data.Text                     as T
                                                ( Text(..)
                                                , pack
                                                )

getCurrTime :: IO UTCTime
getCurrTime = systemToUTCTime <$> getSystemTime

isOkResponse :: Response a -> Bool
isOkResponse response = case getResponseStatusCode response of
  200 -> True
  _   -> False

myID :: a -> a
myID = id

texify :: (Show a) => a -> T.Text
texify = T.pack . show
