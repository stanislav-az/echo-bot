module Helpers where

import qualified Data.ByteString.Char8 as B8 (ByteString(..), pack)
import qualified Data.Text as T (Text(..), pack)
import qualified Network.HTTP.Simple as HTTP
  ( Response(..)
  , getResponseStatusCode
  )

isOkResponse :: HTTP.Response a -> Bool
isOkResponse response =
  case HTTP.getResponseStatusCode response of
    200 -> True
    _ -> False

texify :: (Show a) => a -> T.Text
texify = T.pack . show

toQueryItem :: String -> Maybe B8.ByteString
toQueryItem = Just . B8.pack

showToQueryItem :: Show a => a -> Maybe B8.ByteString
showToQueryItem = Just . B8.pack . show
