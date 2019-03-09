module Ext.Network.HTTP.Types.Status where

import qualified Network.HTTP.Simple as HTTP
  ( Response(..)
  , getResponseStatusCode
  )

isOkResponse :: HTTP.Response a -> Bool
isOkResponse response =
  case HTTP.getResponseStatusCode response of
    200 -> True
    _ -> False
