module RequestBody where

import qualified Network.HTTP.Conduit          as HTTP
import qualified Data.ByteString.Lazy          as LB

getReqBodyLBS :: HTTP.Request -> LB.ByteString
getReqBodyLBS req = case HTTP.requestBody req of
  (HTTP.RequestBodyLBS bs) -> bs
  _ -> error "Request has no Data.ByteString.Lazy.Internal.ByteString encoded body"
