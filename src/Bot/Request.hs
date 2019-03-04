module Bot.Request where

import qualified Network.HTTP.Conduit          as HTTP
import qualified Data.ByteString.Lazy          as LB

data Request = Request {
  unwrapRequest :: HTTP.Request,
  getRequestBody :: LB.ByteString
} deriving (Show)

reqWithEmptyBody :: HTTP.Request -> Request
reqWithEmptyBody req = Request req LB.empty
