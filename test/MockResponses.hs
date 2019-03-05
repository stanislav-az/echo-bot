{-# LANGUAGE OverloadedStrings #-}

module MockResponses where

import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString               as B
import qualified Network.HTTP.Client.Internal  as Client
import qualified Network.HTTP.Types            as Types
import           Data.Maybe

type ResponseLBS = Client.Response LB.ByteString
type Path = B.ByteString

route :: Client.Request -> [(Path, ResponseLBS)] -> ResponseLBS
route req rs = fromMaybe notFoundResponse response
 where
  path     = Client.path req
  response = lookup path rs

routes :: [(Path, ResponseLBS)]
routes = undefined

notFoundResponse :: ResponseLBS
notFoundResponse = Client.Response
  { Client.responseStatus    = Types.status404
  , Client.responseVersion   = Types.http11
  , Client.responseHeaders   = []
  , Client.responseBody      = LB.empty
  , Client.responseCookieJar = Client.CJ { Client.expose = [] }
  , Client.responseClose'    = Client.ResponseClose $ pure ()
  }

res = Client.Response
  { Client.responseStatus    = Types.status404
  , Client.responseVersion   = Types.http11
  , Client.responseHeaders   = [ ( "Content-Type"
                                 , "application/json; charset=utf-8"
                                 )
                               ]
  , Client.responseBody      = LB.empty
  , Client.responseCookieJar = Client.CJ { Client.expose = [] }
  , Client.responseClose'    = Client.ResponseClose $ pure ()
  }
