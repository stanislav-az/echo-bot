module Ext.Network.HTTP.Types.URI where

import qualified Data.ByteString.Char8 as B8 (ByteString(..), pack)

showToQueryItem :: Show a => a -> B8.ByteString
showToQueryItem = B8.pack . show
