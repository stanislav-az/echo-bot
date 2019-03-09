module Ext.Data.Text where

import qualified Data.Text as T (Text(..), pack)

textify :: (Show a) => a -> T.Text
textify = T.pack . show
