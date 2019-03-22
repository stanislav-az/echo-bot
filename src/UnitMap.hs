{-# LANGUAGE RecordWildCards #-}

module UnitMap where

import Data.Maybe (fromMaybe)

newtype UnitMap k v = UnitMap
  { lookup :: Maybe v
  } deriving (Eq, Show)

lookupDefault :: v -> UnitMap k v -> v
lookupDefault v UnitMap {..} = fromMaybe v lookup

insert :: v -> UnitMap k v
insert = UnitMap . Just

empty :: UnitMap k v
empty = UnitMap Nothing
