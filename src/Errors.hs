{-# LANGUAGE FlexibleContexts #-}
module Errors where

data BotError = NoParse ResponseBody | ResponseError ResponseStatus
    deriving Show

type ResponseStatus = String
type ResponseBody = String