{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Data.Configurator.Types

data Bot = Telegram | Slack
  deriving (Show, Eq)

instance Configured Bot where
  convert (String "Telegram") = Just Telegram
  convert (String "Slack") = Just Slack
  convert _ = Nothing
