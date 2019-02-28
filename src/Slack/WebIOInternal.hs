{-# LANGUAGE OverloadedStrings #-}
module Slack.WebIOInternal where

import           Errors
import           Bot
import           Helpers
import           Slack.Bot
import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.CaseInsensitive
import           Data.String
import           Network.HTTP.Simple
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Maybe
import           Data.Aeson
import           Control.Concurrent             ( threadDelay )
import           Bot.BotMonad
import           Bot.BotClass
import           Logging

-- timestamp token channel hMsg rMsg r dlog repeatTS
goSlackBot :: SlackMonad ()
goSlackBot = do
  timestamp  <- gets sLastTimestamp
  conHistory <- makeConHistory
  response   <- httpLBS conHistory

  sJResponse <- hadleResponse response
  let msgs   = sortSJResponse $ messages sJResponse
      lastTS = findLastTS sJResponse timestamp

  handleRepeatTS
  forM_ msgs handleMessage

  modify $ \s -> s { sLastTimestamp = lastTS }
  liftIO $ threadDelay 1000000
  goSlackBot

sortSJResponse :: Maybe [SMessage] -> [T.Text]
sortSJResponse = maybe [] (foldl f []) where
  f txts sm = case user sm of
    Nothing -> txts
    _       -> ((text :: SMessage -> T.Text) sm) : txts

findLastTS :: SJResponse -> Maybe String -> Maybe String
findLastTS sJResponse timestamp = case messages sJResponse of
  (Just []) -> timestamp
  (Just ms) -> Just $ ts $ head ms
  Nothing   -> timestamp

makeConHistory :: SlackMonad Request
makeConHistory = do
  timestamp <- gets sLastTimestamp
  token     <- gets sToken
  channel   <- gets sChannel
  let reqString =
        "GET "
          ++ "https://slack.com/api/conversations.history?token="
          ++ token
          ++ "&channel="
          ++ channel
  case timestamp of
    Nothing   -> return $ parseRequest_ $ reqString ++ "&limit=1"
    (Just ts) -> return $ parseRequest_ $ reqString ++ "&oldest=" ++ ts

makePostMessage :: T.Text -> SlackMonad Request
makePostMessage msgText = do
  token   <- gets sToken
  channel <- gets sChannel
  let
    req = parseRequest_ $ "POST " ++ "https://slack.com/api/chat.postMessage"
    bodyText =
      "{\"channel\": \""
        <> (T.pack channel)
        <> "\", \"text\": \""
        <> msgText
        <> "\"}"
    reqWithHeaders = setRequestHeaders
      [ ("Content-Type" :: CI B.ByteString , "application/json; charset=utf-8")
      , ("Authorization" :: CI B.ByteString, "Bearer " <> (fromString token))
      ]
      req
    endReq =
      setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders
  return endReq

hadleResponse :: Response LB.ByteString -> SlackMonad SJResponse
hadleResponse response = do
  checkResponseStatus response
  let unparsed = getResponseBody response
      parsed   = decode unparsed :: Maybe SJResponse
  maybe (throwParseError unparsed >> pure emptySJResponse) pure parsed

handleMessage :: T.Text -> SlackMonad ()
handleMessage "_help" = do
  hMsg <- gets sHelpMsg
  postMessage hMsg
  return ()
handleMessage "_repeat" = do
  rMsg <- gets sRepeatMsg
  r    <- gets sRepeatNumber
  let rnMsg = rMsg <> (texify r)
  response <- postMessage rnMsg
  let unparsed = getResponseBody response
      parsed   = decode unparsed :: Maybe SJResponse
      repeatTS = ts <$> (message =<< parsed)
  when (isNothing repeatTS) $ throwParseError unparsed
  modify $ \s -> s { sRepeatTimestamp = repeatTS }
handleMessage msg = do
  r <- gets sRepeatNumber
  replicateM_ r $ postMessage msg

postMessage :: T.Text -> SlackMonad (Response LB.ByteString)
postMessage msgText = do
  req      <- makePostMessage msgText
  response <- httpLBS req
  checkResponseStatus response
  chat <- T.pack <$> gets sChannel
  logChatMessage chat msgText
  pure response

makeGetReactions :: SlackMonad Request
makeGetReactions = do
  token    <- gets sToken
  channel  <- gets sChannel
  repeatTS <- gets sRepeatTimestamp
  let rTS = fromMaybe (error "Calling makeGetReactions w/o repeat timestamp")
                      repeatTS
      reqString =
        "GET "
          ++ "https://slack.com/api/reactions.get?token="
          ++ token
          ++ "&channel="
          ++ channel
          ++ "&timestamp="
          ++ rTS
  return $ parseRequest_ reqString

handleRepeatTS :: SlackMonad ()
handleRepeatTS = do
  repeatTS <- gets sRepeatTimestamp
  when (isJust repeatTS) $ do
    req      <- makeGetReactions
    response <- httpLBS req
    checkResponseStatus response
    let unparsed = getResponseBody response
        parsed   = decode unparsed :: Maybe SJResponse
        reacts   = (fmap name) <$> (parsed >>= message >>= reactions)
        maybeR   = parseReaction reacts
    when (isNothing parsed) $ throwParseError unparsed
    when (isJust maybeR) $ do
      let r = fromJust maybeR
      modify $ \s -> s { sRepeatNumber = r, sRepeatTimestamp = Nothing }
      chat <- T.pack <$> gets sChannel
      logChatRepeat chat (texify r)

parseReaction :: Maybe [String] -> Maybe Int
parseReaction (Just [w]) = case w of
  "one"   -> Just 1
  "two"   -> Just 2
  "three" -> Just 3
  "four"  -> Just 4
  "five"  -> Just 5
  _       -> Nothing
parseReaction _ = Nothing
