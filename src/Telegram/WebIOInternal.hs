{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Telegram.WebIOInternal where

import           Errors
import           Helpers
import           Logging
import           Bot
import           Telegram.Bot
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.CaseInsensitive
import           Data.String
import           Network.HTTP.Simple
import           Network.HTTP.Conduit
import           Data.Aeson
import           Data.Maybe
import           Control.Monad                  ( unless
                                                , when
                                                , forM_
                                                , replicateM_
                                                )
import           Control.Monad.State
import           Control.Monad.Except
import           Data.HashMap.Strict     hiding ( null
                                                , filter
                                                , foldr
                                                )
import           Text.Read                      ( readMaybe )
import           Prelude                 hiding ( id )
import           Bot.BotMonad
import           Bot.BotClass

-- lastUpdtID request help repeat repeatMap dlog
goTelegramBot :: TelegramMonad ()
goTelegramBot = do
  offset     <- gets tLastUpdateId
  getUpdates <- makeGetUpdates
  response   <- httpLBS getUpdates
  jresponse  <- handleResponse response
  let mcs        = sortJResponse jresponse offset
      messages   = fst mcs
      callbacks  = snd mcs
      lastUpdtID = findLastUpdtID (result jresponse) offset

  forM_ callbacks handleCallback
  forM_ messages $ uncurry handleMessage

  modify $ \s -> s { tLastUpdateId = lastUpdtID }
  goTelegramBot

-- chatID msgText queryID chatID button
sortJResponse
  :: JResponse
  -> Maybe Integer
  -> ([(Integer, T.Text)], [(String, Integer, String)])
sortJResponse jresponse offset = foldr
  go
  ([], [])
  (seeIfAreOld offset $ result jresponse) where
  seeIfAreOld Nothing xs = xs
  seeIfAreOld _       [] = []
  seeIfAreOld _       xs = tail xs
  go (Update _ a b) (msgs, cbs) = (getMsg a ++ msgs, getCb b ++ cbs)
  getMsg (Just (Message _ (Chat chatID) (Just txt))) = [(chatID, txt)]
  getMsg _ = []
  getCb (Just (CallbackQuery queryID (Just msg) (Just btnPressed))) =
    [ ( queryID
      , (id :: Chat -> Integer) $ (chat :: Message -> Chat) msg
      , btnPressed
      )
    ]
  getCb _ = []

findLastUpdtID :: [Update] -> Maybe Integer -> Maybe Integer
findLastUpdtID [] offset = offset
findLastUpdtID us _      = Just $ update_id $ last us

makeStandardReq :: String -> String
makeStandardReq token = "https://api.telegram.org/bot" ++ token ++ "/"

handleResponse :: Response LB.ByteString -> TelegramMonad JResponse
handleResponse response = do
  checkResponseStatus response
  let unparsed = getResponseBody response
      parsed   = decode unparsed :: Maybe JResponse
  maybe (throwParseError unparsed >> pure emptyJResponse) pure parsed

makeGetUpdates :: TelegramMonad Request
makeGetUpdates = do
  lastUpdateId <- gets tLastUpdateId
  sr           <- makeStandardReq <$> gets tToken
  case lastUpdateId of
    Nothing       -> return $ parseRequest_ $ "GET " ++ sr ++ "getUpdates"
    (Just offset) -> do
      let req = parseRequest_ $ "GET " ++ sr ++ "getUpdates"
          query =
            [ ("offset"           , Just $ BC.pack $ show offset)
            , ("timeout"          , Just "10")
            , ("allowed_updates[]", Just "callback_query,message")
            ]
      return $ setQueryString query req

handleCallback :: (String, Integer, String) -> TelegramMonad ()
handleCallback (queryID, chatID, btnPressed) = do
  chatsRepeat <- gets tRepeatMap
  let btn = readMaybe btnPressed :: Maybe Int
  when (isNothing btn) $ throwError $ BadCallbackData btnPressed
  let chatsRepeat' = insert chatID (fromJust btn) chatsRepeat
  req      <- makeAnswerCallbackQuery queryID btnPressed
  response <- httpLBS req
  checkResponseStatus response
  logChatRepeat (texify chatID) (T.pack btnPressed)
  modify $ \s -> s { tRepeatMap = chatsRepeat' }

handleMessage :: Integer -> T.Text -> TelegramMonad ()
handleMessage chatID "/help" = do
  hMsg <- gets tHelpMsg
  sendMessage chatID hMsg False
handleMessage chatID "/repeat" = do
  rMsg        <- gets tRepeatMsg
  r           <- gets tRepeatNumber
  chatsRepeat <- gets tRepeatMap
  let currR = lookupDefault r chatID chatsRepeat
      rText = texify currR
      rnMsg = rMsg <> rText
  sendMessage chatID rnMsg True
handleMessage chatID msg = do
  r           <- gets tRepeatNumber
  chatsRepeat <- gets tRepeatMap
  let currR = lookupDefault r chatID chatsRepeat
  replicateM_ currR $ sendMessage chatID msg False

sendMessage :: Integer -> T.Text -> Bool -> TelegramMonad ()
sendMessage chatID msgText hasKeyboard = do
  req <- if hasKeyboard
    then makeCallbackQuery chatID msgText
    else makeSendMessage chatID msgText
  response <- httpLBS req
  checkResponseStatus response
  logChatMessage (texify chatID) msgText

makeSendMessage :: Integer -> T.Text -> TelegramMonad Request
makeSendMessage chatID msgText = do
  sr <- makeStandardReq <$> gets tToken
  let
    req        = parseRequest_ $ "POST " ++ sr ++ "sendMessage"
    chatIDText = texify chatID
    bodyText =
      "{\"chat_id\": \"" <> chatIDText <> "\", \"text\": \"" <> msgText <> "\"}"
    reqWithHeaders = setRequestHeaders
      [("Content-Type" :: CI B.ByteString, "application/json")]
      req
  return
    $ setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders

makeCallbackQuery :: Integer -> T.Text -> TelegramMonad Request
makeCallbackQuery chatID msgText = do
  sr <- makeStandardReq <$> gets tToken
  let req     = parseRequest_ $ "POST " ++ sr ++ "sendMessage"
      bodyLBS = encode $ RepeatMessageBody { chat_id      = chatID
                                           , text         = msgText
                                           , reply_markup = keyboard
                                           }
      reqWithHeaders = setRequestHeaders
        [("Content-Type" :: CI B.ByteString, "application/json")]
        req
  return $ setRequestBodyLBS bodyLBS reqWithHeaders

makeAnswerCallbackQuery :: String -> String -> TelegramMonad Request
makeAnswerCallbackQuery queryID btnPressed = do
  sr <- makeStandardReq <$> gets tToken
  let
    req           = parseRequest_ $ "POST " ++ sr ++ "answerCallbackQuery"
    queryIDLBS    = fromString queryID
    btnPressedLBS = fromString btnPressed
    msgLBS = "You've choosen to repeat messages " <> btnPressedLBS <> " times"
    bodyLBS =
      "{\"callback_query_id\": \""
        <> queryIDLBS
        <> "\", \"text\": \""
        <> msgLBS
        <> "\"}"
    reqWithHeaders = setRequestHeaders
      [("Content-Type" :: CI B.ByteString, "application/json")]
      req
  return $ setRequestBodyLBS bodyLBS reqWithHeaders
