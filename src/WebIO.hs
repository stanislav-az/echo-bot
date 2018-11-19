{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module WebIO where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.CaseInsensitive
import           Data.String
import           Network.HTTP.Simple
import           Network.HTTP.Conduit
import           Data.String
import           Data.Aeson
import           Data.Maybe
import           Control.Monad (unless, when, forM_, replicateM_)
import           Control.Monad.State
import           Control.Monad.Except
import           Data.HashMap.Strict hiding (null, filter, foldr)
import           Text.Read (readMaybe)
import           Bot
import           Errors
import           Logging
import           Helpers

handleResponse :: Response LB.ByteString -> ExceptT BotError IO JResponse
handleResponse response = do
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
    when (isNothing parsed) $ throwError $ NoParse $ show unparsed
    return $ fromJust parsed

makeGetUpdates :: StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int, Bool) IO Request
makeGetUpdates = do
    (ofst, sr, _, _, _, _, _) <- get
    case ofst of
        Nothing -> return $ parseRequest_ $ "GET " ++ sr ++ "getUpdates"
        (Just offset) -> do
            let req = parseRequest_ $ "GET " ++ sr ++ "getUpdates" 
                query = [("offset", Just $ BC.pack $ show offset)
                        ,("timeout", Just "10")
                        ,("allowed_updates[]", Just "callback_query,message")]
            return $ setQueryString query req

handleCallback :: (String, Integer, String) -> 
    ExceptT BotError (StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int, Bool) IO) ()
handleCallback (queryID, chatID, btnPressed) = do
    (offset, sr, hMsg, rMsg, r, chatsRepeat, dlog) <- get 
    let btn = readMaybe btnPressed :: Maybe Int
    when (isNothing btn) $ throwError $ BadCallbackData btnPressed
    let chatsRepeat' = insert chatID (fromJust btn) chatsRepeat
        req = parseRequest_ $ "POST " ++ sr ++  "answerCallbackQuery"
        queryIDLBS = fromString queryID
        btnPressedLBS = fromString btnPressed
        msgLBS = "You've choosen to repeat messages " `LB.append` btnPressedLBS `LB.append` " times"
        bodyLBS = "{\"callback_query_id\": \"" `LB.append` queryIDLBS `LB.append` 
                   "\", \"text\": \"" `LB.append` msgLBS `LB.append` "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
        endReq = setRequestBodyLBS bodyLBS reqWithHeaders
    response <- liftIO $ httpLBS endReq
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    currTime <- liftIO getCurrTime
    when dlog $ liftIO $ logDebug $ "\tA number of repeats was changed\n" `T.append` 
        "\tFor: " `T.append` (T.pack $ show chatID) `T.append` "\n" `T.append` "\tTo: " `T.append` (T.pack btnPressed)
    put (offset, sr, hMsg, rMsg, r, chatsRepeat', dlog)
    
handleMessage :: Integer -> T.Text -> 
    ExceptT BotError (StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int, Bool) IO) ()
handleMessage chatID "/help" = do
    (_, _, hMsg, _, _, _, _) <- get
    sendMessage chatID hMsg False
handleMessage chatID "/repeat" = do
    (_, _, _, rMsg, r, chatsRepeat, _) <- get
    let currR = lookupDefault r chatID chatsRepeat
        rText = T.pack $ show currR
        rnMsg = rMsg `T.append` rText
    sendMessage chatID rnMsg True
handleMessage chatID msg = do
    (_, _, _, _, r, chatsRepeat, _) <- get
    let currR = lookupDefault r chatID chatsRepeat
    replicateM_ currR $ sendMessage chatID msg False

sendMessage :: Integer -> T.Text -> Bool ->
    ExceptT BotError (StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int, Bool) IO) ()
sendMessage chatID msgText hasKeyboard = do
    (_, _, _, _, _, _, dlog) <- get
    req <- if hasKeyboard
           then lift $ makeCallbackQuery chatID msgText
           else lift $ makeSendMessage chatID msgText
    let chatIDText = T.pack $ show chatID
    response <- liftIO $ httpLBS req
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    currTime <- liftIO getCurrTime
    when dlog $ liftIO $ 
        logDebug $ "\tA message was sent\n" `T.append` "\tTo: " `T.append` chatIDText `T.append` 
            "\n" `T.append` "\tText: " `T.append` msgText
       
makeSendMessage :: Integer -> T.Text -> 
    StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int, Bool) IO Request
makeSendMessage chatID msgText = do
    (_, sr, _, _, _, _, _) <- get
    let req = parseRequest_ $ "POST " ++ sr ++  "sendMessage"
        chatIDText = T.pack $ show chatID
        bodyText = "{\"chat_id\": \"" `T.append` chatIDText `T.append` "\", \"text\": \"" `T.append` msgText `T.append` "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
    return $ setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders

makeCallbackQuery :: Integer -> T.Text -> 
    StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int, Bool) IO Request
makeCallbackQuery chatID msgText = do
    (_, sr, _, _, _, _, _) <- get
    let req = parseRequest_ $ "POST " ++ sr ++  "sendMessage"
        bodyLBS = encode $ RepeatMessageBody {chat_id = chatID, text = msgText, reply_markup = keyboard}
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
    return $ setRequestBodyLBS bodyLBS reqWithHeaders