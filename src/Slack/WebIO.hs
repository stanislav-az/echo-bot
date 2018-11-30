{-# LANGUAGE OverloadedStrings #-}
module Slack.WebIO (runSlackBot) where

import Config
import Errors
import Logging
import Bot
import Helpers
import Slack.Bot
import qualified Data.Text as T
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.Text.Encoding (encodeUtf8)
import           Data.CaseInsensitive
import           Data.String
import           Network.HTTP.Simple
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Maybe
import           Data.Aeson
import           Control.Concurrent (threadDelay)

{-To DO

-}

runSlackBot :: IO ()
runSlackBot = do
    (token, channel) <- slackConfig
    hMsg <- helpMsg
    rMsg <- repeatMsg
    r <- defaultRepeat
    dlog <- debugLogging
    evalStateT goSlackBot (Nothing, token, channel, hMsg, rMsg, r, dlog, Nothing)
    
--                    timestamp     token   channel hMsg    rMsg    r    dlog  repeatTS
goSlackBot :: StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool, Maybe String) IO ()
goSlackBot = do
    (timestamp, _, _, _, _, _, _, _) <- get
    conHistory <- makeConHistory
    response <- httpLBS conHistory
    
    checked <- liftIO $ runExceptT $ 
        catchError (hadleResponse response) sParsingErrorHandler
    let sJResponse = either (const emptySJResponse) id checked
        msgs = sortSJResponse $ messages sJResponse
        lastTS = findLastTS sJResponse timestamp
    
    runExceptT $
        catchError handleRepeatTS sResponseErrorHandler
    runExceptT $
        catchError (forM_ msgs handleMessage) sResponseErrorHandler

    updateAndDelay lastTS
    goSlackBot

updateAndDelay :: Maybe String -> 
    StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool, Maybe String) IO ()
updateAndDelay lastTS = do
    (_, token, channel, hMsg, rMsg, r, dlog, repeatTS) <- get
    put (lastTS, token, channel, hMsg, rMsg, r, dlog, repeatTS)
    liftIO $ threadDelay 1000000

sortSJResponse :: Maybe [SMessage] -> [T.Text]
sortSJResponse = maybe [] (foldl f []) where
    f txts sm = case user sm of
        Nothing -> txts
        _ -> ((text :: SMessage -> T.Text) sm) : txts

findLastTS :: SJResponse -> Maybe String -> Maybe String
findLastTS sJResponse timestamp = case messages sJResponse of
    (Just []) -> timestamp
    (Just ms) -> Just $ ts $ head ms
    Nothing -> timestamp

makeConHistory :: StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool, Maybe String) IO Request
makeConHistory = do
    (timestamp, token, channel, _, _, _, _, _) <- get
    let reqString = "GET " ++ "https://slack.com/api/conversations.history?token="
            ++ token ++ "&channel=" ++ channel
    case timestamp of 
        Nothing -> return $ parseRequest_ $ reqString ++ "&limit=1" 
        (Just ts) -> return $ parseRequest_ $ reqString ++ "&oldest=" ++ ts

makePostMessage :: T.Text ->
    StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool, Maybe String) IO Request
makePostMessage msgText = do
    (_, token, channel, _, _, _, _, _) <- get
    let req = parseRequest_ $ "POST " ++ "https://slack.com/api/chat.postMessage"
        bodyText = "{\"channel\": \"" `T.append` (T.pack channel) `T.append`
            "\", \"text\": \"" `T.append` msgText `T.append` "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json; charset=utf-8"),
            ("Authorization" :: CI B.ByteString, "Bearer " `B.append` (fromString token))] req
        endReq = setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders
    return endReq

hadleResponse :: Response LB.ByteString -> ExceptT BotError IO SJResponse
hadleResponse response = do
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe SJResponse
    when (isNothing parsed) $ throwError $ NoParse $ show unparsed
    return $ fromJust parsed

handleMessage :: T.Text -> 
    ExceptT BotError (StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool, Maybe String) IO) ()
handleMessage "_help" = do
    (_, _, _, hMsg, _, _, _, _) <- get
    postMessage hMsg
    return ()
handleMessage "_repeat" = do
    (timestamp, token, channel, hMsg, rMsg, r, dlog, _) <- get
    let rText = T.pack $ show r
        rnMsg = rMsg `T.append` rText
    response <- postMessage rnMsg
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe SJResponse
        repeatTS = ts <$> (message =<< parsed)
    when (isNothing repeatTS) $ throwError $ NoParse $ show unparsed
    put (timestamp, token, channel, hMsg, rMsg, r, dlog, repeatTS)
handleMessage msg = do
    (_, _, _, _, _, r, _, _) <- get
    replicateM_ r $ postMessage msg

postMessage :: T.Text -> 
    ExceptT BotError (StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool, Maybe String) IO) (Response LB.ByteString)
postMessage msgText = do
    (_, _, _, _, _, _, dlog, _) <- get
    req <- lift $ makePostMessage msgText
    response <- httpLBS req
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    when dlog $ liftIO $ 
        (logDebug Slack) $ "\tA message was sent\n" `T.append` "\tText: " `T.append` msgText
    return $ response
    
makeGetReactions :: StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool, Maybe String) IO Request
makeGetReactions = do
    (_, token, channel, _, _, _, _, repeatTS) <- get
    let rTS = fromMaybe (error "Calling makeGetReactions w/o repeat timestamp") repeatTS
        reqString = "GET " ++ "https://slack.com/api/reactions.get?token="
            ++ token ++ "&channel=" ++ channel ++ "&timestamp=" ++ rTS
    return $ parseRequest_ reqString
        
handleRepeatTS :: ExceptT BotError (StateT (Maybe String, String, String, T.Text, T.Text, Int, Bool, Maybe String) IO) ()
handleRepeatTS = do
    (timestamp, token, channel, hMsg, rMsg, _, dlog, repeatTS) <- get
    when (isJust repeatTS) $ do
        req <- lift $ makeGetReactions
        response <- httpLBS req 
        unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
        let unparsed = getResponseBody response
            parsed = decode unparsed :: Maybe SJResponse
            reacts = (fmap name) <$> (parsed >>= message >>= reactions)
            maybeR = parseReaction reacts
        when (isNothing parsed) $ throwError $ NoParse $ show unparsed
        when (isJust maybeR) $ do
            let r = fromJust maybeR
            put (timestamp, token, channel, hMsg, rMsg, r, dlog, Nothing)
            when dlog $ liftIO $ (logDebug Slack) $ "\tA number of repeats was changed\n" `T.append` 
                "\tTo: " `T.append` (T.pack $ show r)
        
parseReaction :: Maybe [String] -> Maybe Int
parseReaction (Just [w]) = case w of
    "one"   -> Just 1
    "two"   -> Just 2
    "three" -> Just 3
    "four"  -> Just 4
    "five"  -> Just 5
    _       -> Nothing
parseReaction _ = Nothing
        