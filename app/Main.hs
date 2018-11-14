{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.CaseInsensitive
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Bot
import           Errors
import           Logging
import           Helpers
import           Config
import           Data.Aeson
import           Data.Maybe (fromJust, isNothing, catMaybes, maybeToList)
import           Data.Either (either, fromLeft, fromRight)
import           Control.Monad (unless, when, forM_, join, replicateM_)
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Class
import           Prelude hiding (id)
import           System.Directory (createDirectoryIfMissing)
import           Data.HashMap.Strict hiding (null, filter, foldr)

main :: IO ()
main = do
    currTime <- getCurrTime
    createDirectoryIfMissing False "./log"
    appendFile "./log/debug.log" $ "LOG START at " ++ show currTime ++ "\n"
    appendFile "./log/error.log" $ "LOG START at " ++ show currTime ++ "\n"
    writeFile "./repeat~" ""
    sr <- standardRequest
    hMsg <- helpMsg
    rMsg <- repeatMsg
    r <- defaultRepeat
    evalStateT sendLastMsgs (Nothing, sr, hMsg, rMsg, r, empty)

{-To DO
-- debug/error configuration
-- keyboard forming on every makeCallbackQuery call
-- add error and log management to handleCallback (use liftMaybe)
-}

--                      lastUpdtID     Request Help    Repeat  r    RepeatMap
sendLastMsgs :: StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int) IO ()
sendLastMsgs = do
    (offset, _, _, _, _, _) <- get
    getUpdates <- makeGetUpdates
    response <- liftIO $ httpLBS $ getUpdates
    checked <- liftIO $ runExceptT $ 
        catchError (handleResponse response) parsingErrorHandler
    let jresponse = either (const emptyJResponse) myID checked
        mcs = sortJResponse jresponse offset
        messages = fst mcs
        callbacks = snd mcs
        lastUpdtID = findLastUpdtID (result jresponse) offset
    
    runExceptT $ 
        catchError (forM_ callbacks handleCallback) responseErrorHandler
    runExceptT $ 
        catchError (forM_ messages $ uncurry handleMessage) responseErrorHandler
    
    (_, sr, hMsg, rMsg, r, chatsRepeat) <- get    
    put (lastUpdtID, sr, hMsg, rMsg, r, chatsRepeat)
    sendLastMsgs        

sortJResponse :: JResponse -> Maybe Integer -> ([(Integer,T.Text)], [CallbackQuery])
sortJResponse jresponse offset = foldr go ([],[]) (seeIfAreOld offset $ result jresponse) where
    seeIfAreOld Nothing xs = xs
    seeIfAreOld _ [] = []
    seeIfAreOld _ xs = tail xs
    go (Update _ a b) (msgs,cbs) = (getMsg a ++ msgs, maybeToList b ++ cbs)
    getMsg Nothing = []
    getMsg (Just (Message _ _ Nothing)) = []
    getMsg (Just (Message _ (Chat chatID) (Just txt))) = [(chatID, txt)]

findLastUpdtID :: [Update] -> Maybe Integer -> Maybe Integer
findLastUpdtID [] offset = offset
findLastUpdtID us _ = Just $ update_id $ last us

handleResponse :: Response LB.ByteString -> ExceptT BotError IO JResponse
handleResponse response = do
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
    when (isNothing parsed) $ throwError $ NoParse $ show unparsed
    return $ fromJust parsed

makeGetUpdates :: StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int) IO Request
makeGetUpdates = do
    (ofst, sr, _, _, _, _) <- get
    case ofst of
        Nothing -> return $ parseRequest_ $ "GET " ++ sr ++ "getUpdates"
        (Just offset) -> do
            let req = parseRequest_ $ "GET " ++ sr ++ "getUpdates" 
                query = [("offset", Just $ BC.pack $ show offset)
                        ,("timeout", Just "10")
                        ,("allowed_updates[]", Just "callback_query,message")]
            return $ setQueryString query req

handleCallback :: CallbackQuery -> 
    ExceptT BotError (StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int) IO) ()
handleCallback cq = do
    (offset, sr, hMsg, rMsg, r, chatsRepeat) <- get 
    let req = parseRequest_ $ "POST " ++ sr ++  "answerCallbackQuery"
        queryIDText = T.pack $ (id :: CallbackQuery -> String) cq
        msgText = "You've choosen to repeat messages " `T.append` (T.pack $ fromJust $ callbackData cq) `T.append` " times"
        bodyText = "{\"callback_query_id\": \"" `T.append` queryIDText `T.append` 
                   "\", \"text\": \"" `T.append` msgText `T.append` "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
        endReq = setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders
    response <- liftIO $ httpLBS endReq
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    let chatID = (id :: Chat -> Integer) $ (chat :: Message -> Chat) $ 
            fromJust $ (message :: CallbackQuery -> Maybe Message) cq
        btn = read (fromJust $ callbackData cq) :: Int
        chatsRepeat' = insert chatID btn chatsRepeat
    put (offset, sr, hMsg, rMsg, r, chatsRepeat')
    
handleMessage :: Integer -> T.Text -> 
    ExceptT BotError (StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int) IO) ()
handleMessage chatID "/help" = do
    (_, _, hMsg, _, _, _) <- get
    sendMessage chatID hMsg False
handleMessage chatID "/repeat" = do
    (_, _, _, rMsg, r, chatsRepeat) <- get
    let currR = lookupDefault r chatID chatsRepeat
        rText = T.pack $ show currR
        rnMsg = rMsg `T.append` rText
    sendMessage chatID rnMsg True
handleMessage chatID msg = do
    (_, _, _, _, r, chatsRepeat) <- get
    let currR = lookupDefault r chatID chatsRepeat
    replicateM_ currR $ sendMessage chatID msg False

sendMessage :: Integer -> T.Text -> Bool ->
    ExceptT BotError (StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int) IO) ()
sendMessage chatID msgText hasKeyboard = do
    req <- if hasKeyboard
           then lift $ makeCallbackQuery chatID msgText
           else lift $ makeSendMessage chatID msgText
    let chatIDText = T.pack $ show chatID
    response <- liftIO $ httpLBS req
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    currTime <- liftIO getCurrTime
    liftIO $ logDebug $ "\tA message was sent\n" `T.append` "\tTo: " `T.append` chatIDText `T.append` "\n" `T.append` "\tText: " `T.append` msgText
       
makeSendMessage :: Integer -> T.Text -> 
    StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int) IO Request
makeSendMessage chatID msgText = do
    (_, sr, _, _, _, _) <- get
    let req = parseRequest_ $ "POST " ++ sr ++  "sendMessage"
        chatIDText = T.pack $ show chatID
        bodyText = "{\"chat_id\": \"" `T.append` chatIDText `T.append` "\", \"text\": \"" `T.append` msgText `T.append` "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
    return $ setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders

makeCallbackQuery :: Integer -> T.Text -> 
    StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int) IO Request
makeCallbackQuery chatID msgText = do
    (_, sr, _, _, _, _) <- get
    let req = parseRequest_ $ "POST " ++ sr ++  "sendMessage"
        bodyLBS = encode $ RepeatMessageBody {chat_id = chatID, text = msgText, reply_markup = keyboard}
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
    return $ setRequestBodyLBS bodyLBS reqWithHeaders
