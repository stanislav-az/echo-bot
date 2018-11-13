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
import           Data.Maybe (fromJust, isNothing, catMaybes)
import           Data.Either (either, fromLeft, fromRight)
import           Control.Monad (unless, when, forM_, join, replicateM_)
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Class
import           Prelude hiding (id)
import           System.Directory (createDirectoryIfMissing)

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
    evalStateT sendLastMsgs (Nothing, sr, hMsg, rMsg, r)

{-To DO
-- inline buttons HashMap
-}

--                      lastUpdtID     Request Help    Repeat  r
sendLastMsgs :: StateT (Maybe Integer, String, T.Text, T.Text, Int) IO ()
sendLastMsgs = do
    (offset, sr, hMsg, rMsg, r) <- get
    getUpdates <- makeGetUpdates
    response <- liftIO $ httpLBS $ getUpdates
    checked <- liftIO $ runExceptT $ 
        catchError (handleResponse response) parsingErrorHandler
    let jresponse = either (const emptyJResponse) myID checked

    unless (null $ result jresponse) $ do
        let updts = case offset of
                Nothing -> result jresponse
                _ -> tail $ result jresponse
            lastUpdtID = case updts of
                [] -> offset
                _  -> Just $ update_id $ last updts
            post = envelop updts
        put (lastUpdtID, sr, hMsg, rMsg, r)
        runExceptT $ 
            catchError (forM_ post $ uncurry handleMessage) responseErrorHandler
        return ()

    sendLastMsgs        

envelop :: [Update] -> [(Integer,T.Text)]
envelop updts = catMaybes $ fmap (join . make) updts where
    make updt = fmap f $ (message :: Update -> Maybe Message) updt
    f msg = case (text :: Message -> Maybe T.Text) msg of 
            Nothing -> Nothing
            textMsg -> Just ((id :: Chat -> Integer) $ chat msg, fromJust $ (text :: Message -> Maybe T.Text) msg)

handleResponse :: Response LB.ByteString -> ExceptT BotError IO JResponse
handleResponse response = do
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    let unparsed = getResponseBody response
        parsed = decode unparsed :: Maybe JResponse
    when (isNothing parsed) $ throwError $ NoParse $ show unparsed
    return $ fromJust parsed

makeGetUpdates :: StateT (Maybe Integer, String, T.Text, T.Text, Int) IO Request
makeGetUpdates = do
    (ofst, sr, _, _, _) <- get
    case ofst of
        Nothing -> return $ parseRequest_ $ "GET " ++ sr ++ "getUpdates"
        (Just offset) -> do
            let req = parseRequest_ $ "GET " ++ sr ++ "getUpdates" 
                query = [("offset", Just $ BC.pack $ show offset)
                        ,("timeout", Just "10")
                        ,("allowed_updates[]", Just "message")]
            return $ setQueryString query req

handleMessage :: Integer -> T.Text -> ExceptT BotError (StateT (Maybe Integer, String, T.Text, T.Text, Int) IO) ()
handleMessage chatID "/help" = do
    (_, _, hMsg, _, _) <- get
    sendMessage chatID hMsg
{-handleMessage chatID "/repeat" = do
    r <- liftIO $ getRepeat chatID
    rMsg <- liftIO repeatMsg
    let rText = T.pack $ show r
        rnMsg = rMsg `T.append` "\n(current number is " `T.append` rText `T.append` ")" -- не кастомизировано через конфиг
    sendMessage chatID rnMsg-}
handleMessage chatID msg = do
    (_, _, _, _, r) <- get
    replicateM_ r $ sendMessage chatID msg

sendMessage :: Integer -> T.Text -> ExceptT BotError (StateT (Maybe Integer, String, T.Text, T.Text, Int) IO) ()
sendMessage chatID msgText = do
    req <- lift $ makeSendMessage chatID msgText
    let chatIDText = T.pack $ show chatID
    response <- liftIO $ httpLBS req
    unless (isOkResponse response) $ throwError $ ResponseError $ show $ getResponseStatus response
    currTime <- liftIO getCurrTime
    liftIO $ logDebug $ "\tA message was sent\n" `T.append` "\tTo: " `T.append` chatIDText `T.append` "\n" `T.append` "\tText: " `T.append` msgText
       
makeSendMessage :: Integer -> T.Text -> StateT (Maybe Integer, String, T.Text, T.Text, Int) IO Request
makeSendMessage chatID msgText = do
    (_, sr, _, _, _) <- get
    let req = parseRequest_ $ "POST " ++ sr ++  "sendMessage"
        chatIDText = T.pack $ show chatID
        bodyText = "{\"chat_id\": \"" `T.append` chatIDText `T.append` "\", \"text\": \"" `T.append` msgText `T.append` "\"}"
        reqWithHeaders = setRequestHeaders [("Content-Type" :: CI B.ByteString, "application/json")] req
    return $ setRequestBodyLBS (LB.fromStrict $ encodeUtf8 bodyText) reqWithHeaders
