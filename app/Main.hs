{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import qualified Data.Text as T
import           Network.HTTP.Simple
import           Control.Monad.State
import           Control.Monad.Except
import           Prelude hiding (id)
import           System.Directory (createDirectoryIfMissing)
import           Data.HashMap.Strict hiding (null, filter, foldr)
import           Bot
import           Config
import           WebIO
import           Helpers
import           Errors

main :: IO ()
main = do
    currTime <- getCurrTime
    createDirectoryIfMissing False "./log"
    dlog <- debugLogging
    appendFile "./log/debug.log" $ "LOG " ++ (if dlog then "START" else "DISABLED") ++ " at " ++ show currTime ++ "\n"
    appendFile "./log/error.log" $ "LOG START at " ++ show currTime ++ "\n"
    sr <- standardRequest
    hMsg <- helpMsg
    rMsg <- repeatMsg
    r <- defaultRepeat
    evalStateT sendLastMsgs (Nothing, sr, hMsg, rMsg, r, empty, dlog)

{-To DO
-- keyboard forming on every makeCallbackQuery call
-- unit-tests, readme
-}

--                      lastUpdtID     request help    repeat  r    repeatMap            dlog
sendLastMsgs :: StateT (Maybe Integer, String, T.Text, T.Text, Int, HashMap Integer Int, Bool) IO ()
sendLastMsgs = do
    (offset, _, _, _, _, _, _) <- get
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
    
    (_, sr, hMsg, rMsg, r, chatsRepeat, dlog) <- get    
    put (lastUpdtID, sr, hMsg, rMsg, r, chatsRepeat, dlog)
    sendLastMsgs        

--                                                chatID   msgText     queryID chatID   button
sortJResponse :: JResponse -> Maybe Integer -> ([(Integer, T.Text)], [(String, Integer, String)])
sortJResponse jresponse offset = foldr go ([],[]) (seeIfAreOld offset $ result jresponse) where
    seeIfAreOld Nothing xs = xs
    seeIfAreOld _ [] = []
    seeIfAreOld _ xs = tail xs
    go (Update _ a b) (msgs,cbs) = (getMsg a ++ msgs, getCb b ++ cbs)
    getMsg (Just (Message _ (Chat chatID) (Just txt))) = [(chatID, txt)]
    getMsg _ = []
    getCb (Just (CallbackQuery queryID (Just msg) (Just btnPressed))) = 
        [(queryID, (id :: Chat -> Integer) $ (chat :: Message -> Chat) msg, btnPressed)]
    getCb _ = []

findLastUpdtID :: [Update] -> Maybe Integer -> Maybe Integer
findLastUpdtID [] offset = offset
findLastUpdtID us _ = Just $ update_id $ last us

