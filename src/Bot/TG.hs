{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.TG where

import           Control.Monad  (foldM, replicateM_)
import qualified Data.Aeson     as A
import           Data.Maybe     (fromMaybe, isJust)
import           GHC.Generics

import qualified Bot
import           Bot.TG.Methods
import           Bot.TG.Types
import qualified Logger

data Config = Config
    { token   :: String
    , timeout :: Integer
    } deriving (Show, Eq, Generic)
instance A.FromJSON Config

data Handle = Handle
    { hConfig :: Config
    , hBot    :: Bot.Handle
    , hLogger :: Logger.Handle
    } deriving (Show, Eq)

data State = State
    { users    :: Bot.Users
    , response :: Maybe Response
    , offset   :: Integer
    } deriving (Show, Eq)

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = f $ Handle conf hBot hLog

setRepeat :: Handle -> Either Bot.Exceptions State -> IO (Either Bot.Exceptions State)
setRepeat _ (Left err) = return $ Left err
setRepeat handle (Right state@State {..}) = do
    case response of
        Nothing -> return $ Right state
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needSet = filter
                    (\u -> fromMaybe "" (message u >>= messageText) `elem` ["1","2","3","4","5"] &&
                        Bot.getCommand
                            users
                            (maybe 0 tgUserId (message u >>= messageFrom))) updates
            foldM (setter handle) (Right state) needSet

setter :: Handle -> Either Bot.Exceptions State -> Update -> IO (Either Bot.Exceptions State)
setter _ (Left err) _ = return $ Left err
setter Handle {..} (Right state@State {..}) UpMessge {..} = do
    case message >>= messageFrom of
        Nothing -> return $ Right state
        Just us -> do
            let msg = fromMaybe (show $ Bot.repeatDefault $ Bot.hConfig hBot) (message >>= messageText)
            let newUsers = Bot.putRepeat
                    users
                    (Bot.User
                            { uName = tgUserName us
                            , uID = tgUserId us
                            , uRep = read msg
                            , uSentRep = False })
            Logger.debug hLogger $ show newUsers
            let newSUsers = state {users = newUsers}
            Logger.info hLogger $ "Set repeat for " ++ tgUserName us ++
                          " to " ++ msg
            let newState = delUpdate newSUsers updateId
            return $ Right newState

sendRepeat :: Handle -> Either Bot.Exceptions State -> IO (Either Bot.Exceptions State)
sendRepeat _ (Left err) = return $ Left err
sendRepeat handle (Right state@State {..}) = do
    case response of
        Nothing -> return $ Right state
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needHelp = filter (\u -> fromMaybe "" (message u >>= messageText) == "/repeat") updates
            foldM (senderRep handle) (Right state) needHelp

senderRep :: Handle -> Either Bot.Exceptions State -> Update -> IO (Either Bot.Exceptions State)
senderRep _ (Left err) _ = return $ Left err
senderRep handle@Handle {..} (Right state@State {..}) up = do
            let user = Bot.User
                            { Bot.uName = maybe "" tgUserName (message up >>= messageFrom)
                            , Bot.uID = maybe 0 tgUserId (message up >>= messageFrom)
                            , Bot.uRep = Bot.repeatDefault $ Bot.hConfig hBot
                            , Bot.uSentRep = True
                            }
            let repState = state {users = Bot.setCommand users user}
            let repMessage = Bot.repeatText1 (Bot.hConfig hBot)
                    ++ show (Bot.getRepeat
                                users
                                (Bot.repeatDefault $ Bot.hConfig hBot)
                                (maybe 0 tgUserId (message up >>= messageFrom))) ++ "\r\n"
                    ++ Bot.repeatText2 (Bot.hConfig hBot)
            sender handle True False repMessage "REPEAT" (Right repState) up

sendHelp :: Handle -> Either Bot.Exceptions State -> IO (Either Bot.Exceptions State)
sendHelp _ (Left err) = return $ Left err
sendHelp handle@Handle {..} (Right state@State {..}) = do
    case response of
        Nothing -> return $ Right state
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needHelp = filter (\u -> fromMaybe "" (message u >>= messageText) == "/help") updates
            let helpMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender handle False False helpMessage "HELP") (Right state) needHelp

delUpdate :: State -> Integer -> State
delUpdate state@State {..} uid = state {response = newResp} where
    newResp = case response of
        Nothing -> response
        Just resp -> let
            newUpdate = filter (\u -> updateId u /= uid) $ responseResult resp
            in Just resp {responseResult = newUpdate}

getResponse :: Handle -> State -> IO (Either Bot.Exceptions State)
getResponse Handle {..} state@State {..} = do
    eJson <- getResponseFromAPI (token hConfig) $ getUpdates (timeout hConfig) offset
    Logger.debug hLogger $ show eJson
    case eJson of
        Left err -> return $ Left err
        Right json -> do
            let resp = A.decodeStrict json
            case resp of
                Nothing   -> return $ Right state { response = Nothing }
                Just r -> do
                    case responseResult r of
                        [] -> return $ Right state { response = resp }
                        ups -> do
                            let oset = 1 + updateId (last ups)
                            return $ Right state { response = resp
                                  , offset = oset}

repeatMessage :: Handle -> Either Bot.Exceptions State -> IO (Either Bot.Exceptions State)
repeatMessage _ (Left err) = return $ Left err
repeatMessage handle (Right state@State {..}) = do
    case response of
        Nothing -> return $ Right state
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            foldM (sender handle False True "" "ECHO") (Right state) updates

sender :: Handle -> Bool -> Bool -> String -> String -> Either Bot.Exceptions State
    -> Update -> IO (Either Bot.Exceptions State)
sender _ _ _ _ _ (Left err) _ = return $ Left err
sender Handle {..} kb rep textMes textLog (Right state@State {..}) UpMessge {..} = do
    case message of
        Nothing -> return $ Right state
        Just mes -> do
            case messageFrom mes of
                Nothing -> return $ Right state
                Just us -> do
                    let repeats = if not rep then 1
                            else Bot.getRepeat
                                    users
                                    (Bot.repeatDefault $ Bot.hConfig hBot)
                                    (tgUserId us)
                    let msg = case textMes of
                            "" -> fromMaybe "" $ messageText mes
                            _  -> textMes
                    replicateM_ repeats $ do
                        _ <- getResponseFromAPI
                                (token hConfig)
                                (sendMessage
                                    (tgChatId $ messageChat mes)
                                    msg
                                    kb)
                        return ()
                    Logger.info hLogger $ "Sent message " ++ textLog ++
                            " to " ++ tgUserName us ++
                            " " ++ show repeats ++ " times"
                    let newState = delUpdate state updateId
                    return $ Right newState

todo :: Handle -> State -> IO (Either Bot.Exceptions State)
todo handle state =
    getResponse handle state >>=
    sendHelp handle >>=
    sendRepeat handle >>=
    setRepeat handle >>=
    repeatMessage handle

run :: Handle -> State -> IO ()
run handle@Handle {..} state = do
    result <- todo handle state
    case result of
        Left err -> do
            Logger.error hLogger $ show err
        Right newstate -> run handle newstate

