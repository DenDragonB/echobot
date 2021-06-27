{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.TG where

import           Control.Monad  (foldM, replicateM_)
import qualified Data.Aeson     as A
import           Data.Maybe     (fromMaybe, isJust)

import qualified Bot
import           Bot.TG.Methods
import           Bot.TG.Types
import qualified Logger

data Config = Config
    { token   :: String
    , timeout :: Integer
    } deriving (Show, Eq)
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON BotTelegram.Config" $ \o -> Config
        <$> o A..: "token"
        <*> o A..: "timeout"

data Handle = Handle
    { hConfig :: Config
    , hBot    :: Bot.Handle
    , hLogger :: Logger.Handle
--    , offset   :: Integer
--    , response :: Maybe Response
    } deriving (Show, Eq)

data State = State
    { users    :: [Bot.User]
    , response :: Maybe Response
    , offset   :: Integer
    } deriving (Show, Eq)

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = f $ Handle conf hBot hLog -- 0 Nothing

setRepeat :: Handle -> State -> IO State
setRepeat handle state@State {..} = do
    case response of
        Nothing -> return state
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needSet = filter
                    (\u -> fromMaybe "" (message u >>= messageText) `elem` ["1","2","3","4","5"] &&
                        Bot.getCommand
                            users
                            (maybe 0 tgUserId (message u >>= messageFrom))) updates
            foldM (setter handle) state needSet

setter :: Handle -> State -> Update -> IO State
setter Handle {..} state@State {..} UpMessge {..} = do
    case message >>= messageFrom of
        Nothing -> return state
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
            return newState

sendRepeat :: Handle -> State -> IO State
sendRepeat handle state@State {..} = do
    case response of
        Nothing -> return state
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needHelp = filter (\u -> fromMaybe "" (message u >>= messageText) == "/repeat") updates
            foldM (senderRep handle) state needHelp

senderRep :: Handle -> State -> Update -> IO State
senderRep handle@Handle {..} state@State {..} up = do
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
            sender handle True False repMessage "REPEAT" repState up

sendHelp :: Handle -> State -> IO State
sendHelp handle@Handle {..} state@State {..}= do
    case response of
        Nothing -> return state
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needHelp = filter (\u -> fromMaybe "" (message u >>= messageText) == "/help") updates
            let helpMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender handle False False helpMessage "HELP") state needHelp

delUpdate :: State -> Integer -> State
delUpdate state@State {..} uid = state {response = newResp} where
    newResp = case response of
        Nothing -> response
        Just resp -> let
            newUpdate = filter (\u -> updateId u /= uid) $ responseResult resp
            in Just resp {responseResult = newUpdate}

getResponse :: Handle -> State -> IO State
getResponse Handle {..} state@State {..} = do
    json <- getResponseFromAPI (token hConfig) $ getUpdates (timeout hConfig) offset
    Logger.debug hLogger $ show json
    let resp = A.decodeStrict json
    case resp of
        Nothing   -> return state { response = Nothing }
        Just r -> do
            case responseResult r of
                [] -> return state { response = resp }
                ups -> do
                    let oset = 1 + updateId (last ups)
                    return state { response = resp
                                  , offset = oset}

repeatMessage :: Handle -> State -> IO State
repeatMessage handle state@State {..} = do
    case response of
        Nothing -> return state
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            foldM (sender handle False True "" "ECHO") state updates

sender :: Handle -> Bool -> Bool -> String -> String -> State -> Update -> IO State
sender Handle {..} kb rep textMes textLog state@State {..} UpMessge {..} = do
    case message of
        Nothing -> return state
        Just mes -> do
            case messageFrom mes of
                Nothing -> return state
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
                    return newState

todo :: Handle -> State -> IO State
todo handle state =
    getResponse handle state >>=
    sendHelp handle >>=
    sendRepeat handle >>=
    setRepeat handle >>=
    repeatMessage handle >>=
    todo handle
