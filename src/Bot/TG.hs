{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.TG where

import qualified Data.Aeson as A
import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad (foldM,replicateM_)

import qualified Logger
import qualified Bot
import           Bot.TG.Types
import           Bot.TG.Methods

data Config = Config
    { token   :: String
    , timeout :: Integer
    }
    deriving Show
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON BotTelegram.Config" $ \o -> Config
        <$> o A..: "token"
        <*> o A..: "timeout"

data Handle = Handle
    { hConfig  :: Config
    , hBot     :: Bot.Handle
    , hLogger  :: Logger.Handle
    , offset   :: Integer
    , response :: Maybe Response
    }
    deriving Show

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = f $ Handle conf hBot hLog 0 Nothing

setRepeat :: Handle -> IO Handle
setRepeat handle@Handle {..} = do
    case response of
        Nothing -> return handle
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needSet = filter 
                    (\u -> fromMaybe "" (message u >>= messageText) `elem` ["1","2","3","4","5"] &&
                        Bot.getCommand 
                            (Bot.users hBot)
                            (maybe 0 tgUserId (message u >>= messageFrom))) updates
            foldM setter handle needSet 

setter :: Handle -> Update -> IO Handle
setter handle@Handle {..} UpMessge {..} = do
    case message >>= messageFrom of
        Nothing -> return handle
        Just us -> do
            let msg = fromMaybe (show $ Bot.repeatDefault $ Bot.hConfig hBot) (message >>= messageText)
            let newUsers = Bot.putRepeat 
                    (Bot.users hBot) 
                    (Bot.User 
                            { uName = tgUserName us
                            , uID = tgUserId us
                            , uRep = read msg
                            , uSentRep = False })
            Logger.debug hLogger $ show newUsers
            let newBot = hBot {Bot.users = newUsers}
            let newHUsers = handle {hBot = newBot}
            Logger.info hLogger $ "Set repeat for " ++ tgUserName us ++
                          " to " ++ msg
            let newHandle = delUpdate newHUsers updateId
            return newHandle

sendRepeat :: Handle -> IO Handle
sendRepeat handle@Handle {..} = do
    case response of
        Nothing -> return handle
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needHelp = filter (\u -> fromMaybe "" (message u >>= messageText) == "/repeat") updates
            foldM senderRep handle needHelp

senderRep :: Handle -> Update -> IO Handle
senderRep handle@Handle {..} up = do
            let user = Bot.User 
                            { Bot.uName = maybe "" tgUserName (message up >>= messageFrom) 
                            , Bot.uID = maybe 0 tgUserId (message up >>= messageFrom)
                            , Bot.uRep = Bot.repeatDefault $ Bot.hConfig hBot
                            , Bot.uSentRep = True 
                            }
            let repHandle = handle {hBot = hBot {Bot.users = Bot.setCommand (Bot.users hBot) user}}
            let repMessage = Bot.repeatText1 (Bot.hConfig hBot)
                    ++ show (Bot.getRepeat 
                                (Bot.users hBot)
                                (Bot.repeatDefault $ Bot.hConfig hBot)
                                (maybe 0 tgUserId (message up >>= messageFrom))) ++ "\r\n"
                    ++ Bot.repeatText2 (Bot.hConfig hBot)
            sender True False repMessage "REPEAT" repHandle up

sendHelp :: Handle -> IO Handle
sendHelp handle@Handle {..} = do
    case response of
        Nothing -> return handle
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            let needHelp = filter (\u -> fromMaybe "" (message u >>= messageText) == "/help") updates
            let helpMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender False False helpMessage "HELP") handle needHelp

delUpdate :: Handle -> Integer -> Handle
delUpdate handle@Handle {..} uid = handle {response = newResp} where
    newResp = case response of
        Nothing -> response
        Just resp -> let
            newUpdate = filter (\u -> updateId u /= uid) $ responseResult resp
            in Just resp {responseResult = newUpdate}

getResponse :: Handle -> IO Handle
getResponse handle@Handle {..} = do
    json <- getResponseFromAPI (token hConfig) $ getUpdates (timeout hConfig) offset
    Logger.debug hLogger $ show json
    let resp = A.decodeStrict json
    case resp of
        Nothing   -> return handle { response = Nothing }
        Just r -> do
            case responseResult r of
                [] -> return handle { response = resp }
                ups -> do
                    let oset = 1 + updateId (last ups)
                    return handle { response = resp 
                                  , offset = oset}

repeatMessage :: Handle -> IO Handle
repeatMessage handle@Handle {..} = do
    case response of
        Nothing -> return handle
        Just resp -> do
            let updates = filter (isJust . message) $ responseResult resp
            foldM (sender False True "" "ECHO") handle updates

sender :: Bool -> Bool -> String -> String -> Handle -> Update -> IO Handle
sender kb rep textMes textLog handle@Handle {..} UpMessge {..} = do
    case message of
        Nothing -> return handle
        Just mes -> do
            case messageFrom mes of
                Nothing -> return handle
                Just us -> do
                    let repeats = if not rep then 1
                            else Bot.getRepeat
                                    (Bot.users hBot)
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
                    let newHandle = delUpdate handle updateId
                    return newHandle

todo :: Handle -> IO Handle
todo handle = 
    getResponse handle >>=
    sendHelp >>=
    sendRepeat >>=
    setRepeat >>=
    repeatMessage >>=
    todo