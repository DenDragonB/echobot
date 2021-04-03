{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VK where

import qualified Data.ByteString.UTF8 as BS
import qualified Data.Aeson as A
import           Control.Monad (foldM,replicateM_)
import           Data.Time.Clock 

import qualified Logger
import qualified Bot
import           Bot.VK.Types
import           Bot.VK.Methods


data Handle = Handle
    { hConfig  :: Config
    , hBot     :: Bot.Handle
    , hLogger  :: Logger.Handle
    , server   :: LongPollServer
    , offset   :: String
    , response :: Maybe Response
    }
    deriving Show

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = do
    json <- getResponseFromAPI $ getServer conf
    let respsrv = A.decodeStrict json :: Maybe RespServer
    case respsrv of
        Nothing   -> do
            Logger.error hLog "No answear from LongPoll server VK"
            return ()
        Just serv -> do
            f $ Handle conf hBot hLog (getResp serv) (startTs $ getResp serv) Nothing

copyNewMessage :: Handle -> IO Handle
copyNewMessage handle@Handle {..} = do
    case response of
        Nothing -> return handle
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            foldM copier handle newUpdates    

copier :: Handle -> Update -> IO Handle
copier handle@Handle {..} NewMessage {..} = do
    let repeats = Bot.getRepeat
                            (Bot.users hBot)
                            (Bot.repeatDefault $ Bot.hConfig hBot)
                            (fromID $ message upObject) 
    let msg = text $ message upObject
    replicateM_ repeats $ do
                t <- getCurrentTime
                let rnd = diffTimeToPicoseconds (utctDayTime t) `mod` (10 ^ 9)
                json <- getResponseFromAPI
                            (copyMessage
                            hConfig
                            upObject
                            (fromID $ message upObject) 
                            rnd
                            msg)
                Logger.debug hLogger $ show json
    Logger.info hLogger $ "Sent message ECHO to user id " ++ 
                          show (fromID $ message upObject) ++
                          " " ++ show repeats ++ " times"
    let newHandle = delUpdate handle eventId
    return newHandle

setRepeat :: Handle -> IO Handle
setRepeat handle@Handle {..} = do
    case response of
        Nothing -> return handle
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needSet = filter 
                    (\u -> (text . message . upObject) u `elem` ["1","2","3","4","5"] &&
                            Bot.getCommand (Bot.users hBot)
                                       (fromID $ message $ upObject u)) newUpdates
            foldM setter handle needSet 

setter :: Handle -> Update -> IO Handle
setter handle@Handle {..} NewMessage {..} = do
    let newUsers = Bot.putRepeat (Bot.users hBot) 
                                 (Bot.User { uName = ""
                                           , uID = fromID $ message upObject
                                           , uRep = read $ text $ message upObject
                                           , uSentRep = False })
    Logger.debug hLogger $ show newUsers
    let newBot = hBot {Bot.users = newUsers}
    let newHUsers = handle {hBot = newBot}
    Logger.info hLogger $ "Set repeat for user id :" ++ (show . fromID . message) upObject ++
                          " to " ++ text (message upObject)
    let newHandle = delUpdate newHUsers eventId
    return newHandle

sendRepeat :: Handle -> IO Handle
sendRepeat handle@Handle {..} = do
    case response of
        Nothing -> return handle
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needRep = filter (\u -> (text . message . upObject) u == "/repeat") newUpdates
            foldM setCommand handle needRep

setCommand :: Handle -> Update -> IO Handle
setCommand handle@Handle {..} upd = do
    let user = Bot.User { uName = ""
                        , uID = fromID $ message $ upObject upd
                        , uRep = Bot.repeatDefault $ Bot.hConfig hBot
                        , uSentRep = True }
    let repHandle = handle {hBot = hBot {Bot.users = Bot.setCommand (Bot.users hBot) user}}
    let repMessage = Bot.repeatText1 (Bot.hConfig hBot)
                     ++ show (Bot.getRepeat 
                                (Bot.users hBot)
                                (Bot.repeatDefault $ Bot.hConfig hBot)
                                (fromID $ message $ upObject upd)) ++ "\r\n"
                    ++ Bot.repeatText2 (Bot.hConfig hBot)
    sender True repMessage "REPEAT" repHandle upd

sendHelp :: Handle -> IO Handle
sendHelp handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
        Nothing -> return handle
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needHelp = filter (\u -> (text . message . upObject) u == "/help") newUpdates
            let helpMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender False helpMessage "HELP") handle needHelp

sender :: Bool -> String -> String -> Handle -> Update -> IO Handle
sender kb textMes textLog handle@Handle {..} NewMessage {..} = do
    t <- getCurrentTime
    let rnd = diffTimeToPicoseconds (utctDayTime t) `mod` (10 ^ 9)
    json <- getResponseFromAPI
                (sendMessage
                hConfig
                (fromID $ message upObject) 
                rnd
                textMes
                kb)
    Logger.debug hLogger $ show json
    Logger.info hLogger $ "Sent message " ++ textLog ++ 
                          " to user id " ++ show (fromID $ message upObject)
    let newHandle = delUpdate handle eventId
    return newHandle

delUpdate :: Handle -> String -> Handle
delUpdate handle@Handle {..} id = handle {response = newResp} where
    newResp = case response of
        Nothing -> response
        Just resp -> let
            ups = filter (\u -> upType u == MessageNew) $ updates resp
            newUpdate = filter (\u -> eventId u /= id) ups
            in Just resp {updates = newUpdate}

getResponse :: Handle -> IO Handle
getResponse handle@Handle {..} = do
    json <- getUpdates server (timeout hConfig) offset
    Logger.debug hLogger $ show json
    let response = A.decodeStrict json
    Logger.debug hLogger $ show response
    case response of
        Nothing   -> return handle { response = Nothing }
        Just resp -> return handle { response = response 
                                   , offset = ts resp}

todo :: Handle -> IO Handle
todo handle = 
    getResponse handle 
    >>= sendHelp
    >>= sendRepeat
    >>= setRepeat
    >>= copyNewMessage
    >>= todo

configTest = Config { token = "" 
                    , groupVKId = 1
                    , timeout = 60
                    }
handleTest = Handle
    { hConfig = configTest
    , hBot = Bot.handleTest
    , hLogger = Logger.handleTest
    , server = LPServer "" "" ""
    , offset = "0"
    , response = Nothing
    }