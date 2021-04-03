{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VK where

import qualified Data.ByteString.UTF8 as BS
import qualified Data.Aeson as A
import           Control.Monad (foldM,replicateM_)

import qualified Logger
import qualified Bot
import           Bot.VKLib
import           Data.Time.Clock 

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

--copyMessage :: Monad m => Handle -> m Handle
copyNewMessage :: Handle -> IO Handle
copyNewMessage handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
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
{-
--setRepeat :: Monad m => Handle -> m Handle
setRepeat :: Handle -> IO Handle
setRepeat handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
        Nothing -> return handle
        Just resp -> do
            let updates = filter (\u -> tgUpType u == UPMessage) $ responseResult resp
            let needSet = filter 
                    (\u -> tgMessageText (tgMessage u) `elem` ["1","2","3","4","5"] &&
                        Bot.getCommand (Bot.users hBot)
                                       (tgUserId $ tgMessageFrom $ tgMessage u)) updates
            foldM setter handle needSet 

--setter :: Monad m => Handle -> TGUpdate -> m Handle
setter :: Handle -> TGUpdate -> IO Handle
setter handle@Handle {..} TGUpMessge {..} = do
    let newUsers = Bot.putRepeat (Bot.users hBot) 
                                 (Bot.User { uName = tgUserName $ tgMessageFrom tgMessage
                                           , uID = tgUserId $ tgMessageFrom tgMessage
                                           , uRep = read $ tgMessageText tgMessage
                                           , uSentRep = False })
    Logger.debug hLogger $ show newUsers
    let newBot = hBot {Bot.users = newUsers}
    let newHUsers = handle {hBot = newBot}
    Logger.info hLogger $ "Set repeat for " ++ tgUserName (tgMessageFrom tgMessage) ++
                          " to " ++ read (tgMessageText tgMessage)
    let newHandle = delUpdate newHUsers tgUpdateId
    return newHandle
-}
--sendRepeat :: Monad m => Handle -> m Handle
sendRepeat :: Handle -> IO Handle
sendRepeat handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
        Nothing -> return handle
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needRep = filter (\u -> (text . message . upObject) u == "/repeat") newUpdates
            let repMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender False repMessage "REPEAT") handle needRep

-- sendHelp :: Monad m => Handle -> m Handle
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

delUpdate :: Handle -> String -> Handle
delUpdate handle@Handle {..} id = handle {response = newResp} where
    newResp = case response of
        -- There are no or bad response from api
        Nothing -> response
        Just resp -> let
            ups = filter (\u -> upType u == MessageNew) $ updates resp
            newUpdate = filter (\u -> eventId u /= id) ups
            in Just resp {updates = newUpdate}

--getUpdates :: Monad m => Handle -> m Handle
getResponse :: Handle -> IO Handle
getResponse handle@Handle {..} = do
    json <- getUpdates server (timeout hConfig) offset
    Logger.debug hLogger $ show json
    let response = A.decodeStrict json
    -- Logger.debug hLogger $ show response
    case response of
        Nothing   -> return handle { response = Nothing }
        Just resp -> return handle { response = response 
                                   , offset = ts resp}

--sender :: Monad m => String -> String -> Handle -> TGUpdate -> m Handle
sender :: Bool -> String -> String -> Handle -> Update -> IO Handle
sender rep textMes textLog handle@Handle {..} NewMessage {..} = do
    let repeats = if not rep then 1 
                  else Bot.getRepeat
                            (Bot.users hBot)
                            (Bot.repeatDefault $ Bot.hConfig hBot)
                            (fromID $ message upObject) 
    let msg = case textMes of
                "" -> text $ message upObject
                _  -> textMes
    replicateM_ repeats $ do
                t <- getCurrentTime
                let rnd = diffTimeToPicoseconds (utctDayTime t) `mod` (10 ^ 9)
                json <- getResponseFromAPI
                            (sendMessage
                            hConfig
                            (fromID $ message upObject) 
                            rnd
                            msg)
                Logger.debug hLogger $ show json
    Logger.info hLogger $ "Sent message " ++ textLog ++ 
                          " to user id " ++ show (fromID $ message upObject) ++
                          " " ++ show repeats ++ " times"
    let newHandle = delUpdate handle eventId
    return newHandle
{-
--senderKB :: Monad m => String -> String -> Handle -> TGUpdate -> m Handle
senderKB :: String -> Handle -> TGUpdate -> IO Handle
senderKB textLog handle@Handle {..} TGUpMessge {..} = do
    let textMes = Bot.repeatText1 (Bot.hConfig hBot)
                    ++ show (Bot.getRepeat 
                                (Bot.users hBot)
                                (Bot.repeatDefault $ Bot.hConfig hBot)
                                (tgUserId $ tgMessageFrom tgMessage)) ++ "\r\n"
                    ++ Bot.repeatText2 (Bot.hConfig hBot)
    getResponseFromAPI
        (token hConfig) 
        (tgSendButtons 
            (tgChatId $ tgMessageChat tgMessage)
            textMes
            (TGKeyBoard 
                [[TGButton "1",TGButton "2",TGButton "3",TGButton "4",TGButton "5"]]
                True
                True
                False))
    Logger.info hLogger $ "Sent message " ++ textLog ++ " to " ++ tgUserName (tgMessageFrom tgMessage)
    let user = Bot.User { uName = tgUserName $ tgMessageFrom tgMessage
                        , uID = tgUserId $ tgMessageFrom tgMessage
                        , uRep = Bot.repeatDefault $ Bot.hConfig hBot
                        , uSentRep = True}
    let newHandle = handle {hBot = hBot {Bot.users = Bot.setCommand (Bot.users hBot) user}}
    return $ delUpdate newHandle tgUpdateId
-}
todo :: Handle -> IO Handle
todo handle = 
    getResponse handle 
    >>= sendHelp
{-    >>= sendRepeat
    >>= setRepeat
-}
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