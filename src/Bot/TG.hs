{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.TG where

import qualified Data.ByteString.UTF8 as BS
import qualified Data.Aeson as A
import           Control.Monad (foldM,replicateM_)

import qualified Logger
import qualified Bot
import           Bot.TGLib

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
    , response :: Maybe TGResponse
    }
    deriving Show

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = f $ Handle conf hBot hLog 0 Nothing

--copyMessage :: Monad m => Handle -> m Handle
copyMessage :: Handle -> IO Handle
copyMessage handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
        Nothing -> return handle
        Just resp -> do
            let updates = filter (\u -> tgUpType u == UPMessage) $ responseResult resp
            foldM (sender 0 "" "ECHO") handle updates

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

--sendRepeat :: Monad m => Handle -> m Handle
sendRepeat :: Handle -> IO Handle
sendRepeat handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
        Nothing -> return handle
        Just resp -> do
            let updates = filter (\u -> tgUpType u == UPMessage) $ responseResult resp
            let needHelp = filter (\u -> tgMessageText (tgMessage u) == "/repeat") updates
            foldM (senderKB "REPEAT") handle needHelp

-- sendHelp :: Monad m => Handle -> m Handle
sendHelp :: Handle -> IO Handle
sendHelp handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
        Nothing -> return handle
        Just resp -> do
            let updates = filter (\u -> tgUpType u == UPMessage) $ responseResult resp
            let needHelp = filter (\u -> tgMessageText (tgMessage u) == "/help") updates
            let helpMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender 1 helpMessage "HELP") handle needHelp

delUpdate :: Handle -> Integer -> Handle
delUpdate handle@Handle {..} id = handle {response = newResp} where
    newResp = case response of
        -- There are no or bad response from api
        Nothing -> response
        Just resp -> let
            updates = filter (\u -> tgUpType u == UPMessage) $ responseResult resp
            newUpdate = filter (\u -> tgUpdateId u /= id) updates
            in Just resp {responseResult = newUpdate}

--getUpdates :: Monad m => Handle -> m Handle
getResponse :: Handle -> IO Handle
getResponse handle@Handle {..} = do
    json <- getResponseFromAPI (token hConfig) $ tgGetUpdates (timeout hConfig) offset
    Logger.debug hLogger $ show json
    let response = A.decodeStrict json
    case response of
        Nothing   -> return handle { response = Nothing }
        Just resp -> do
            case responseResult resp of
                [] -> return handle { response = response }
                ups -> do
                    let offset = 1 + tgUpdateId (last ups)
                    return handle { response = response 
                                  , offset = offset}

--sender :: Monad m => String -> String -> Handle -> TGUpdate -> m Handle
sender :: Int -> String -> String -> Handle -> TGUpdate -> IO Handle
sender rep textMes textLog handle@Handle {..} TGUpMessge {..} = do
    let repeats = case rep of
                    1 -> rep
                    0 -> Bot.getRepeat
                            (Bot.users hBot)
                            (Bot.repeatDefault $ Bot.hConfig hBot)
                            (tgUserId $ tgMessageFrom tgMessage) 
    let msg = case textMes of
                "" -> tgMessageText tgMessage
                _  -> textMes
    replicateM_ repeats $  getResponseFromAPI
                (token hConfig) 
                (tgSendMessage 
                    (tgChatId $ tgMessageChat tgMessage)
                    msg)
    Logger.info hLogger $ "Sent message " ++ textLog ++ 
                          " to " ++ tgUserName (tgMessageFrom tgMessage) ++
                          " " ++ show repeats ++ " times"
    let newHandle = delUpdate handle tgUpdateId
    return newHandle

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

todo :: Handle -> IO Handle
todo handle = 
    getResponse handle >>=
    sendHelp >>=
    sendRepeat >>=
    setRepeat >>=
    copyMessage >>=
    todo
