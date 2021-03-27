{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.TG where

import qualified Data.ByteString.UTF8 as BS
import qualified Data.Aeson as A

import qualified Logger as Logger
import qualified Bot as Bot
import           Bot.TGLib

data Config = Config
    { token   :: String
    , timeout :: Integer
    }
    deriving Show
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON BotTelegram.Config" $ \o -> Config
        <$> o A..: "token"
        <$> o A..: "timeout"

data Handle = Handle
    { hConfig  :: Config
    , hBot     :: Bot.Handle
    , hLogger  :: Logger.Handle
    , command  :: Bool
    , offset   :: Integer
    , response :: Maybe TGResponse
    }
    deriving Show

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = f $ Handle conf hBot hLog False 0

setRepeat :: Handle -> Users -> TGMessage -> IO Users
setRepeat (Handle {..}) users (TGMessage {..}) = do
    let newUser = Bot.User 
                      { Bot.uName = tgUserName tgMessageFrom
                      , Bot.uID   = tgUserId tgMessageFrom
                      , Bot.uRep  = read tgMessageText
                      }
    Logger.info hLogger $ "Setting the number of repetitions for the user " ++ Bot.uName newUser
    return $ Bot.putRepeat users newUser 

copyMessage :: Handle -> Users -> TGMessage -> IO ()
copyMessage (Handle {..}) users (TGMessage {..}) = do
    let rep = Bot.getRepeat users (Bot.repeatDefault $ Bot.hConfig hBot) (tgUserId tgMessageFrom)
    let ms = take rep $ repeat $ tgCopyMessage (tgChatId tgMessageChat)
                                               (tgChatId tgMessageChat)
                                                tgMessageId
    mapM_ (getResponseFromAPI (token hConfig)) ms
    Logger.info hLogger "Echo sent"

whatUpdate :: Handle -> Users -> Bool -> TGUpdate -> IO ()
whatUpdate htg users com (TGUpMessge {..}) = do
    Logger.info (hLogger htg) ("Received a message from the user "
        ++ (tgUserName $ tgMessageFrom tgMessage)
        ++ " (id:" ++ (show $ tgUserId $tgMessageFrom tgMessage)
        ++ ")")
    case tgMessageText tgMessage of
        "/repeat" -> do newCom <- sendRepeat htg users tgMessage
                        todo htg users newCom $ succ tgUpdateId
        "/help"   -> do sendHelp htg tgMessage
                        todo htg users com $ succ tgUpdateId
        "/stop"   -> do Logger.info (hLogger htg) $ "The bot was stopped by user "
                                         ++ (tgUserName $ tgMessageFrom tgMessage)
                        return ()
        _         -> if com && tgMessageText tgMessage `elem` ["1","2","3","4","5"] 
                         then do newUsers <- setRepeat htg users tgMessage
                                 todo htg newUsers False $ succ tgUpdateId
                         else do copyMessage htg users tgMessage 
                                 todo htg users com $ succ tgUpdateId
whatUpdate htg users com update = todo htg users com $ succ $ tgUpdateId update

sendRepeat :: Monad m => Handle -> m Handle
sendRepeat handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
        Nothing -> return handle
        Just resp -> do
            let updates = filter (tgUpType == UPMessage) responseResult resp
            let needHelp = filter (tgMessageText tgMessage == "/repeat") updates
            let repeatMessage = Bot.repeatText1 $ Bot.hConfig hBot
                                ++ show $ Bot.repeatDefault $ Bot.hConfig hBot ++ "\r\n"
                                ++ Bot.repeatText2 $ Bot.hConfig hBot
            foldM (senderKB repeatMessage "REPEAT") handle needHelp


sendHelp :: Monad m => Handle -> m Handle
sendHelp handle@Handle {..} = do
    case response of
        -- There are no or bad response from api
        Nothing -> return handle
        Just resp -> do
            let updates = filter (tgUpType == UPMessage) responseResult resp
            let needHelp = filter (tgMessageText tgMessage == "/help") updates
            let helpMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender helpMessage "HELP") handle needHelp


getUpdates :: Monad m => Handle -> m Handle
getUpdates handle@Handle {..} = do
    json <- getResponseFromAPI (token hConfig) $ getUpdates (timeout hConfig) offset
    Logger.debug hLogger $ show json
    return handle {response = A.decodeStrict json}

sender :: Monad m => String -> Handle -> TGMessage -> m Handle
sender textMes textLog handle@Handle {..} TGMessage {..} = do
    getResponseFromAPI
        (token hConfig) 
        (tgSendMessage 
            (tgChatId tgMessageChat)
            textMes)
    Logger.info hLogger $ "Sent message " ++ textLog ++ " to " ++ tgUserName tgMessageFrom
    return handle

senderKB :: Monad m => String -> Handle -> TGMessage -> m Handle
senderKB textMes textLog handle@Handle {..} TGMessage {..} = do
    getResponseFromAPI
        (token hConfig) 
        (tgSendButtons 
            (tgChatId tgMessageChat)
            textMes
            (TGKeyBoard 
                [[TGButton "1",TGButton "2",TGButton "3",TGButton "4",TGButton "5"]]
                True
                True
                False))
    Logger.info hLogger $ "Sent message " ++ textLog ++ " to " ++ tgUserName tgMessageFrom
    let user = Bot.User { uName = tgUserName tgMessageFrom
                        , uID = tgUserName tgMessageFrom
                        , uRep = Bot.repeatDefault $ Bot.hConfig hBot
                        , uSentRep = True}
    let newHandle = handle {hBot = hBot {Bot.users = setCommand (Bot.users hBot) user}}
    Logger.debug hLogger $ "Set command REPEAT to " ++ tgUserName tgMessageFrom
    return newHandle

todo :: Handle -> IO Handle
todo handle = 
    getUpdates handle >>=
    sendHelp >>=
    sendRepeat >>=
    todo

configTest = Config { token = "1601854063:AAHSq7CxULiYMUyHiSiwS1ByOVA-kUg_ejU" }
handleTest = Handle
    { hConfig = configTest
    , hBot = Bot.handleTest
    , hLogger = Logger.handleTest
    }