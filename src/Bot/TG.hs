{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.TG where

import qualified Data.ByteString.UTF8 as BS
import qualified Data.Aeson as A

import qualified Logger as Logger
import qualified Bot as Bot
import           Bot.TGLib

data Config = Config
    {token :: String}
    deriving Show
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON BotTelegram.Config" $ \o -> Config
        <$> o A..: "token"

data Handle = Handle
    { hConfig :: Config
    , hBot    :: Bot.Handle
    , hLogger :: Logger.Handle
    }
    deriving Show

type Users = [Bot.User]

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = f $ Handle conf hBot hLog

sendHelp :: Handle -> TGMessage -> IO ()
sendHelp (Handle {..}) (TGMessage {..}) = do
    getResponseFromAPI
        (token hConfig) 
        (tgSendMessage 
            (tgChatId tgMessageChat)
            (Bot.helpText $ Bot.hConfig hBot))
    Logger.info hLogger "Help text sent"

sendRepeat :: Handle -> Users -> TGMessage -> IO Bool
sendRepeat (Handle {..}) users (TGMessage {..}) = do
    getResponseFromAPI
        (token hConfig) 
        (tgSendButtons 
            (tgChatId tgMessageChat)
            (
                (Bot.repeatText1 $ Bot.hConfig hBot)
                ++ (show $ Bot.repeatDefault $ Bot.hConfig hBot) ++ "\r\n"
                ++ (Bot.repeatText2 $ Bot.hConfig hBot)
            )
            (TGKeyBoard 
                [[TGButton "1",TGButton "2",TGButton "3",TGButton "4",TGButton "5"]]
                True
                True
                False))
    Logger.info hLogger "Repeat command sent"
    return True

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

todo :: Handle -> Users -> Bool -> Integer -> IO ()
todo htg users com offset = do
    json <- getResponseFromAPI (token $ hConfig htg) $ tgGetUpdates 60 offset
    Logger.debug (hLogger htg) $ show json
    let Just resp = A.decodeStrict json
    case responseResult resp of
        [] -> todo htg users com offset
        _  -> mapM_ (whatUpdate htg users com) $ responseResult resp

configTest = Config { token = "1601854063:AAHSq7CxULiYMUyHiSiwS1ByOVA-kUg_ejU" }
handleTest = Handle
    { hConfig = configTest
    , hBot = Bot.handleTest
    , hLogger = Logger.handleTest
    }