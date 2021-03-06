{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VK where

import           Control.Monad   (foldM, replicateM_)
import qualified Data.Aeson      as A
import           Data.Maybe      (fromMaybe)
import           Data.Time.Clock

import qualified Bot
import           Bot.VK.Methods
import           Bot.VK.Types
import qualified Logger


data Handle = Handle
    { hConfig :: Config
    , hBot    :: Bot.Handle
    , hLogger :: Logger.Handle
    , hServer :: LongPollServer
    }
    deriving (Show,Eq)

data State = State
    { users    :: Bot.Users
    , server   :: LongPollServer
    , response :: Maybe Response
    , offset   :: String
    } deriving (Show,Eq)

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = do
    respsrv <- getServerAddress conf
    case respsrv of
        Left err   -> do
            Logger.error hLog $ show err
            return ()
        Right serv -> do
            f $ Handle conf hBot hLog serv

copyNewMessage :: Handle -> Either Bot.Exceptions State -> IO (Either Bot.Exceptions State)
copyNewMessage _ (Left err) = return $ Left err
copyNewMessage handle (Right state@State {..}) = do
    case response of
        Nothing -> return $ Right state
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            foldM (copier handle) (Right state) newUpdates

copier :: Handle -> Either Bot.Exceptions State -> Update -> IO (Either Bot.Exceptions State)
copier _ (Left err) _ = return $ Left err
copier Handle {..} (Right state@State {..}) Update {..} = do
    case upObject of
        Nothing -> return $ Right state
        Just obj -> do
            let repeats = Bot.getRepeat
                            users
                            (Bot.repeatDefault $ Bot.hConfig hBot)
                            (fromID $ message obj)
            let msg = text $ message obj
            replicateM_ repeats $ do
                t <- getCurrentTime
                let rnd = diffTimeToPicoseconds (utctDayTime t) `mod` (10 ^ (9 :: Integer))
                json <- getResponseFromAPI
                            (copyMessage
                            hConfig
                            obj
                            (fromID $ message obj)
                            rnd
                            msg)
                Logger.debug hLogger $ show json
            Logger.info hLogger $ "Sent message ECHO to user id " ++
                          show (fromID $ message obj) ++
                          " " ++ show repeats ++ " times"
            let newState = delUpdate state eventId
            return $ Right newState

setRepeat :: Handle -> Either Bot.Exceptions State -> IO (Either Bot.Exceptions State)
setRepeat _ (Left err) = return $ Left err
setRepeat handle (Right state@State {..}) = do
    case response of
        Nothing -> return $ Right state
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needSet = filter
                    (\u -> case upObject u of
                        Nothing -> False
                        Just obj -> text (message obj) `elem` ["1","2","3","4","5"] &&
                            Bot.getCommand users
                                       (fromID $ message obj)) newUpdates
            foldM (setter handle) (Right state) needSet

setter :: Handle -> Either Bot.Exceptions State -> Update -> IO (Either Bot.Exceptions State)
setter _ (Left err) _ = return $ Left err
setter Handle {..} (Right state@State {..}) Update {..} = do
    case upObject of
        Nothing -> return $ Right state
        Just obj -> do
            let newUsers = Bot.putRepeat users
                                 (Bot.User { uName = ""
                                           , uID = fromID $ message obj
                                           , uRep = read $ text $ message obj
                                           , uSentRep = False })
            Logger.debug hLogger $ show newUsers
            let newSUsers = state {users = newUsers}
            Logger.info hLogger $ "Set repeat for user id :" ++ (show . fromID . message) obj ++
                          " to " ++ text (message obj)
            let newState = delUpdate newSUsers eventId
            return $ Right newState

sendRepeat :: Handle -> Either Bot.Exceptions State -> IO (Either Bot.Exceptions State)
sendRepeat _ (Left err) = return $ Left err
sendRepeat handle (Right state@State {..}) = do
    case response of
        Nothing -> return $ Right state
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needRep = filter (\u -> fmap (text . message) (upObject u) == Just "/repeat") newUpdates
            foldM (setCommand handle) (Right state) needRep

setCommand :: Handle -> Either Bot.Exceptions State -> Update -> IO (Either Bot.Exceptions State)
setCommand _ (Left err) _ = return $ Left err
setCommand handle@Handle {..} (Right state@State {..}) upd = do
    case upObject upd of
        Nothing -> return $ Right state
        Just obj -> do
            let user = Bot.User { uName = ""
                        , uID = fromID $ message obj
                        , uRep = Bot.repeatDefault $ Bot.hConfig hBot
                        , uSentRep = True }
            let repState = state {users = Bot.setCommand users user}
            let repMessage = Bot.repeatText1 (Bot.hConfig hBot)
                     ++ show (Bot.getRepeat
                                users
                                (Bot.repeatDefault $ Bot.hConfig hBot)
                                (fromID $ message obj)) ++ "\r\n"
                    ++ Bot.repeatText2 (Bot.hConfig hBot)
            sender handle True repMessage "REPEAT" (Right repState) upd

sendHelp :: Handle -> Either Bot.Exceptions State -> IO (Either Bot.Exceptions State)
sendHelp _ (Left err) = return $ Left err
sendHelp handle@Handle {..} (Right state@State {..}) = do
    case response of
        Nothing -> return $ Right state
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needHelp = filter (\u -> fmap (text . message) (upObject u) == Just "/help") newUpdates
            let helpMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender handle False helpMessage "HELP") (Right state) needHelp

sender :: Handle -> Bool -> String -> String -> Either Bot.Exceptions State -> Update
    -> IO (Either Bot.Exceptions State)
sender _ _ _ _ (Left err) _ = return $ Left err
sender Handle {..} kb textMes textLog (Right state) Update {..} = do
    case upObject of
        Nothing -> return $ Right state
        Just obj -> do
            t <- getCurrentTime
            let rnd = diffTimeToPicoseconds (utctDayTime t) `mod` (10 ^ (9 :: Integer))
            json <- getResponseFromAPI
                (sendMessage
                hConfig
                (fromID $ message obj)
                rnd
                textMes
                kb)
            Logger.debug hLogger $ show json
            Logger.info hLogger $ "Sent message " ++ textLog ++
                          " to user id " ++ show (fromID $ message obj)
            let newState = delUpdate state eventId
            return $ Right newState

delUpdate :: State -> String -> State
delUpdate state@State {..} uid = state {response = newResp} where
    newResp = case response of
        Nothing -> response
        Just resp -> let
            ups = filter (\u -> upType u == MessageNew) $ updates resp
            newUpdate = filter (\u -> eventId u /= uid) ups
            in Just resp {updates = newUpdate}

getResponse :: Handle -> State -> IO (Either Bot.Exceptions State)
getResponse handle@Handle {..} state@State {..} = do
    eJson <- getUpdates server (timeout hConfig) offset
    case eJson of
        Left err   -> return $ Left err
        Right json -> do
            Logger.debug hLogger $ show json
            case A.decodeStrict json of
                Just failresp -> case failed failresp of
                    2 -> do
                        Logger.info hLogger "The key expired"
                        getNewServer handle state
                    3 -> do
                        Logger.warning hLogger "Information is lost"
                        getNewServer handle state
                    1 -> do
                        Logger.warning hLogger "The event history is outdated or has been partially lost"
                        return $ Right state {offset = fromMaybe "" $ fts failresp
                             ,response = Nothing}
                    _ -> return $ Left $ Bot.FatalError "Unknown failed response from VK server"
                Nothing -> do
                    let resp = A.decodeStrict json
                    Logger.debug hLogger $ show resp
                    case resp of
                        Nothing   -> return $ Right state { response = Nothing }
                        Just r    -> return $ Right state { response = resp
                            , offset = ts r}

getServerAddress :: Config -> IO (Either Bot.Exceptions LongPollServer)
getServerAddress conf = do
    eJson <- getResponseFromAPI $ getServer conf
    case eJson of
        Left err -> return $ Left err
        Right json -> do
            case getResp <$> A.decodeStrict json of
                Nothing   -> return $ Left $ Bot.FatalError "No answear from LongPoll server VK"
                Just srv -> return $ Right srv

getNewServer :: Handle -> State -> IO (Either Bot.Exceptions State)
getNewServer Handle {..} state = do
    serv <- getServerAddress hConfig
    case serv of
        Left err   -> return $ Left err
        Right srv -> do
            return $ Right state { server = srv
                        , response = Nothing
                        , offset = startTs srv}

todo :: Handle -> State -> IO (Either Bot.Exceptions State)
todo handle state =
    getResponse handle state
    >>= sendHelp handle
    >>= sendRepeat handle
    >>= setRepeat handle
    >>= copyNewMessage handle

run :: Handle -> State -> IO ()
run handle@Handle {..} state = do
    result <- todo handle state
    case result of
        Left err -> do
            Logger.error hLogger $ show err
        Right newstate -> run handle newstate
