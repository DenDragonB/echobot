{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VK where

import           Control.Monad   (foldM, replicateM_)
import qualified Data.Aeson      as A
import           Data.Time.Clock

import qualified Bot
import           Bot.VK.Methods
import           Bot.VK.Types
import qualified Logger


data Handle = Handle
    { hConfig :: Config
    , hBot    :: Bot.Handle
    , hLogger :: Logger.Handle
    , server  :: LongPollServer
--    , offset   :: String
--    , response :: Maybe Response
    }
    deriving (Show,Eq)

data State = State
    { users    :: [Bot.User]
    , response :: Maybe Response
    , offset   :: String
    } deriving (Show,Eq)

withHandle :: Logger.Handle -> Bot.Handle -> Config -> (Handle -> IO ()) -> IO ()
withHandle hLog hBot conf f = do
    json <- getResponseFromAPI $ getServer conf
    let respsrv = A.decodeStrict json :: Maybe RespServer
    case respsrv of
        Nothing   -> do
            Logger.error hLog "No answear from LongPoll server VK"
            return ()
        Just serv -> do
            f $ Handle conf hBot hLog (getResp serv) --(startTs $ getResp serv) Nothing

copyNewMessage :: Handle -> State -> IO State
copyNewMessage handle state@State {..} = do
    case response of
        Nothing -> return state
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            foldM (copier handle) state newUpdates

copier :: Handle -> State -> Update -> IO State
copier Handle {..} state@State {..} Update {..} = do
    case upObject of
        Nothing -> return state
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
            return newState

setRepeat :: Handle -> State -> IO State
setRepeat handle state@State {..} = do
    case response of
        Nothing -> return state
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needSet = filter
                    (\u -> case upObject u of
                        Nothing -> False
                        Just obj -> text (message obj) `elem` ["1","2","3","4","5"] &&
                            Bot.getCommand users
                                       (fromID $ message obj)) newUpdates
            foldM (setter handle) state needSet

setter :: Handle -> State -> Update -> IO State
setter Handle {..} state@State {..} Update {..} = do
    case upObject of
        Nothing -> return state
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
            return newState

sendRepeat :: Handle -> State -> IO State
sendRepeat handle state@State {..} = do
    case response of
        Nothing -> return state
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needRep = filter (\u -> fmap (text . message) (upObject u) == Just "/repeat") newUpdates
            foldM (setCommand handle) state needRep

setCommand :: Handle -> State -> Update -> IO State
setCommand handle@Handle {..} state@State {..} upd = do
    case upObject upd of
        Nothing -> return state
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
            sender handle True repMessage "REPEAT" repState upd

sendHelp :: Handle -> State -> IO State
sendHelp handle@Handle {..} state@State {..} = do
    case response of
        Nothing -> return state
        Just resp -> do
            let newUpdates = filter (\u -> upType u == MessageNew) $ updates resp
            let needHelp = filter (\u -> fmap (text . message) (upObject u) == Just "/help") newUpdates
            let helpMessage = Bot.helpText $ Bot.hConfig hBot
            foldM (sender handle False helpMessage "HELP") state needHelp

sender :: Handle -> Bool -> String -> String -> State -> Update -> IO State
sender Handle {..} kb textMes textLog state Update {..} = do
    case upObject of
        Nothing -> return state
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
            return newState

delUpdate :: State -> String -> State
delUpdate state@State {..} uid = state {response = newResp} where
    newResp = case response of
        Nothing -> response
        Just resp -> let
            ups = filter (\u -> upType u == MessageNew) $ updates resp
            newUpdate = filter (\u -> eventId u /= uid) ups
            in Just resp {updates = newUpdate}

getResponse :: Handle -> State -> IO State
getResponse Handle {..} state@State {..} = do
    json <- getUpdates server (timeout hConfig) offset
    Logger.debug hLogger $ show json
    let resp = A.decodeStrict json
    Logger.debug hLogger $ show resp
    case resp of
        Nothing   -> return state { response = Nothing }
        Just r    -> return state { response = resp
                                  , offset = ts r}

todo :: Handle -> State -> IO State
todo handle state =
    getResponse handle state
    >>= sendHelp handle
    >>= sendRepeat handle
    >>= setRepeat handle
    >>= copyNewMessage handle
    >>= todo handle
