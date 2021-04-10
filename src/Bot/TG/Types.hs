{-# LANGUAGE OverloadedStrings #-}

module Bot.TGLib where

import           Data.Aeson
--import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as BSLazy (toStrict)
import           Network.HTTP.Simple

-- Type of Response from Telegram bots API
data TGResponse = TGResponse {
    responseOk :: Bool,
    responseResult :: [TGUpdate] 
    }
    deriving Show
instance FromJSON TGResponse where
    parseJSON (Object response) = do
        Just resOk <- response .: "ok"
        Just resResult <- response .: "result"
        return TGResponse {responseOk = resOk,
                           responseResult = resResult}
instance ToJSON TGResponse where
    toJSON res = object [ "ok" .= responseOk res
                        , "result" .= responseResult res
                        ]

-- Type of Update response as describing in Telegram bots API
data TGUpdate =   TGUpMessge        {tgUpdateId :: Integer, tgUpType :: TGUpdateType,  tgMessage :: TGMessage}
                | TGUpInQuery       {tgUpdateId :: Integer, tgUpType :: TGUpdateType,  tgInlineQuery :: TGInlineQuery}
                | TGUpChoseInResult {tgUpdateId :: Integer, tgUpType :: TGUpdateType,  tgChosenInlineResult :: TGChosenInlineResult}
                | TGUpCallBackQuery {tgUpdateId :: Integer, tgUpType :: TGUpdateType,  tgCallbackQuery :: TGCallbackQuery}
                deriving Show
instance FromJSON TGUpdate where
    parseJSON (Object update) = do
        -- These fields are necessary
        Just upID       <- update .:  "update_id"
        upMes           <- update .:  "message"
        case upMes of
            Just res -> return TGUpMessge {tgUpdateId = upID, 
                                           tgUpType = UPMessage, 
                                           tgMessage = res}
            Nothing  -> do
                upInQuery  <- update .:  "inline_query"
                case upInQuery of
                    Just res -> return TGUpInQuery {tgUpdateId = upID, 
                                                    tgUpType = UPInlineQuery, 
                                                    tgInlineQuery = res}
                    Nothing  -> do
                        Just upChInRes  <- update .:  "chosen_inline_result"
                        case upChInRes of
                            Just res -> return TGUpChoseInResult {tgUpdateId = upID, 
                                                                  tgUpType = UPChosenInlineResult, 
                                                                  tgChosenInlineResult = res}
                            Nothing -> do
                                Just upCBQuerry <- update .:  "callback_query"
                                case upCBQuerry of
                                    Just res -> return TGUpCallBackQuery {tgUpdateId = upID, 
                                                                          tgUpType = UPCallbackQuery, 
                                                                          tgCallbackQuery = res}
                                    Nothing -> fail $ "parsing TGUpdate failed," ++
                                                       "unknown Update response"
instance ToJSON TGUpdate where
    toJSON upd = case tgUpType upd of
        UPMessage -> object [ "update_id" .= tgUpdateId upd
                            , "message"   .= tgMessage upd
                            ]
        UPInlineQuery -> object [ "update_id" .= tgUpdateId upd
                                , "inline_query"   .= tgInlineQuery upd
                                ]
        UPChosenInlineResult -> object [ "update_id" .= tgUpdateId upd
                                       , "chosen_inline_result"   .= tgChosenInlineResult upd
                                       ]
        UPCallbackQuery -> object [ "update_id" .= tgUpdateId upd
                                  , "callback_query"   .= tgCallbackQuery upd
                                  ]
data TGUpdateType = UPMessage 
                  | UPInlineQuery
                  | UPChosenInlineResult
                  | UPCallbackQuery
                  deriving (Show,Eq)

-- Type of Message as describing in Telegram bots API
data TGMessage = TGMessage {
    tgMessageId :: Integer,
    tgMessageFrom :: TGUser,
    tgMessageChat :: TGChat,
    tgMessageDate :: Integer,
    tgMessageText :: String,
    tgMessagePhotos :: [TGPhotoSize]
    }
    deriving Show
instance FromJSON TGMessage where
    parseJSON (Object message) = do
        -- These fields are necessary
        Just mesID        <- message .:  "message_id"
        Just mesDate      <- message .:  "date"
        Just mesChat      <- message .:  "chat"
        -- This fields are optional
        Just mesFrom      <- message .:? "from" .!= Just (TGUser 0 False "" "" "" "")
        Just mesText      <- message .:? "text" .!= Just ""
        Just mesPhoto     <- message .:? "photo" .!= Just []
        return TGMessage {tgMessageId     = mesID,
                          tgMessageDate   = mesDate,
                          tgMessageChat   = mesChat,
                          tgMessageFrom   = mesFrom,
                          tgMessageText   = mesText,
                          tgMessagePhotos = mesPhoto
                          }
instance ToJSON TGMessage where
    toJSON message = object [ "message_id" .= tgMessageId message
                            , "date"       .= tgMessageDate message
                            , "chat"       .= tgMessageChat message
                            , "from"       .= tgMessageFrom message
                            , "text"       .= tgMessageText message
                            , "photo"      .= tgMessagePhotos message
                            ]                

-- Type of Message as describing in Telegram bots API
data TGInlineQuery = TGInlineQuery {
    tgInQueryId     :: String,         -- Unique identifier for this query
    tgInQueryFrom   :: TGUser,         -- Sender
    tgInQueryLoc    :: String,         -- Опционально. Sender location, only for bots that request user location
    tgInQuery       :: String,         -- Text of the query
    tgInQueryOffset :: String          -- Offset of the results to be returned, can be controlled by the bot
    }
    deriving Show
instance FromJSON TGInlineQuery where
        parseJSON (Object inquery) = do
            -- These fields are necessary
            Just inqID        <- inquery .:  "id"
            Just inqFrom      <- inquery .:  "from"
            Just inqText      <- inquery .:  "query"
            Just inqOffset    <- inquery .:  "offset"
            -- This fields are optional
            Just inqLoc       <- inquery .:? "location" .!= Just ""
            return TGInlineQuery {tgInQueryId      = inqID,
                                  tgInQueryFrom    = inqFrom,
                                  tgInQueryLoc     = inqLoc,
                                  tgInQuery        = inqText,
                                  tgInQueryOffset  = inqOffset
                                  }
instance ToJSON TGInlineQuery where
    toJSON inq = object [ "id"       .= tgInQueryId inq
                        , "from"     .= tgInQueryFrom inq
                        , "location" .= tgInQueryLoc inq
                        , "query"    .= tgInQuery inq
                        , "offset"   .= tgInQueryOffset inq
                        ]  

-- Type of ChosenInlineResult as describing in Telegram bots API
data TGChosenInlineResult = TGChosenInlineResult {
    tgChosInResId    :: String,      -- The unique identifier for the result that was chosen
    tgChosInResFrom  :: TGUser,      -- The user that chose the result
    tgChosInResLoc   :: String,      -- Опционально. Sender location, only for bots that 
                                     -- require user location
    tgChosInResMesId :: String,      -- Опционально. Identifier of the sent inline message. 
                                     -- Available only if there is an inline keyboard attached
                                     -- to the message. Will be also received in callback queries
                                     -- and can be used to edit the message.
    tgChosInResQuery :: String       -- The query that was used to obtain the result
    }
    deriving Show
instance FromJSON TGChosenInlineResult where
    parseJSON (Object chosenInRes) = do
        -- These fields are necessary
        Just resID        <- chosenInRes .:  "result_id"
        Just resFrom      <- chosenInRes .:  "from"
        Just resText      <- chosenInRes .:  "query"
        -- This fields are optional
        Just resMesId     <- chosenInRes .:? "inline_message_id" .!= Just ""
        Just resLoc       <- chosenInRes .:? "location" .!= Just ""        
        return TGChosenInlineResult {tgChosInResId    = resID,
                                     tgChosInResFrom  = resFrom,
                                     tgChosInResLoc   = resLoc,
                                     tgChosInResMesId = resMesId,
                                     tgChosInResQuery = resText
                                     }
instance ToJSON TGChosenInlineResult where
    toJSON inRes = object [ "result_id"         .= tgChosInResId inRes
                          , "from"              .= tgChosInResFrom inRes
                          , "location"          .= tgChosInResLoc inRes
                          , "inline_message_id" .= tgChosInResMesId inRes
                          , "query"             .= tgChosInResQuery inRes
                          ]  

-- Type of CallbackQuery as describing in Telegram bots API
data TGCallbackQuery = TGCallbackQuery {
    tdCBQueryId   :: String,      -- Уникальный идентификатор запроса
    tdCBQueryFrom :: TGUser,      -- Отправитель
    tdCBQueryMes  :: TGMessage,   -- Опционально. Сообщение, к которому была привязана вызвавшая
                                  -- запрос кнопка. Обратите внимание: если сообщение слишком
                                  -- старое, содержание сообщения и дата отправки будут недоступны.
    tdCBQueryIMId :: String,      -- Опционально. Идентификатор сообщения, отправленного через
                                  -- вашего бота во встроенном режиме
    tdCBQueryData :: String       -- Данные, связанные с кнопкой. Обратите внимание, что клиенты
                                  -- могут добавлять свои данные в это поле.
    }
    deriving Show
instance FromJSON TGCallbackQuery where
    parseJSON (Object cbQuery) = do
        -- These fields are necessary
        Just queryID        <- cbQuery .:  "id"
        Just queryFrom      <- cbQuery .:  "from"
        Just queryText      <- cbQuery .:  "data"
        -- This fields are optional
        Just queryMes       <- cbQuery .:? "message" .!= 
                                      Just (TGMessage 0 
                                                     (TGUser 0 False "" "" "" "")
                                                     (TGChat 0 PrivateChat "" "" "" "" False)
                                                      0
                                                      ""
                                                      [])
        Just queryMesId     <- cbQuery .:? "inline_message_id" .!= Just ""        
        return TGCallbackQuery {tdCBQueryId   = queryID,
                                tdCBQueryFrom = queryFrom,
                                tdCBQueryMes  = queryMes,
                                tdCBQueryIMId = queryMesId,
                                tdCBQueryData = queryText
                                }
instance ToJSON TGCallbackQuery where
    toJSON cbQuery = object [ "id"                .= tdCBQueryId cbQuery
                            , "from"              .= tdCBQueryFrom cbQuery
                            , "message"           .= tdCBQueryMes cbQuery
                            , "inline_message_id" .= tdCBQueryIMId cbQuery
                            , "data"              .= tdCBQueryData cbQuery
                            ]  

-- Type of User as describing in Telegram bots API, added IsBot and Language fields
data TGUser = TGUser {
    tgUserId :: Integer,
    tgUserIsBot :: Bool,
    tgUserFirstName :: String,
    tgUserLastName :: String,
    tgUserName :: String,
    tgUserLanguage :: String}
    deriving Show
instance FromJSON TGUser where
    parseJSON (Object user) = do
        -- These fields are necessary
        Just userID        <- user .:  "id"
        Just userIsBot     <- user .:  "is_bot"
        Just userFirstName <- user .:  "first_name"
        -- This fields are optional
        Just userLastName  <- user .:? "last_name"     .!= Just ""
        Just userName      <- user .:? "username"      .!= Just ""
        Just userLang      <- user .:? "language_code" .!= Just ""
        return TGUser {tgUserId        = userID,
                       tgUserIsBot     = userIsBot,
                       tgUserFirstName = userFirstName,
                       tgUserLastName  = userLastName,
                       tgUserName      = userName,
                       tgUserLanguage  = userLang
                       }
instance ToJSON TGUser where
    toJSON user = object [ "id"            .= tgUserId user
                         , "is_bot"        .= tgUserIsBot user
                         , "first_name"    .= tgUserFirstName user
                         , "last_name"     .= tgUserLastName user
                         , "username"      .= tgUserIsBot user
                         , "language_code" .= tgUserLanguage user
                         ]                       

-- Type of Chat as describing in Telegram bots API
data TGChat = TGChat {
    tgChatId :: Integer,
    tgChatType :: TGChatType,
    tgChatTitle :: String,
    tgChatUserName :: String,
    tgChatFirstName :: String,
    tgChatLastName :: String,
    tgChatAllAdmins :: Bool}
    deriving Show
instance FromJSON TGChat where
    parseJSON (Object chat) = do
        -- These fields are necessary
        Just chatID        <- chat .:  "id"
        Just chatType      <- chat .:  "type"
        -- This fields are optional
        Just chatTitle     <- chat .:? "title"      .!= Just ""
        Just chatFirstName <- chat .:? "first_name" .!= Just ""
        Just chatLastName  <- chat .:? "last_name"  .!= Just ""
        Just chatName      <- chat .:? "username"   .!= Just ""
        Just chatAllAdmin  <- chat .:? "all_members_are_administrators" .!= Just False
        return TGChat {tgChatId        = chatID,
                       tgChatType      = chatType,
                       tgChatTitle     = chatTitle,
                       tgChatUserName  = chatName,
                       tgChatFirstName = chatFirstName,
                       tgChatLastName  = chatLastName,
                       tgChatAllAdmins = chatAllAdmin
                       }
instance ToJSON TGChat where
    toJSON chat = object [ "id"         .= tgChatId chat
                         , "type"       .= tgChatType chat
                         , "title"      .= tgChatTitle chat
                         , "first_name" .= tgChatFirstName chat
                         , "last_name"  .= tgChatLastName chat
                         , "username"   .= tgChatUserName chat
                         , "all_members_are_administrators" .= tgChatAllAdmins chat
                         ]              
data TGChatType = PrivateChat
                | GroupChat
                | SupergroupChat
                | ChannelChat
                | FailChat
                deriving Show
instance FromJSON TGChatType where
    parseJSON = withText "chatType" $ \s ->
        case s of
            "private"    -> return PrivateChat
            "group"      -> return GroupChat            
            "supergroup" -> return SupergroupChat
            "channel"    -> return ChannelChat
            _            -> fail $ "parsing TGChatType failed, unexpected " ++ show s ++
                                   " (expected \"private\", \"group\", \"supergroup\" or \"channel\")"
instance ToJSON TGChatType where
    toJSON PrivateChat    = "private"
    toJSON GroupChat      = "group"
    toJSON SupergroupChat = "supergroup"
    toJSON ChannelChat    = "channel"

-- Type of Pictures as describing in Telegram bots API
data TGPhotoSize = TGPhotoSize {
    tgPhotoId     :: String,      -- Уникальный идентификатор файла
    tgPhotoWidth  :: Integer,     -- Photo width
    tgPhotoHeight :: Integer,     -- Photo height
    tgPhotoSize   :: Integer      -- Опционально. Размер файла
    }
    deriving (Eq,Show)
instance FromJSON TGPhotoSize where
    parseJSON (Object photo) = do
        -- These fields are necessary
        Just photoID   <- photo .:  "file_id"
        Just photoW    <- photo .:  "width"
        Just photoH    <- photo .:  "height"
        -- This fields are optional
        Just photoS    <- photo .:? "file_size"     .!= Just 0
        return TGPhotoSize {tgPhotoId     = photoID,
                            tgPhotoWidth  = photoW,
                            tgPhotoHeight = photoH,
                            tgPhotoSize   = photoS
                            }
instance ToJSON TGPhotoSize where
    toJSON photo = object [ "file_id"   .= tgPhotoId photo
                          , "width"     .= tgPhotoWidth photo
                          , "height"    .= tgPhotoHeight photo
                          , "file_size" .= tgPhotoSize photo
                          ]                       

-- Type of custom Keyboard as describing in Telegram bots API
data TGReplyKeyboardMarkup = TGKeyBoard
    { tgButtons     :: [[TGButton]]
    , tgKBResize    :: Bool
    , tgKBOneTime   :: Bool
    , tgKBSelective :: Bool
    }
    deriving Show
instance FromJSON TGReplyKeyboardMarkup where
    parseJSON (Object keyboard) = do
        -- These fields are necessary
        Just buttons   <- keyboard .:  "keyboard"
        -- This fields are optional
        Just resize    <- keyboard .:? "resize_keyboard"   .!= Just False
        Just oneTime   <- keyboard .:? "one_time_keyboard" .!= Just False
        Just select    <- keyboard .:? "selective"         .!= Just False
        return TGKeyBoard { tgButtons     = buttons
                          , tgKBResize    = resize
                          , tgKBOneTime   = oneTime
                          , tgKBSelective = select
                          }
instance ToJSON TGReplyKeyboardMarkup where
    toJSON keyboard = object [ "keyboard"          .= tgButtons keyboard
                             , "resize_keyboard"   .= tgKBResize keyboard
                             , "one_time_keyboard" .= tgKBOneTime keyboard
                             , "selective"         .= tgKBSelective keyboard
                             ]    

data TGButton = TGButton
    { tgButtonText      :: String
--    , tgRequestContact  :: Bool	
--    , tgRequestLocation	:: Bool	
--    , tgRequestPoll	    :: KeyboardButtonPollType	
    }
    deriving Show
instance FromJSON TGButton where
    parseJSON (Object button) = do
        -- These fields are necessary
        Just text   <- button .:  "text"
        -- This fields are optional
--        Just contact    <- keyboard .:? "request_contact"   .!= Just False
--        Just location   <- keyboard .:? "request_location"  .!= Just False
--        Just poll       <- keyboard .:? "request_poll"      .!= Just undefined
        return TGButton { tgButtonText = text
                        }
instance ToJSON TGButton where
    toJSON button = object [ "text" .= tgButtonText button
                           ]    

-- Types for creating requests
data ReqSet = ReqSet {tgMethod :: String, tgReqParams :: [(BS.ByteString,Maybe BS.ByteString)]}