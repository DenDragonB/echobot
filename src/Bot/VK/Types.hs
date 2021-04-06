{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VK.Types where

import           Data.Aeson

newtype RespServer = RespServer {getResp :: LongPollServer} deriving Show
instance FromJSON RespServer where
    parseJSON = withObject "FromJSON VKLib.Response" $ \o -> RespServer
        <$> o .: "response"

data LongPollServer = LPServer 
    { sAddres :: String
    , key     :: String
    , startTs :: String
    } deriving Show
instance FromJSON LongPollServer where
    parseJSON = withObject "FromJSON VKLib.LPServer" $ \o -> LPServer
        <$> o .: "server"
        <*> o .: "key"
        <*> o .: "ts"

data Config = Config
    { token     :: String
    , groupVKId :: Integer
    , timeout   :: Integer
    } deriving Show
instance FromJSON Config where
    parseJSON = withObject "FromJSON VKLib.Config" $ \o -> Config
        <$> o .: "token"
        <*> o .: "groupId"
        <*> o .: "timeout"

data Response = Response { ts      :: String     -- number of last events
                         , updates :: [Update]  -- new events
                         } deriving Show
instance FromJSON Response where
    parseJSON = withObject "FromJSON VKLib.Response" $ \o -> Response
        <$> o .: "ts"
        <*> o .: "updates"

data Update = Update { upType    :: UpType 
                     , upObject  :: Maybe ObjMEssageNew
                     , upGroupID :: Integer
                     , eventId   :: String
                     } deriving Show
instance FromJSON Update where
    parseJSON (Object upd) = do
        Just uType  <- upd .: "type"
        Just uGroup <- upd .: "group_id"
        Just uEvent <- upd .: "event_id"
        case uType of
            MessageNew -> do
                Just uObj <- upd .: "object"
                return Update  { upType    = uType 
                               , upObject  = Just uObj
                               , upGroupID = uGroup
                               , eventId   = uEvent
                               }
            _ -> return Update { upType   = uType
                               , upGroupID = uGroup
                               , eventId   = uEvent
                               , upObject  = Nothing 
                               }


data UpType = MessageNew | MessageEvent | TypeUnknown deriving (Show,Eq)
instance FromJSON UpType where
    parseJSON = withText "VKLib.UpType" $ \s ->
        case s of
            "message_new"    -> return MessageNew
            "message_event"  -> return MessageEvent
            _                -> return TypeUnknown

newtype ObjMEssageNew = ObjMEssageNew { message :: ObjMessage} deriving Show
instance FromJSON ObjMEssageNew where
    parseJSON = withObject "FromJSON VKLib.UpObject" $ \o -> ObjMEssageNew
        <$> o .: "message"

data ObjMessage = ObjMessage { mesId :: Integer
                             , fromID :: Integer
                             , text :: String 
                             , attach :: [Attachment]
                             } deriving Show
instance FromJSON ObjMessage where
    parseJSON = withObject "FromJSON VKLib.ObjMessage" $ \o -> ObjMessage
        <$> o .: "id"
        <*> o .: "from_id"
        <*> o .: "text"
        <*> o .: "attachments"


data Attachment  = AtMedia   String Media
                 | AtLink    String Link
                 | AtSticker String Sticker
                 deriving Show
instance FromJSON Attachment where
    parseJSON (Object attach) = do
        aType         <- attach .: "type"
        case aType :: Maybe String of
            Nothing -> fail "FromJSON VKLib.Attachment"
            Just "photo"        -> do
                Just aPhoto   <- attach .: "photo"
                return $ AtMedia "photo" aPhoto
            Just "video"        -> do
                Just aVideo   <- attach .: "video"
                return $ AtMedia "video" aVideo
            Just "audio"        -> do
                Just aAudio   <- attach .: "audio"
                return $ AtMedia "audio" aAudio
            Just "doc"          -> do
                Just aDoc     <- attach .: "doc"
                return $ AtMedia "doc" aDoc
            Just "link"         -> do
                Just aLink    <- attach .: "link"
                return $ AtLink "link" aLink
            Just "market"       -> do
                Just aMArket  <- attach .: "market"
                return $ AtMedia "market" aMArket
            Just "market_album" -> do
                Just aMAlbum  <- attach .: "market_album"
                return $ AtMedia "market_album" aMAlbum
            Just "wall"         -> do
                Just aWall    <- attach .: "wall"
                return $ AtMedia "wall" aWall
            Just "wall_reply"   -> do
                Just aWReply  <- attach .: "wall_reply"
                return $ AtMedia "wall_reply" aWReply
            Just "sticker"      -> do
                Just aSticker <- attach .: "sticker"
                return $ AtSticker "sticker" aSticker
            Just "gift"         -> do
                Just aGift    <- attach .: "gift"
                return $ AtMedia "gift" aGift



data Media = Media { objectId   :: Integer 
                   , ownerId    :: Integer
                   , access_key :: String 
                   } deriving Show
instance FromJSON Media where
    parseJSON (Object med) = do
        Just id    <- med .: "id"
        Just owner <- med .: "owner_id"
--        key        <- med .: "access_key"
--        case key of
--            Nothing -> return $ Media id owner ""
--            Just k  -> 
        return $ Media id owner "" -- k

data Link = Link { url   :: String 
                 , title :: String 
                 } deriving Show
instance FromJSON Link where
    parseJSON = withObject "FromJSON VKLib.Link" $ \o -> Link
        <$> o .: "url"
        <*> o .: "title"

data Sticker = Sticker { prodID  :: Integer
                       , stickID :: Integer
                       } deriving Show
instance FromJSON Sticker where
    parseJSON = withObject "FromJSON VKLib.Sticker" $ \o -> Sticker
        <$> o .: "product_id"
        <*> o .: "sticker_id"

data Keyboard = Keyboard { oneTime :: Bool
                         , buttons :: [[Button]]
                         , inline  :: Bool
                         } deriving Show
instance FromJSON Keyboard where
    parseJSON = withObject "FromJSON VKLib.Keyboard" $ \o -> Keyboard
        <$> o .: "one_time"
        <*> o .: "buttons"
        <*> o .: "inline"
instance ToJSON Keyboard where
    toJSON keyboard = object [ "one_time" .= oneTime keyboard
                             , "buttons"  .= buttons keyboard
                             , "inline"   .= inline keyboard
                             ]
        
data Button = Button { butAction :: ButAction
                     , butColor  :: ButColor
                     } deriving Show
instance FromJSON Button where
    parseJSON = withObject "FromJSON VKLib.Button" $ \o -> Button
        <$> o .: "action"
        <*> o .: "color"
instance ToJSON Button where
    toJSON but = object [ "action" .= butAction but
                        , "color"  .= butColor but
                        ]

data ButColor = PrimaryB             -- синяя кнопка, обозначает основное действие. #5181B8
              | SecondaryB           -- обычная белая кнопка. #FFFFFF
              | NegativeB            -- опасное действие, или отрицательное действие 
                                     -- (отклонить, удалить и тд). #E64646
              | PositiveB            -- согласиться, подтвердить. #4BB34B
instance Show ButColor where
    show PrimaryB   = "primary"
    show SecondaryB = "secondary"
    show NegativeB  = "negative"
    show PositiveB  = "positive"
instance FromJSON ButColor where
    parseJSON = withText "VKLib.ButColor" $ \s ->
        case s of
            "primary"    -> return PrimaryB
            "secondary"  -> return SecondaryB
            "negative"  -> return NegativeB
            "positive"  -> return PositiveB
            _            -> fail $ "Unknown color for button"
instance ToJSON ButColor where
    toJSON PrimaryB   = "primary"
    toJSON SecondaryB = "secondary"
    toJSON NegativeB  = "negative"
    toJSON PositiveB  = "positive"

data ButAction = ButText { bType   :: String 
                         , bLabel  :: String
                         , payload :: String
                         } 
               | ButOther { bType   :: String }
               deriving Show
instance FromJSON ButAction where
    parseJSON (Object but) = do
        Just butType    <- but .: "type"
        case butType of
            "text" -> do
                Just butL  <- but .: "label"
                Just butPL <- but .: "payload"
                return $ ButText butType butL butPL
            _ -> return $ ButOther butType
instance ToJSON ButAction where
    toJSON ButText {..} = object [ "type"    .= bType
                                 , "label"   .= bLabel
                                 , "payload" .= payload
                                 ]
    toJSON ButOther {..} = object [ "type" .= bType ]

    