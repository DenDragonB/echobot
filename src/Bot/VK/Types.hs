{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VK.Types where

import           Data.Aeson
import           GHC.Generics

newtype RespServer = RespServer {getResp :: LongPollServer} deriving Show
instance FromJSON RespServer where
    parseJSON = withObject "FromJSON VK.Types.Response of Server" $ \o -> RespServer
        <$> o .: "response"

data LongPollServer = LPServer
    { sAddres :: String
    , key     :: String
    , startTs :: String
    } deriving (Show,Eq)
instance FromJSON LongPollServer where
    parseJSON = withObject "FromJSON VK.Types.LPServer" $ \o -> LPServer
        <$> o .: "server"
        <*> o .: "key"
        <*> o .: "ts"

data Config = Config
    { token   :: String
    , groupId :: Integer
    , timeout :: Integer
    } deriving (Show,Eq,Generic)
instance FromJSON Config

data Failed = Failed
    { failed :: Int
    , fts    :: Maybe String
    } deriving (Show,Eq,Generic)
instance FromJSON Failed where
    parseJSON = withObject "FromJSON VK.Types.Response" $ \o -> Failed
        <$> o .: "failed"
        <*> o .: "ts"

data Response = Response { ts      :: String     -- number of last events
                         , updates :: [Update]  -- new events
                         } deriving (Show,Eq,Generic)
instance FromJSON Response

data Update = Update { upType    :: UpType
                     , upObject  :: Maybe ObjMEssageNew
                     , upGroupID :: Integer
                     , eventId   :: String
                     } deriving (Show,Eq)
instance FromJSON Update where
    parseJSON = withObject "FromJSON VK.Types.Response" $ \upd -> do
        uType  <- upd .: "type"
        uGroup <- upd .: "group_id"
        uEvent <- upd .: "event_id"
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
    parseJSON = withText "FromJSON VK.Types.UpType" $ \case
        "message_new"   -> return MessageNew
        "message_event" -> return MessageEvent
        _               -> return TypeUnknown

newtype ObjMEssageNew = ObjMEssageNew { message :: ObjMessage} deriving (Show,Eq,Generic)
instance FromJSON ObjMEssageNew

data ObjMessage = ObjMessage { mesId  :: Integer
                             , fromID :: Integer
                             , text   :: String
                             , attach :: [Attachment]
                             } deriving (Show,Eq)
instance FromJSON ObjMessage where
    parseJSON = withObject "FromJSON VK.Types.ObjMessage" $ \o -> ObjMessage
        <$> o .: "id"
        <*> o .: "from_id"
        <*> o .: "text"
        <*> o .: "attachments"


data Attachment  = AtMedia { aType :: String, media :: Media }
                 | AtLink { aType :: String, link :: Link }
                 | AtSticker { aType :: String, sticker :: Sticker }
                 deriving (Show,Eq)
instance FromJSON Attachment where
    parseJSON = withObject "FromJSON VK.Types.Attachment" $ \attach -> do
        aType <- attach .: "type"
        case aType of
            Nothing -> fail "FromJSON VK.Types.Attachment"
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
            Just a -> fail $ "FromJSON VK.Types.Attachment: unknown attachment " <> a

data Media = Media { objectId   :: Integer
                   , ownerId    :: Integer
                   , access_key :: String
                   } deriving (Show,Eq)
instance FromJSON Media where
    parseJSON = withObject "FromJSON VK.Types.Media" $ \o -> do
        mid  <- o .: "id"
        moid <- o .: "owner_id"
        return $ Media mid moid ""


data Link = Link { url   :: String
                 , title :: String
                 } deriving (Show,Eq,Generic)
instance FromJSON Link

data Sticker = Sticker { prodID  :: Integer
                       , stickID :: Integer
                       } deriving (Show,Eq)
instance FromJSON Sticker where
    parseJSON = withObject "FromJSON VK.Types.Sticker" $ \o -> Sticker
        <$> o .: "product_id"
        <*> o .: "sticker_id"

data Keyboard = Keyboard { oneTime :: Bool
                         , buttons :: [[Button]]
                         , inline  :: Bool
                         } deriving Show
instance FromJSON Keyboard where
    parseJSON = withObject "FromJSON VK.Types.Keyboard" $ \o -> Keyboard
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
    parseJSON = withObject "FromJSON VK.Types.Button" $ \o -> Button
        <$> o .: "action"
        <*> o .: "color"
instance ToJSON Button where
    toJSON but = object [ "action" .= butAction but
                        , "color"  .= butColor but
                        ]

data ButColor = PrimaryB             -- ?????????? ????????????, ???????????????????? ???????????????? ????????????????. #5181B8
              | SecondaryB           -- ?????????????? ?????????? ????????????. #FFFFFF
              | NegativeB            -- ?????????????? ????????????????, ?????? ?????????????????????????? ????????????????
                                     -- (??????????????????, ?????????????? ?? ????). #E64646
              | PositiveB            -- ??????????????????????, ??????????????????????. #4BB34B
instance Show ButColor where
    show PrimaryB   = "primary"
    show SecondaryB = "secondary"
    show NegativeB  = "negative"
    show PositiveB  = "positive"
instance FromJSON ButColor where
    parseJSON = withText "VK.Types.ButColor" $ \case
            "primary"   -> return PrimaryB
            "secondary" -> return SecondaryB
            "negative"  -> return NegativeB
            "positive"  -> return PositiveB
            _           -> fail "Unknown color for button"
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
    parseJSON = withObject "FromJSON VK.Types.ButAction" $ \o -> do
        butType    <- o .: "type"
        case butType of
            "text" -> do
                butL  <- o .: "label"
                butPL <- o .: "payload"
                return $ ButText butType butL butPL
            _ -> return $ ButOther butType
instance ToJSON ButAction where
    toJSON ButText {..} = object [ "type"    .= bType
                                 , "label"   .= bLabel
                                 , "payload" .= payload
                                 ]
    toJSON ButOther {..} = object [ "type" .= bType ]


