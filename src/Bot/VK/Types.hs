{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types where

import           Data.Aeson
import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString.UTF8 as BS
--import qualified Data.ByteString.Lazy as BSLazy (toStrict)

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

data Update = NewMessage { upType    :: UpType 
                         , upObject  :: ObjMEssageNew
                         , upGroupID :: Integer
                         , eventId   :: String
                         } 
            | Other      { upType    :: UpType 
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
                return NewMessage { upType    = uType 
                                  , upObject  = uObj
                                  , upGroupID = uGroup
                                  , eventId   = uEvent
                                  }
            _ -> return Other { upType    = uType
                              , upGroupID = uGroup
                              , eventId   = uEvent
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


data Attachment  = AtMedia { aType :: String, media :: Media }
                 | AtLink { aType :: String, link :: Link } 
                 | AtSticker { aType :: String, sticker :: Sticker } 
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
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON Media where
    parseJSON = withObject "FromJSON VKLib.Photo" $ \o -> Media
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key"

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
        
    