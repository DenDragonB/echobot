{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VKLib where

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


data Attachment  = AtPhoto { aType :: String, photo :: Photo }
                 | AtVideo { aType :: String, video :: Video }
                 | AtAudio { aType :: String, audio :: Audio }
                 | AtDocument { aType :: String, doc :: Document }
                 | AtLink { aType :: String, link :: Link } 
                 | AtMarket { aType :: String, market :: Market }
                 | AtMarketAlbum { aType :: String, marketA :: MarketAlbum }
                 | AtWall { aType :: String, wall :: Wall }
                 | AtWallReply { aType :: String, wallR :: WallReply }
                 | AtSticker { aType :: String, sticker :: Sticker } 
                 | AtGift { aType :: String, gift :: Gift }
                 deriving Show
instance FromJSON Attachment where
    parseJSON (Object attach) = do
        aType         <- attach .: "type"
        case aType :: Maybe String of
            Nothing -> fail "FromJSON VKLib.Attachment"
            Just "photo"        -> do
                Just aPhoto   <- attach .: "photo"
                return $ AtPhoto "photo" aPhoto
            Just "video"        -> do
                Just aVideo   <- attach .: "video"
                return $ AtVideo "video" aVideo
            Just "audio"        -> do
                Just aAudio   <- attach .: "audio"
                return $ AtAudio "audio" aAudio
            Just "doc"          -> do
                Just aDoc     <- attach .: "doc"
                return $ AtDocument "doc" aDoc
            Just "link"         -> do
                Just aLink    <- attach .: "link"
                return $ AtLink "link" aLink
            Just "market"       -> do
                Just aMArket  <- attach .: "market"
                return $ AtMarket "market" aMArket
            Just "market_album" -> do
                Just aMAlbum  <- attach .: "market_album"
                return $ AtMarketAlbum "market_album" aMAlbum
            Just "wall"         -> do
                Just aWall    <- attach .: "wall"
                return $ AtWall "wall" aWall
            Just "wall_reply"   -> do
                Just aWReply  <- attach .: "wall_reply"
                return $ AtWallReply "wall_reply" aWReply
            Just "sticker"      -> do
                Just aSticker <- attach .: "sticker"
                return $ AtSticker "sticker" aSticker
            Just "gift"         -> do
                Just aGift    <- attach .: "gift"
                return $ AtGift "gift" aGift



data Photo = Photo { objectId   :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON Photo where
    parseJSON = withObject "FromJSON VKLib.Photo" $ \o -> Photo
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key"


data Video = Video { objectId    :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON Video where
    parseJSON = withObject "FromJSON VKLib.Video" $ \o -> Video
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key"

data Audio = Audio { objectId    :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON Audio where
    parseJSON = withObject "FromJSON VKLib.Audio" $ \o -> Audio
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key"

data Document = Doc { objectId    :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON Document where
    parseJSON = withObject "FromJSON VKLib.Document" $ \o -> Doc
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

data Market = Market { objectId    :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON Market where
    parseJSON = withObject "FromJSON VKLib.Market" $ \o -> Market
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key"

data MarketAlbum = MarketAlbum { objectId    :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON MarketAlbum where
    parseJSON = withObject "FromJSON VKLib.MarketAlbum" $ \o -> MarketAlbum
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key" 

data Wall = Wall { objectId    :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON Wall where
    parseJSON = withObject "FromJSON VKLib.Wall" $ \o -> Wall
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key"

data WallReply = WallReply { objectId    :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON WallReply where
    parseJSON = withObject "FromJSON VKLib.WallReply" $ \o -> WallReply
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key"   

data Sticker = Sticker { prodID  :: Integer
                       , stickID :: Integer
                       } deriving Show
instance FromJSON Sticker where
    parseJSON = withObject "FromJSON VKLib.Sticker" $ \o -> Sticker
        <$> o .: "product_id"
        <*> o .: "sticker_id"

data Gift = Gift { objectId    :: Integer 
                   , ownerId    :: Integer
                   , access_key :: Maybe String 
                   } deriving Show
instance FromJSON Gift where
    parseJSON = withObject "FromJSON VKLib.Gift" $ \o -> Gift
        <$> o .: "id"
        <*> o .: "owner_id"
        <*> o .: "access_key"   

-- Types for creating requests
data ReqSet = ReqSet {method :: String, reqParams :: [(BS.ByteString,Maybe BS.ByteString)]}

getResponseFromAPI :: ReqSet -> IO BS.ByteString
getResponseFromAPI settings = do
    let request
            = HTTP.setRequestMethod (BS.fromString "GET")
            $ HTTP.setRequestHost   (BS.fromString "api.vk.com")
            $ HTTP.setRequestPort   (443)
            $ HTTP.setRequestSecure (True)
            $ HTTP.setRequestPath   (BS.fromString $ "/method/" ++ method settings)
            $ HTTP.setRequestQueryString (reqParams settings)
            $ HTTP.defaultRequest
    res <- HTTP.httpBS request
    return (HTTP.getResponseBody res)

getUpdates :: LongPollServer -> Integer -> String -> IO BS.ByteString
getUpdates server wait ts = do 
    request <- HTTP.parseRequest $ 
        sAddres server ++ "?act=a_check&key=" ++ key server ++ 
        "&ts=" ++ ts ++ "&wait=" ++ show wait
    res <- HTTP.httpBS request
    return (HTTP.getResponseBody res)

-- Methods of Telegram API
getServer :: Config -> ReqSet
getServer Config {..} = ReqSet {method = "groups.getLongPollServer",
    reqParams = [ (BS.fromString "group_id", Just $ BS.fromString $ show groupVKId)
                , (BS.fromString "access_token", Just $ BS.fromString token)
                , (BS.fromString "v",  Just $ BS.fromString "5.130")
                ] }

sendMessage :: Config -> Integer -> Integer -> String -> ReqSet
sendMessage Config {..} user rnd msg = ReqSet {method = "messages.send",
    reqParams = [ (BS.fromString "group_id", Just $ BS.fromString $ show groupVKId)
                , (BS.fromString "user_id", Just $ BS.fromString $ show user)
                , (BS.fromString "random_id", Just $ BS.fromString $ show rnd)
                , (BS.fromString "message", Just $ BS.fromString msg)
                , (BS.fromString "access_token", Just $ BS.fromString token)
                , (BS.fromString "v",  Just $ BS.fromString "5.130")
                ] }

copyMessage :: Config -> ObjMEssageNew -> Integer -> Integer -> String -> ReqSet
copyMessage Config {..} mes user rnd msg = ReqSet 
    { method = "messages.send"
    , reqParams = [ (BS.fromString "group_id", Just $ BS.fromString $ show groupVKId)
                , (BS.fromString "user_id", Just $ BS.fromString $ show user)
                , (BS.fromString "random_id", Just $ BS.fromString $ show rnd)
                , (BS.fromString "message", Just $ BS.fromString msg)
                , (BS.fromString "access_token", Just $ BS.fromString token)
                , (BS.fromString "v",  Just $ BS.fromString "5.130")
                ] <> attachToReq mes}

attachToReq :: ObjMEssageNew -> [(BS.ByteString, Maybe BS.ByteString)]
attachToReq ObjMEssageNew {..} = 
    case attach message of 
        [] -> []
        _  -> [(BS.fromString "attachment", Just $ BS.fromString st)]
                    where 
                        st = atString $ attach message
        
atString :: [Attachment] -> String
atString [a]    = str a
atString (a:as) = str a <> "," <> atString as

str :: Attachment -> String
str a | a `elem`
        "photo" -> case (access_key . photo) a of
            Nothing -> "photo" <> (show . ownerId . photo) a<> "_" <> (show . photoId . photo) a
            Just k  -> "photo" <> (show . ownerId . photo) a<> "_" <> (show . photoId . photo) a <> "_" <> k
        
    