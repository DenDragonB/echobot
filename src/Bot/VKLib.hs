{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VKLib where

import           Data.Aeson
import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString.UTF8 as BS
--import qualified Data.ByteString.Lazy as BSLazy (toStrict)

data LongPollServer = LPServer 
    { sAddres :: String
    , key     :: String
    , ts      :: String}
instance FromJSON Config where
    parseJSON = withObject "FromJSON VKLib.LPServer" $ \o -> LPServer
        <$> o .: "server"
        <*> o .: "key"
        <*> o .: "ts"

data Config = Config
    { token     :: String
    , groupVKId :: Integer
    , timeout   :: Integer
    }
    deriving Show
instance FromJSON Config where
    parseJSON = withObject "FromJSON VKLib.Config" $ \o -> Config
        <$> o .: "token"
        <*> o .: "groupId"
        <*> o .: "timeout"

data Response = Response { ts      :: String     -- number of last events
                         , updates :: [Update]  -- new events
                         }
instance FromJSON Response where
    parseJSON = withObject "FromJSON VKLib.Response" $ \o -> Response
        <$> o .: "ts"
        <*> o .: "updates"

data Update = Update { upType    :: UpType 
                     , upObject  :: UpObject
                     , upGroupID :: Integer
                     }
instance FromJSON Update where
    parseJSON = withObject "FromJSON VKLib.Update" $ \o -> Update
        <$> o .: "type"
        <*> o .: "object"
        <*> o .: "group_id"

data UpType = MessageNew | MessageEvent | TypeUnknown
instance FromJSON UpType where
    parseJSON = withText "VKLib.UpType" $ \s ->
        case s of
            "message_new"    -> return MessageNew
            "message_event"  -> return MessageEvent
            _                -> return TypeUnknown

newtype UpObject = UpObject { objMessage :: ObjMessage}
instance FromJSON UpObject where
    parseJSON = withObject "FromJSON VKLib.UpObject" $ \o -> UpObject
        <$> o .: "id"

data ObjMessage = ObjMessage { mesId :: Integer
                             , fromID :: Integer
                             , text :: String 
                             , attach :: [Attachment]}
instance FromJSON ObjMessage where
    parseJSON = withObject "FromJSON VKLib.ObjMessage" $ \o -> ObjMessage
        <$> o .: "id"
        <*> o .: "from_id"
        <*> o .: "text"
        <*> o .: "attachments"


data Attachment  = AtPhoto { photo :: Photo }
                 | AtVideo { video :: Video }
                 | AtAudio { audio :: Audio }
                 | AtDocument { doc :: Document }
                 | AtLink { link :: Link } 
                 | AtMarket { market :: Market }
                 | AtMarketAlbum { marketA :: MarketAlbum }
                 | AtWall { wall :: Wall }
                 | AtWallReply { wallR :: WallReply }
                 | AtSticker { sticker :: Sticker } 
                 | AtGift { gift :: Gift }
instance FromJSON Attachment where
    parseJSON (Object attach) = do
        aType         <- attach .: "type"
        Just aPhoto   <- attach .: "photo"
        Just aVideo   <- attach .: "video"
        Just aAudio   <- attach .: "audio"
        Just aDoc     <- attach .: "doc"
        Just aLink    <- attach .: "link"
        Just aMArket  <- attach .: "market"
        Just aMAlbum  <- attach .: "market_album"
        Just aWall    <- attach .: "wall"
        Just aWReply  <- attach .: "wall_reply"
        Just aSticker <- attach .: "sticker"
        Just aGift    <- attach .: "gift"
        case aType :: Maybe String of
            Nothing -> fail "FromJSON VKLib.Attachment"
            Just "photo"        -> return $ AtPhoto aPhoto
            Just "video"        -> return $ AtVideo aVideo
            Just "audio"        -> return $ AtAudio aAudio
            Just "doc"          -> return $ AtDocument aDoc
            Just "link"         -> return $ AtLink aLink
            Just "market"       -> return $ AtMarket aMArket
            Just "market_album" -> return $ AtMarketAlbum aMAlbum
            Just "wall"         -> return $ AtWall aWall
            Just "wall_reply"   -> return $ AtWallReply aWReply
            Just "sticker"      -> return $ AtSticker aSticker
            Just "gift"         -> return $ AtGift aGift



data Photo = Photo { photoId :: Integer 
                   , ownerId :: Integer 
                   }
instance FromJSON Photo where
    parseJSON = withObject "FromJSON VKLib.Photo" $ \o -> Photo
        <$> o .: "id"
        <*> o .: "owner_id"

data Video = Video { videoId :: Integer }
instance FromJSON Video where
    parseJSON = withObject "FromJSON VKLib.Video" $ \o -> Video
        <$> o .: "id" 

data Audio = Audio { audioId :: Integer }
instance FromJSON Audio where
    parseJSON = withObject "FromJSON VKLib.Audio" $ \o -> Audio
        <$> o .: "id" 

data Document = Doc { docId :: Integer }
instance FromJSON Document where
    parseJSON = withObject "FromJSON VKLib.Document" $ \o -> Doc
        <$> o .: "id" 

data Link = Link { url   :: String 
                 , title :: String 
                 }
instance FromJSON Link where
    parseJSON = withObject "FromJSON VKLib.Link" $ \o -> Link
        <$> o .: "url"
        <*> o .: "title"

data Market = Market { marketId :: Integer }
instance FromJSON Market where
    parseJSON = withObject "FromJSON VKLib.Market" $ \o -> Market
        <$> o .: "id" 

data MarketAlbum = MarketAlbum { mAlbumId :: Integer }
instance FromJSON MarketAlbum where
    parseJSON = withObject "FromJSON VKLib.MarketAlbum" $ \o -> MarketAlbum
        <$> o .: "id" 

data Wall = Wall { wallId :: Integer }
instance FromJSON Wall where
    parseJSON = withObject "FromJSON VKLib.Wall" $ \o -> Wall
        <$> o .: "id" 

data WallReply = WallReply { wReplyId :: Integer}
instance FromJSON WallReply where
    parseJSON = withObject "FromJSON VKLib.WallReply" $ \o -> WallReply
        <$> o .: "id"   

data Sticker = Sticker { prodID  :: Integer
                       , stickID :: Integer}
instance FromJSON Sticker where
    parseJSON = withObject "FromJSON VKLib.Sticker" $ \o -> Sticker
        <$> o .: "product_id"
        <*> o .: "sticker_id"

data Gift = Gift { giftId :: Integer }
instance FromJSON Gift where
    parseJSON = withObject "FromJSON VKLib.Gift" $ \o -> Gift
        <$> o .: "id"   

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

getUpdates :: LongPollServer -> Integer -> Integer -> IO BS.ByteString
getUpdates server wait ts = do 
    request <- HTTP.parseRequest $ 
        sAddres server ++ "?act=a_check&key=" ++ key server ++ 
        "&ts=" ++ show ts ++ "&wait=" ++ show wait
    res <- HTTP.httpBS request
    return (HTTP.getResponseBody res)

-- Methods of Telegram API
getServer :: Config -> ReqSet
getServer Config {..} = ReqSet {method = "groups.getLongPollServer",
    reqParams = [ (BS.fromString "group_id", Just $ BS.fromString $ show groupVKId)
                , (BS.fromString "access_token", Just $ BS.fromString token)
                , (BS.fromString "v",  Just $ BS.fromString "5.130")
                ] }

