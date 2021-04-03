{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VKLib where

import           Data.Aeson
import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString.UTF8 as BS
--import qualified Data.ByteString.Lazy as BSLazy (toStrict)
import           Bot.VK.Types

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
str AtMedia {..} = case access_key media of
            Nothing -> aType <> (show . ownerId) media <> "_" <> (show . objectId) media
            Just k  -> aType <> (show . ownerId) media <> "_" <> (show . objectId) media <> "_" <> k
        
    