{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.VK.Methods
    ( ReqSet (..)
-- functions to send request to vk servers
    , getResponseFromAPI
    , getUpdates
-- vk api methods
    , getServer
    , sendMessage
    , copyMessage

    ) where

import           Bot.VK.Types
import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as BSLazy (toStrict)
import qualified Data.ByteString.UTF8 as BS
import qualified Network.HTTP.Simple  as HTTP

-- Types for creating requests
data ReqSet = ReqSet { method    :: String
                     , reqParams :: [(BS.ByteString,Maybe BS.ByteString)]} deriving (Show,Eq)

getResponseFromAPI :: ReqSet -> IO BS.ByteString
getResponseFromAPI settings = do
    let request
            = HTTP.setRequestMethod (BS.fromString "GET")
            $ HTTP.setRequestHost   (BS.fromString "api.vk.com")
            $ HTTP.setRequestPort   443
            $ HTTP.setRequestSecure True
            $ HTTP.setRequestPath   (BS.fromString $ "/method/" ++ method settings)
            $ HTTP.setRequestQueryString (reqParams settings)
            HTTP.defaultRequest
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

sendMessage :: Config -> Integer -> Integer -> String -> Bool -> ReqSet
sendMessage Config {..} user rnd msg kb = ReqSet {method = "messages.send",
    reqParams = [ (BS.fromString "group_id", Just $ BS.fromString $ show groupVKId)
                , (BS.fromString "user_id", Just $ BS.fromString $ show user)
                , (BS.fromString "random_id", Just $ BS.fromString $ show rnd)
                , (BS.fromString "message", Just $ BS.fromString msg)
                , (BS.fromString "access_token", Just $ BS.fromString token)
                , (BS.fromString "v",  Just $ BS.fromString "5.130")
                ] <> sendKB kb}

sendKB :: Bool -> [(BS.ByteString, Maybe BS.ByteString)]
sendKB flag = [(BS.fromString "keyboard",  Just $ BSLazy.toStrict $ encode keyboardForRep)
    | flag]

keyboardForRep :: Keyboard
keyboardForRep = Keyboard
    { oneTime = True
    , buttons = [
        [ Button { butAction = ButText { bType = "text"
                                       , bLabel = "1"
                                       , payload = ""}
                 , butColor = PrimaryB }
        , Button { butAction = ButText { bType = "text"
                                       , bLabel = "2"
                                       , payload = ""}
                 , butColor = PrimaryB }
        , Button { butAction = ButText { bType = "text"
                                       , bLabel = "3"
                                       , payload = ""}
                 , butColor = PrimaryB }
        , Button { butAction = ButText { bType = "text"
                                       , bLabel = "4"
                                       , payload = ""}
                 , butColor = PrimaryB }
        , Button { butAction = ButText { bType = "text"
                                       , bLabel = "5"
                                       , payload = ""}
                 , butColor = PrimaryB }
        ]]
    , inline = False
    }

copyMessage :: Config -> ObjMEssageNew -> Integer -> Integer -> String -> ReqSet
copyMessage conf mes user rnd msg =
    let req = sendMessage conf user rnd msg False
        param = reqParams req
    in req {reqParams = param <> attachToReq mes <> stickToReq mes}

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
atString _      = ""

str :: Attachment -> String
str AtMedia {..} = case access_key media of
            "" -> aType <> (show . ownerId) media <> "_" <> (show . objectId) media
            _  -> aType <> (show . ownerId) media <> "_" <>
                           (show . objectId) media <> "_" <> access_key media
str AtSticker {} = ""
str AtLink {..}    = aType <> url link <> "_" <> title link

stickToReq :: ObjMEssageNew -> [(BS.ByteString, Maybe BS.ByteString)]
stickToReq ObjMEssageNew {..} = foldr stickString [] $ attach message

stickString :: Attachment
            -> [(BS.ByteString, Maybe BS.ByteString)]
            -> [(BS.ByteString, Maybe BS.ByteString)]
stickString AtSticker {..} _ =
    [(BS.fromString "sticker_id",  Just $ BS.fromString $ show $ stickID sticker)]
stickString _ sts = sts



