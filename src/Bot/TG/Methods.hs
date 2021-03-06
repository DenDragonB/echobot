{-# LANGUAGE OverloadedStrings #-}

module Bot.TG.Methods where

import qualified Bot
import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as BSLazy (toStrict)
import qualified Data.ByteString.UTF8 as BS
import qualified Network.HTTP.Simple  as HTTPSimple
import qualified Network.HTTP.Client.Conduit  as HTTP

import           Bot.TG.Types

-- Types for creating requests
data ReqSet = ReqSet {method :: String, reqParams :: [(BS.ByteString,Maybe BS.ByteString)]}
    deriving (Show, Eq)

getResponseFromAPI :: String -> ReqSet -> IO (Either Bot.Exceptions BS.ByteString)
getResponseFromAPI token settings = do
    let request
            = HTTPSimple.setRequestMethod (BS.fromString "GET")
            $ HTTPSimple.setRequestHost   (BS.fromString "api.telegram.org")
            $ HTTPSimple.setRequestPort   443
            $ HTTPSimple.setRequestSecure True
            $ HTTPSimple.setRequestPath   (BS.fromString $ "/bot" ++ token ++ "/" ++ method settings)
            $ HTTPSimple.setRequestQueryString (reqParams settings)
            $ setRequestResponseTimeout HTTP.responseTimeoutNone
            HTTPSimple.defaultRequest
    Bot.getAnswear request

setRequestResponseTimeout :: HTTP.ResponseTimeout -> HTTPSimple.Request -> HTTPSimple.Request
setRequestResponseTimeout x req = req { HTTP.responseTimeout = x }

-- Methods of Telegram API
getUpdates :: Integer -> Integer -> ReqSet
getUpdates timeout offset = ReqSet {method = "getUpdates",
    reqParams = [ (BS.fromString "timeout", Just $ BS.fromString $ show timeout)
                , (BS.fromString "offset",  Just $ BS.fromString $ show offset)
                ] }

sendMessage :: Integer -> String -> Bool -> ReqSet
sendMessage chatID text isKB =  ReqSet {method = "sendMessage",
                reqParams = [ (BS.fromString "chat_id", Just $ BS.fromString $ show chatID)
                            , (BS.fromString "text", Just $ BS.fromString text)
                            ] <> addKB isKB}

addKB :: Bool -> [(BS.ByteString,Maybe BS.ByteString)]
addKB isKB =
    [(BS.fromString "reply_markup", Just $ BSLazy.toStrict $ encode keyboard) | isKB]

keyboard :: TGReplyKeyboardMarkup
keyboard = TGKeyBoard
                [[TGButton "1",TGButton "2",TGButton "3",TGButton "4",TGButton "5"]]
                True
                True
                False

copyMessage :: Integer -> Integer -> Integer -> ReqSet
copyMessage chatID fromChatID messageID = ReqSet {method = "copyMessage",
            reqParams = [ (BS.fromString "chat_id", Just $ BS.fromString $ show chatID)
                        , (BS.fromString "from_chat_id", Just $ BS.fromString $ show fromChatID)
                        , (BS.fromString "message_id", Just $ BS.fromString $ show messageID)
                        ] }
