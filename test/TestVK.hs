{-# LANGUAGE OverloadedStrings #-}
module TestVK where

import Test.Hspec

import qualified Logger
import qualified Bot
import Bot.VK
import Bot.VK.Types
import Bot.VK.Methods

testConfig :: Config
testConfig = Config
    { token     = "1a2bc3"
    , groupId   = 123
    , timeout   = 25
    }

testHandle :: Handle
testHandle = Handle
    { hConfig  = testConfig
    , hBot     = Bot.Handle 
        { Bot.hConfig = Bot.Config
            { Bot.aboutText     = "about"
            , Bot.helpText      = "help"
            , Bot.repeatText1   = "repeat1"
            , Bot.repeatText2   = "repeat2"
            , Bot.repeatDefault = 1
            }
        }
    , hLogger  = Logger.Handle
        { Logger.hConfig = Logger.Config
            { Logger.logTo = Logger.LogToConsole
            , Logger.logPath = ""
            , Logger.logMinLevel = Logger.Debug 
            }
        }
    , hServer = LPServer 
        { sAddres = "serverADDR"
        , key     = "token"
        , startTs = "ts1"
        }
    }

testState :: State
testState = State
    { users = Bot.emptyUsers
    , server   = LPServer 
        { sAddres = "serverADDR"
        , key     = "token"
        , startTs = "ts1"
        }
    , offset   = "25"
    , response = Just testResponse
    }

testResponse :: Response
testResponse = Response 
    { ts      = "ts2"
    , updates = [testUpdate]
    }

testUpdate :: Update
testUpdate = Update 
    { upType    = MessageNew 
    , upObject  = Just testObjectMessageNew
    , upGroupID = 123
    , eventId   = "event123"
    }

testObjectMessageNew :: ObjMEssageNew
testObjectMessageNew = ObjMEssageNew 
    { message = ObjMessage
        { mesId = 456
        , fromID = 789
        , text = "objectMessage" 
        , attach = testAttachments
        }
    }

testAttachments :: [Attachment]
testAttachments =   
    [ AtMedia {aType = "photo", media = 
        Media   { objectId = 1234 
                , ownerId = 5678
                , access_key = "accesskey" 
                } }
    , AtMedia {aType = "audio", media = 
        Media   { objectId = 1234 
                , ownerId = 5678
                , access_key = "" 
                } }
    , AtLink {aType = "link", link = 
        Link    { url = "url" 
                , title = "title"
                } }
    , AtSticker { aType = "sticker", sticker =
        Sticker { prodID = 12345
                , stickID = 6789
                } } 
    ]

main :: IO ()
main = hspec $ do
    describe "VK API methods" $ do
        it "getServer" $ do
            let res = getServer testConfig
            res `shouldBe` ReqSet {method = "groups.getLongPollServer",
                    reqParams = [ ("group_id", Just "123")
                                , ("access_token", Just "1a2bc3")
                                , ("v",  Just "5.130")
                                ] }
        it "sendMessage" $ do
            let res = sendMessage testConfig 235 55555 "message" False
            res `shouldBe` ReqSet {method = "messages.send",
                    reqParams = [ ("group_id", Just "123")
                                , ("user_id", Just "235")
                                , ("random_id", Just "55555")
                                , ("message", Just "message")
                                , ("access_token", Just "1a2bc3")
                                , ("v",  Just "5.130")
                                ] }
        it "sendMessage with keyboard" $ do
            let res = sendMessage testConfig 235 55555 "message" True
            res `shouldBe` ReqSet {method = "messages.send",
                    reqParams = [ ("group_id", Just "123")
                                , ("user_id", Just "235")
                                , ("random_id", Just "55555")
                                , ("message", Just "message")
                                , ("access_token", Just "1a2bc3")
                                , ("v",  Just "5.130")
                                , ("keyboard",  Just $ "{\"buttons\":["
                                    <> "[{\"color\":\"primary\",\"action\":"
                                    <> "{\"payload\":\"\",\"type\":\"text\","
                                    <> "\"label\":\"1\"}},{\"color\":\"primary\","
                                    <> "\"action\":{\"payload\":\"\",\"type\":"
                                    <> "\"text\",\"label\":\"2\"}},{\"color\":"
                                    <> "\"primary\",\"action\":{\"payload\":\"\","
                                    <> "\"type\":\"text\",\"label\":\"3\"}},"
                                    <> "{\"color\":\"primary\",\"action\":{"
                                    <> "\"payload\":\"\",\"type\":\"text\",\"label\":"
                                    <> "\"4\"}},{\"color\":\"primary\",\"action\":{"
                                    <> "\"payload\":\"\",\"type\":\"text\",\"label\":"
                                    <> "\"5\"}}]],\"inline\":false,\"one_time\":true}")
                                ] }
        it "copyMessage" $ do
            let res = copyMessage testConfig testObjectMessageNew 235 55555 "message"
            res `shouldBe` ReqSet {method = "messages.send",
                    reqParams = [ ("group_id", Just "123")
                                , ("user_id", Just "235")
                                , ("random_id", Just "55555")
                                , ("message", Just "message")
                                , ("access_token", Just "1a2bc3")
                                , ("v",  Just "5.130")
                                , ("attachment",  Just $ "photo5678_1234_accesskey"
                                                      <> ",audio5678_1234"
                                                      <> ",linkurl_title"
                                                      <> ",")
                                , ("sticker_id",  Just "6789")
                                ] }  
    describe "VK functions" $ do
        it "delUpdate" $ do
            let res = delUpdate testState "event123"
                mustResp = testResponse {updates = []}
            res `shouldBe` testState {response = Just mustResp}