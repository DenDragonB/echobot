{-# LANGUAGE OverloadedStrings #-}
module TestTG where

import Test.Hspec

import qualified Logger
import qualified Bot
import Bot.TG
import Bot.TG.Types
import Bot.TG.Methods

testConfig :: Config
testConfig = Config
    { token     = "1a2bc3"
    , timeout   = 60
    }

testResponse :: Response
testResponse = Response
    { responseOk = True
    , responseResult = [testUpdate]
    }

testUpdate :: Update
testUpdate = UpMessge
    { updateId = 123
    , message = Nothing
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
    }

testState :: State
testState = State
    { users = Bot.emptyUsers
    , response = Just testResponse
    , offset   = 25
    }

main :: IO ()
main = hspec $ do
    describe "TG API methods" $ do
        it "getUpdates" $ do
            let res = getUpdates 12 345
            res `shouldBe` ReqSet {method = "getUpdates",
                    reqParams = [ ("timeout", Just "12")
                                , ("offset", Just "345")
                                ] }
        it "sendMessage" $ do
            let res = sendMessage 123 "message" False
            res `shouldBe` ReqSet {method = "sendMessage",
                    reqParams = [ ("chat_id", Just "123")
                                , ("text", Just "message")
                                ] }
        it "sendMessage with keyboard" $ do
            let res = sendMessage 123 "message" True
            res `shouldBe` ReqSet {method = "sendMessage",
                    reqParams = [ ("chat_id", Just "123")
                                , ("text", Just "message")
                                , ("reply_markup", Just $ "{\"one_time_keyboard\":true,"
                                    <> "\"resize_keyboard\":true,\"selective\":false,"
                                    <> "\"keyboard\":[[{\"text\":\"1\"},{\"text\":\"2\"},"
                                    <> "{\"text\":\"3\"},{\"text\":\"4\"},{\"text\":\"5\"}]]}")
                                ] }
        it "copyMessage" $ do
            let res = copyMessage 123 456 789
            res `shouldBe` ReqSet {method = "copyMessage",
                    reqParams = [ ("chat_id", Just "123")
                                , ("from_chat_id", Just "456")
                                , ("message_id", Just "789")
                                ] }  
    describe "TG functions" $ do
        it "delUpdate" $ do
            let res = delUpdate testState 123
                mustResp = testResponse {responseResult = []}
            res `shouldBe` testState {response = Just mustResp}
