{-# LANGUAGE OverloadedStrings #-}
module TestVK where

import Test.Hspec

import Bot.VK.Types
import Bot.VK.Methods

testConfig :: Config
testConfig = Config
    { token     = "1a2bc3"
    , groupVKId = 123
    , timeout   = 25
    }

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