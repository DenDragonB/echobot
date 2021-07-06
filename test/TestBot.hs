{-# LANGUAGE OverloadedStrings #-}
module TestBot where

import Test.Hspec
import Data.HashMap.Strict

import Bot

testConfig :: Config
testConfig = Config
    { aboutText     = "about"
    , helpText      = "help"
    , repeatText1   = "repeat1"
    , repeatText2   = "repeat2"
    , Bot.repeatDefault = 1
    }

testUsersEmpty :: Users
testUsersEmpty = empty

testUsers :: Users
testUsers = insert 1 testUser1 $ insert 2 testUser2 empty

testUser1 :: User
testUser1 = User "user1" 1 1 False

testUser2 :: User
testUser2 = User "user2" 2 1 True

testUser3 :: User
testUser3 = User "user3" 3 3 True

main :: IO ()
main = hspec $ do
    describe "Bot functions" $ do
        it "setCommand to empty users" $ do
            let res = setCommand testUsersEmpty testUser1
            res `shouldBe` (insert 1 testUser1 emptyUsers)
        it "setCommand to user" $ do
            let res = setCommand testUsers testUser1 {uSentRep = True}
            res `shouldBe`  (insert 1 testUser1 {uSentRep = True} $ insert 2 testUser2 empty)
        it "setCommand to new user" $ do
            let res = setCommand testUsers testUser3
            res `shouldBe` 
                ( insert 1 testUser1 
                $ insert 2 testUser2 
                $ insert 3 testUser3 empty)
        it "getCommand" $ do
            let res = getCommand testUsers 2
            res `shouldBe` True
        it "getCommand with existant user" $ do
            let res = getCommand testUsers 3
            res `shouldBe` False
        it "putRepeat" $ do
            let res = putRepeat testUsers testUser1 {uRep = 2}
            res `shouldBe`  (insert 1 testUser1 {uRep = 2} $ insert 2 testUser2 empty)
        it "putRepeat with new user" $ do
            let res = putRepeat testUsers testUser3
            res `shouldBe`  
                ( insert 1 testUser1 
                $ insert 2 testUser2 
                $ insert 3 testUser3 empty)
        it "getRepeat" $ do
            let res = getRepeat testUsers 3 2
            res `shouldBe` 1
        it "getRepeat with existant user" $ do
            let res = getRepeat testUsers 2 3
            res `shouldBe` 2