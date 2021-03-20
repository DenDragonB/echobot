{-# LANGUAGE OverloadedStrings #-}

module Bot where

import qualified Data.Aeson as A
import qualified Logger as Logger

type Token = String

data Config = Config
    { aboutText     :: String
    , helpText      :: String
    , repeatText1   :: String
    , repeatText2   :: String
    , repeatDefault :: URep
    }
    deriving Show
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON BotTelegram.Config" $ \o -> Config
        <$> o A..: "aboutText"   
        <*> o A..: "helpText"
        <*> o A..: "repeatText1"
        <*> o A..: "repeatText2"
        <*> o A..: "repeatDefault"

data Handle = Handle
    { hConfig :: Config
    }
    deriving Show

-- type of user to store the number of repetitions
type UName = String
type UID   = Integer
type URep  = Int
data User  = User
    { uName :: UName
    , uID   :: UID
    , uRep  :: URep
    }

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle conf f = f $ Handle conf

putRepeat :: [User] -> User -> [User]
putRepeat [] newUser = [newUser]
putRepeat (u:us) newUser | uID u == uID newUser = newUser : us
                         | otherwise = u : putRepeat us newUser 

getRepeat :: [User] -> URep -> UID -> URep
getRepeat us defRep id = foldr (\u ini -> if uID u == id then uRep u else ini) defRep us 

configTest = Config 
    { aboutText     = "about text"
    , helpText      = "help text"
    , repeatText1   = "repeat now"
    , repeatText2   = "repeat what"
    , repeatDefault = 1
    }
handleTest = Handle
    { hConfig = configTest }