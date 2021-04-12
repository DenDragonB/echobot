{-# LANGUAGE OverloadedStrings #-}

module Bot where

import qualified Data.Aeson as A

type Token = String

data Config = Config
    { aboutText     :: String
    , helpText      :: String
    , repeatText1   :: String
    , repeatText2   :: String
    , repeatDefault :: URep
    }
    deriving (Show,Eq)
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON BotTelegram.Config" $ \o -> Config
        <$> o A..: "aboutText"   
        <*> o A..: "helpText"
        <*> o A..: "repeatText1"
        <*> o A..: "repeatText2"
        <*> o A..: "repeatDefault"

data Handle = Handle
    { hConfig :: Config
    ,  users :: [User]
    }
    deriving (Show,Eq)

-- type of user to store the number of repetitions
type UName = String
type UID   = Integer
type URep  = Int
data User  = User
    { uName    :: UName
    , uID      :: UID
    , uRep     :: URep
    , uSentRep :: Bool
    }
    deriving (Show,Eq)

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle conf f = f $ Handle conf []

setCommand :: [User] -> User -> [User]
setCommand [] newUser = [User { uName    = uName newUser
                                  , uID      = uID newUser
                                  , uRep     = uRep newUser
                                  , uSentRep = uSentRep newUser
                                  } ]
setCommand (u:us) newUser
    | uID u == uID newUser = User { uName    = uName newUser
                                  , uID      = uID newUser
                                  , uRep     = uRep u
                                  , uSentRep = uSentRep newUser
                                  } : us
    | otherwise = u : putRepeat us newUser 

getCommand :: [User] -> UID -> Bool
getCommand [] _      = False
getCommand (u:us) uid | uID u == uid = uSentRep u
                      | otherwise   = getCommand us uid

putRepeat :: [User] -> User -> [User]
putRepeat [] newUser = [newUser]
putRepeat (u:us) newUser | uID u == uID newUser = newUser : us
                         | otherwise = u : putRepeat us newUser 

getRepeat :: [User] -> URep -> UID -> URep
getRepeat us defRep uid = foldr (\u ini -> if uID u == uid then uRep u else ini) defRep us 