{-# LANGUAGE DeriveGeneric #-}

module Bot where

import qualified Data.Aeson          as A
import           Data.HashMap.Strict as MAP
import           GHC.Generics

type Token = String

data Config = Config
    { aboutText     :: String
    , helpText      :: String
    , repeatText1   :: String
    , repeatText2   :: String
    , repeatDefault :: URep
    }
    deriving (Show,Eq,Generic)
instance A.FromJSON Config

newtype Handle = Handle
    { hConfig :: Config
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

type Users = HashMap UID User

data Exceptions
    = ServerNotResponding -- The server is not responding
    | FatalError String -- Other Errors with end program
     deriving Eq
instance Show Exceptions where
    show ServerNotResponding = "The server is not responding"
    show (FatalError s)      = s


emptyUsers :: Users
emptyUsers = empty

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle conf f = f $ Handle conf

setCommand :: Users -> User -> Users
setCommand users newUser = insert (uID newUser) newUser users


getCommand :: Users -> UID -> Bool
getCommand users uid = maybe False uSentRep (MAP.lookup uid users)

putRepeat :: Users -> User -> Users
putRepeat users newUser = insert (uID newUser) newUser users

getRepeat :: Users -> URep -> UID -> URep
getRepeat users defrep uid = maybe defrep uRep (MAP.lookup uid users)
