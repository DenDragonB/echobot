{-# LANGUAGE DeriveGeneric #-}

module Bot where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Aeson                  as A
import qualified Data.ByteString.UTF8        as BS
import           Data.HashMap.Strict         as MAP
import           GHC.Generics
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple         as HTTPSimple

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
    | HTTPError String -- Errors from HTTP request
     deriving Eq
instance Show Exceptions where
    show ServerNotResponding = "The server is not responding"
    show (FatalError s)      = s
    show (HTTPError s)       = "HTTP: " <> s


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

getResponse :: HTTPSimple.Request -> IO BS.ByteString
getResponse request = do
    res <- HTTPSimple.httpBS request
    return $ HTTPSimple.getResponseBody res

getAnswear :: HTTPSimple.Request -> IO (Either Exceptions BS.ByteString)
getAnswear req = do
    catch (Right <$> getResponse req) $ \e -> do
        case (e :: HTTPSimple.HttpException) of
            HTTPSimple.InvalidUrlException url err -> return $ Left $ HTTPError
                ("Invalid URL: " <> url <> ". reason: " <> err)
            HTTPSimple.HttpExceptionRequest _ cont -> do
                case cont of
                    HTTP.ResponseTimeout -> getAnswear req
                    HTTP.ConnectionTimeout -> getAnswear req
                    _ -> return $ Left $ HTTPError (show cont)
