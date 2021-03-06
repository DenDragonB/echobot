{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Logger
    ( LogLevel (..)
    , LogTo (..)
    , Config (..)
    , Handle (..)

    , withHandle

    , debug
    , info
    , warning
    , error

    ) where

import qualified Data.Aeson      as A
import           Data.Time.Clock
import           GHC.Generics
import           Prelude         hiding (error, log)
import qualified System.IO       as IO (FilePath, IOMode (..), hPutStrLn,
                                        stderr, withFile)

data LogLevel
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Generic)
instance A.FromJSON LogLevel

data LogTo
    = LogToFile
    | LogToConsole
    deriving (Eq, Ord, Show, Generic)
instance A.FromJSON LogTo 

data Config = Config
    { logTo       :: LogTo
    , logPath     :: IO.FilePath
    , logMinLevel :: LogLevel
    }
    deriving (Show,Eq,Generic)
instance A.FromJSON Config

newtype Handle = Handle
    { hConfig :: Config  }
    deriving (Show,Eq)

-- Create Handle with Config
withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = case logPath config of
    "" -> f $ Handle config {logPath = "log.txt"}
    _  -> f $ Handle config

-- Output log string to file
logToFile :: Config -> LogLevel -> String -> IO ()
logToFile Config {..} lvl str = IO.withFile logPath IO.AppendMode
    (\ hdl -> do
        time <- getCurrentTime
        IO.hPutStrLn hdl $ show time ++ " - " ++ show lvl ++ ": " ++ str)

-- Output log string to stderr
logToConsole :: LogLevel -> String -> IO ()
logToConsole lvl str = IO.hPutStrLn IO.stderr $ show lvl ++ ": " ++ str

-- Output log depending on the configuration
log :: Handle -> LogLevel -> String -> IO ()
log Handle {..} lvl str | lvl >= logMinLevel hConfig = case logTo hConfig of
                                                           LogToFile -> logToFile hConfig lvl str
                                                           _         -> logToConsole lvl str
                          | otherwise                  = return ()

debug, info, warning, error :: Handle -> String -> IO ()
debug   h = log h Debug
info    h = log h Info
warning h = log h Warning
error   h = log h Error
