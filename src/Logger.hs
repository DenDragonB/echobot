{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

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

    , configTest
    , handleTest

    ) where

import           Prelude hiding (log, error)
import qualified System.IO as IO (IOMode (..),FilePath,withFile,hPutStrLn,stderr)
import qualified Data.Aeson as A
import qualified Data.Text as T
import           Data.Time.Clock 

data LogLevel 
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)
instance A.FromJSON LogLevel where
    parseJSON = A.withText "FromJSON Logger.LogLevel" $ \t ->
        case t of
            "Debug"   -> pure Debug
            "Info"    -> pure Info
            "Warning" -> pure Warning
            "Error"   -> pure Error
            _         -> fail $ "Unknown log level: " ++ T.unpack t

data LogTo
    = LogToFile
    | LogToConsole
    deriving (Eq, Ord, Show)
instance A.FromJSON LogTo where
    parseJSON = A.withText "FromJSON Logger.LogTo" $ \t ->
        case t of
            "LogToFile"    -> pure LogToFile
            "LogToConsole" -> pure LogToConsole
            _         -> fail $ "Unknown where log to: " ++ T.unpack t

data Config = Config
    { logTo   :: LogTo
    , logPath :: IO.FilePath
    , logMinLevel :: LogLevel
    }
    deriving (Show)
instance A.FromJSON Config where
    parseJSON (A.Object config) = do
        Just logTo    <- config A..: "logTo"
        Just logLevel <- config A..: "logMinLevel"
        Just logPath  <- config A..: "logPath"
        let cLogPath = case logPath of
                          "" -> "log.txt"
                          _  -> logPath
        return Config { logTo       = logTo
                      , logPath     = cLogPath
                      , logMinLevel = logLevel
                      }

data Handle = Handle 
    { hConfig :: Config  }
    deriving Show

-- Create Handle with Config
withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = f $ Handle config

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
log (Handle {..}) lvl str | lvl >= logMinLevel hConfig = case logTo hConfig of
                                                           LogToFile -> logToFile hConfig lvl str
                                                           _         -> logToConsole lvl str
                          | otherwise                  = return ()

debug, info, warning, error :: Handle -> String -> IO ()
debug   h = log h Debug
info    h = log h Info
warning h = log h Warning
error   h = log h Error

configTest = Config 
    { logTo = LogToFile
    , logPath = "log.txt" 
    , logMinLevel = Debug
    }
handleTest = Handle {  hConfig = configTest }