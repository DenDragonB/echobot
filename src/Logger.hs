{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger 
    ( LogLevel (..)
    , LogTo (..)
    , Config (..)
    , Handle (..)

    , withHandle

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
        case T.toUpper t of
            "DEBUG"   -> pure Debug
            "INFO"    -> pure Info
            "WARNING" -> pure Warning
            "ERROR"   -> pure Error
            _         -> fail $ "Unknown log level: " ++ T.unpack t

data LogTo
    = LogToFile
    | LogToConsole
    deriving (Eq, Ord, Show)
instance A.FromJSON LogTo where
    parseJSON = A.withText "FromJSON Logger.LogTo" $ \t ->
        case T.toUpper t of
            "LOFTOFILE"    -> pure LogToFile
            "LOGTOCONSOLE" -> pure LogToConsole
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
    { debug :: String -> IO ()
    , info :: String -> IO ()
    , warning :: String -> IO ()
    , error :: String -> IO ()
    }
instance Show Handle where
    show _ = "Logger services" 

-- Create Handle with Config
withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = f $ Handle
    { debug   = log config Debug
    , info    = log config Info
    , warning = log config Warning
    , error   = log config Error
    }

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
log :: Config -> LogLevel -> String -> IO ()
log conf lvl str | lvl >= logMinLevel conf = case logTo conf of
                                LogToFile -> logToFile conf lvl str
                                _         -> logToConsole lvl str
                 | otherwise               = return ()