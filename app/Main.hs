{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Text.Toml as TOML

import qualified Logger as Logger
import qualified Bot as Bot
import qualified Bot.TG as TG
import qualified Bot.VK as VK
import qualified Bot.VKLib as VKLib

--import qualified Bot.VK as VK

data Config = Config
    { cLogger :: Logger.Config
    , startOn :: String
    , cBot    :: Bot.Config
    , cBotTG  :: TG.Config
    , cBotVK  :: VKLib.Config
    }
    deriving Show
instance Aeson.FromJSON Config where
    parseJSON = Aeson.withObject "FromJSON Main.Config" $ \o -> Config
        <$> o Aeson..: "logger"   
        <*> o Aeson..: "startOn"
        <*> o Aeson..: "user"
        <*> o Aeson..: "telegram" 
        <*> o Aeson..: "vk"


main :: IO ()
main = do
    -- read config
    tMainConf <- readFile "app/main.conf"
    tUserConf <- readFile "config.conf"
    let Right toml = TOML.parseTomlDoc "" $ T.pack $ tUserConf ++ tMainConf 
    let Just conf = Aeson.decode $ Aeson.encode toml :: Maybe Config      

    -- Logger.info hlogger "Logger is work"
    Logger.withHandle (cLogger conf) $ \logger ->
        Bot.withHandle (cBot conf) $ \bot ->
            case startOn conf of
                "vk" -> VK.withHandle logger bot (cBotVK conf) $ \handle -> do
                            Logger.debug logger $ show handle
                            newHandle <- VK.todo handle
                            return ()
                _    -> TG.withHandle logger bot (cBotTG conf) $ \handle -> do
                            Logger.debug logger $ show handle
                            newHandle <- TG.todo handle
                            return ()