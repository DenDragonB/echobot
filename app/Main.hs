{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Text.Toml as TOML

import qualified Logger as Logger
import qualified Bot as Bot
import qualified Bot.TG as TG
import qualified Bot.VK as VK
import qualified Bot.VK.Types as VKTypes

--import qualified Bot.VK as VK

data Config = Config
    { cLogger :: Logger.Config
    , startOn :: String
    , cBot    :: Bot.Config
    , cBotTG  :: TG.Config
    , cBotVK  :: VKTypes.Config
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
    let config = Aeson.eitherDecode $ Aeson.encode toml :: Either String Config
    case config of
        Left str   -> print str
        Right conf -> do
            -- Logger.info hlogger "Logger is work"
            Logger.withHandle (cLogger conf) $ \logger ->
                Bot.withHandle (cBot conf) $ \bot ->
                    case startOn conf of
                        "vk" -> VK.withHandle logger bot (cBotVK conf) $ \handle -> do
                                    Logger.debug logger $ show handle
                                    _ <- VK.todo handle
                                    return ()
                        _    -> TG.withHandle logger bot (cBotTG conf) $ \handle -> do
                                    Logger.debug logger $ show handle
                                    _ <- TG.todo handle
                                    return ()