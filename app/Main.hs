{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bot
import qualified Bot.TG          as TG
import qualified Bot.VK          as VK
import qualified Bot.VK.Types    as VKTypes
import qualified Data.Aeson      as Aeson
import qualified Data.ByteString as BS
import qualified Data.Yaml       as YAML
import qualified Logger

data Config = Config
    { cLogger :: Logger.Config
    , startOn :: String
    , cBot    :: Bot.Config
    , cBotTG  :: TG.Config
    , cBotVK  :: VKTypes.Config
    }
    deriving (Show)
instance Aeson.FromJSON Config where
    parseJSON = Aeson.withObject "FromJSON Main.Config" $ \o ->
        Config
            <$> o Aeson..: "logger"
            <*> o Aeson..: "startOn"
            <*> o Aeson..: "user"
            <*> o Aeson..: "telegram"
            <*> o Aeson..: "vk"

main :: IO ()
main = do
    -- read config
    tMainConf <- BS.readFile "app/main.yaml"
    tUserConf <- BS.readFile "config.yaml"

    let config = YAML.decodeEither' $  tMainConf <> tUserConf

    case config of
        Left str -> print str
        Right conf -> do
            -- Logger.info hlogger "Logger is work"
            Logger.withHandle (cLogger conf) $ \logger ->
                Bot.withHandle (cBot conf) $ \bot -> case startOn conf of
                    "vk" ->
                        VK.withHandle logger bot (cBotVK conf) $ \handle -> do
                            Logger.debug logger $ show handle
                            _ <- VK.todo handle $ VK.State Bot.emptyUsers (VK.hServer handle) Nothing
                                (VKTypes.startTs $ VK.hServer handle)
                            return ()
                    _ -> TG.withHandle logger bot (cBotTG conf) $ \handle -> do
                        Logger.debug logger $ show handle
                        _ <- TG.todo handle $ TG.State Bot.emptyUsers Nothing 0
                        return ()
