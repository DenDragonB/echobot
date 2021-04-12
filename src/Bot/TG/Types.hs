{-# LANGUAGE OverloadedStrings #-}

module Bot.TG.Types where

import           Data.Aeson

-- Type of Response from Telegram bots API
data Response = Response 
    { responseOk :: Bool
    , responseResult :: [Update] 
    }
    deriving Show
instance FromJSON Response where
    parseJSON = withObject "FromJSON TG.Types.Response" $ \o -> 
        Response <$> o .: "ok"
                 <*> o .: "result"

-- Type of Update response as describing in Telegram bots API
data Update = UpMessge 
    { updateId :: Integer
    , message  :: Maybe Мessage
    } deriving (Show, Eq)
instance FromJSON Update where
    parseJSON = withObject "FromJSON TG.Types.Update" $ \o -> do
        upid   <- o .: "update_id"
        uptype <- o .: "message"
        return $ UpMessge upid uptype

-- Type of Message as describing in Telegram bots API
data Мessage = Мessage 
    { messageId :: Integer
    , messageChat :: TGChat
    , messageDate :: Integer
    , messageFrom :: Maybe TGUser    
    , messageText :: Maybe String
    } deriving (Show, Eq)
instance FromJSON Мessage where
    parseJSON = withObject "FromJSON TG.Types.Мessage" $ \o -> do
        mid   <- o .: "message_id"
        mchat <- o .:  "chat"
        mdate <- o .:  "date"
        mfrom <- o .:  "from"
        mtext <- o .:  "text"
        return $ Мessage mid mchat mdate mfrom mtext

-- Type of User as describing in Telegram bots API, added IsBot and Language fields
data TGUser = TGUser 
    { tgUserId :: Integer
    , tgUserName :: String
    } deriving (Show, Eq)
instance FromJSON TGUser where
    parseJSON = withObject "FromJSON TG.Types.TGUser" $ \o -> 
        TGUser <$> o .: "id"
               <*> o .: "username"                  

-- Type of Chat as describing in Telegram bots API
data TGChat = TGChat 
    { tgChatId :: Integer
    } deriving (Show, Eq)
instance FromJSON TGChat where
    parseJSON = withObject "FromJSON TG.Types.TGChat" $ \o -> 
        TGChat <$> o .: "id"                  

-- Type of custom Keyboard as describing in Telegram bots API
data TGReplyKeyboardMarkup = TGKeyBoard
    { tgButtons     :: [[TGButton]]
    , tgKBResize    :: Bool
    , tgKBOneTime   :: Bool
    , tgKBSelective :: Bool
    } deriving (Show, Eq)
instance FromJSON TGReplyKeyboardMarkup where
    parseJSON = withObject "FromJSON TG.Types.TGButton" $ \keyboard -> do
        TGKeyBoard  <$> keyboard .:  "keyboard"
        -- This fields are optional
                    <*> keyboard .:? "resize_keyboard"   .!= False
                    <*> keyboard .:? "one_time_keyboard" .!= False
                    <*> keyboard .:? "selective"         .!= False
instance ToJSON TGReplyKeyboardMarkup where
    toJSON keyboard = object [ "keyboard"          .= tgButtons keyboard
                             , "resize_keyboard"   .= tgKBResize keyboard
                             , "one_time_keyboard" .= tgKBOneTime keyboard
                             , "selective"         .= tgKBSelective keyboard
                             ]    

data TGButton = TGButton
    { tgButtonText :: String
    } deriving (Show, Eq)
instance FromJSON TGButton where
    parseJSON = withObject "FromJSON TG.Types.TGButton" $ \o -> 
        TGButton <$> o .: "text" 
instance ToJSON TGButton where
    toJSON button = object [ "text" .= tgButtonText button
                           ]