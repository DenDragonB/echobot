{-# LANGUAGE OverloadedStrings #-}

module Bot.VKLib where

import           Data.Aeson

data Response = Response { ts      :: String     -- number of last events
                         , updates :: [Update]  -- new events
                         }

data Update = Update { upType    :: UpType 
                     , upObject  :: UpObject
                     , upGroupID :: Integer
                     }

data UpType = MessageNew | MessageEvent

newtype UpObject = UpObject { objMessage :: ObjMessage}

data ObjMessage = ObjMessage { mesId :: Integer
                             , fromID :: Integer
                             , text :: String 
                             , attach :: [Attachment]}

data Attachment  = AtPhoto { photo :: Photo }
                 | AtVideo { video :: Video }
                 | AtAudio { audio :: Audio }
                 | AtDocument { doc :: Document }
                 | AtLink { link :: Link } 
                 | AtMarket { market :: Market }
                 | AtMarketAlbum { marketA :: MarketAlbum }
                 | AtWall { wall :: Wall }
                 | AtWallReply { wallR :: WallReply }
                 | AtSticker { sticker :: Sticker } 
                 | AtGift { gift :: Gift }

data Photo = Photo { photoId :: Integer 
                   , ownerId :: Integer 
                   }
data Video = Video { videoId :: Integer }
data Audio = Audio { audioId :: Integer }
data Document = Doc { docId :: Integer }
data Link = Link { url   :: String 
                 , title :: String 
                 }
data Market = Market { marketId :: Integer }
data MarketAlbum = MarketAlbum { mAlbumId :: Integer }
data Wall = Wall { wallId :: Integer }
data WallReply = WallReply { wReplyId :: Integer}
data Sticker = Sticker { prodID  :: Integer
                       , stickID :: Integer} 
data Gift = Gift { giftId :: Integer }