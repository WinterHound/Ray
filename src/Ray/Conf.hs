{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Ray.Conf where

import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Concurrent.STM.TMVar   (TMVar)
import           Data.Bimap                     (Bimap)
import           Data.Conf
import           Data.IntMap.Strict             (IntMap)
import           Network.Wreq.Session           (Session)
import           Numeric.Natural                (Natural)
import           TD.Data.TdlibParameters
import           TD.Data.Update                 (Update)
import           TD.Lib                         (Client)


------------------------------------------------------------------------------------------

deriving instance Read TdlibParameters


type ChatID = Int
type UserID = Int
type MessageID = Int
type DocID = Int
type FileID = Int

data UserConfig = UserConfig {
  tdlibParameters        :: TdlibParameters
  , botToken             :: String
  , uploadURL            :: String
  , maxQueueBoundForChat :: Natural
  , currentBotID         :: Int
  }
-- , maxNoUsers           :: Natural


type ChatQueue = TBQueue Update
-- ChatID -> ChatQueue

type ChatPool = TMVar (IntMap ChatQueue)
-- ChatID -> ChatQueue

type DownloadPool = TMVar (IntMap ChatQueue)
-- FileID -> ChatQueue

data AppEnv = AppEnv {
    tgClient     :: Client
  , userConfig   :: UserConfig
  , session      :: Session
  , chatPool     :: ChatPool
  , downloadPool :: DownloadPool
  }

getUserConfig :: String -> IO UserConfig
getUserConfig filePath = do
  userConfMap <- readConf filePath
  let getV :: forall a m. (Read a, MonadFail m) => String -> m a
      getV = gV userConfMap in
    if | null userConfMap -> fail "Failed To Parse : ~/rayConf.hs"
       | otherwise -> UserConfig
                      <$> getV @TdlibParameters "tdlibParameters"
                      <*> getV @String "botToken"
                      <*> getV @String "uploadURL"
                      <*> getV @Natural "maxQueueBoundForChat"
                      <*> getV @Int "currentBotID"
  where
    gV :: forall a m. (Read a, MonadFail m) => Conf -> String -> m a
    gV conf key = do
      let value = getConf @a key conf in
        case value of
          Just result -> return result
          Nothing     -> fail $ "Error in getting \"" <> key <> "\" value"


------------------------------------------------------------------------------------------
-- OLD

-- getUserConfig :: String -> IO UserConfig
-- getUserConfig filePath = do
--   userConfMap <- readConf filePath
--   if | null userConfMap -> fail "No settings.hs file"
--      | otherwise -> do
--          let botToken_ = getConf @String "botToken" userConfMap
--              tdlibParameters_ = getConf @TdlibParameters "tdlibParameters" userConfMap
--              noThreads_ = getConf @Int "noThreads" userConfMap
--              maxQueueBound_ = getConf @Int "maxQueueBound" userConfMap in
--            if | isNothing botToken_  -> fail "Error in parsing botToken value"
--               | isNothing tdlibParameters_  -> fail "Error in parsing tdlibParameters value"
--               | isNothing noThreads_  -> fail "Error in parsing noThreads value"
--               | isNothing maxQueueBound_ -> fail "Error in parsing maxQueueBound value"
--               | otherwise    -> return $ UserConfig (fromJust botToken_) (fromJust tdlibParameters_) (fromJust noThreads_) (fromJust maxQueueBound_)


