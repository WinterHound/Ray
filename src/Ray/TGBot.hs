{-# LANGUAGE RecordWildCards #-}

module Ray.TGBot ( loopTGBot, tgBotIO ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.IntMap                   as IntMap
import           Network.Wreq.Session          (newSession)
import           System.Directory              (getHomeDirectory)
import qualified TD.Data.GeneralResult         as G
import           TD.Lib
import           TD.Query.SetLogVerbosityLevel
import           Text.Pretty.Simple            (pPrint)

import           Ray.Conf
import           Ray.TGBot.Handle              (handleGeneralResults)
import           Ray.TGBot.Types

------------------------------------------------------------------------------------------

tgBotIO :: IO ()
tgBotIO = do
  home <- getHomeDirectory
  settings <- getUserConfig (home <> "/rayConf.hs")
  client <- create
  newPortal <- newSession
  emptyChatPool <- newEmptyTMVarIO
  emptyDownloadPool <- newTMVarIO IntMap.empty
  send client SetLogVerbosityLevel {new_verbosity_level = Just 2}
  runTGBot loopTGBot (AppEnv client settings newPortal emptyChatPool emptyDownloadPool) ()


loopTGBot :: TGBot AppEnv () ()
loopTGBot = forever $ do
  AppEnv {..} <- ask
  tgValue <- liftIO $ receive tgClient
  case tgValue of
    Nothing                               -> pPrint "At ............... Nothing"
    Just (G.ResultWithExtra result extra) -> do
      pPrint "result ..... extra"
      pPrint result
      handleGeneralResults result


