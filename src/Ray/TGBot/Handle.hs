{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ray.TGBot.Handle (handleGeneralResults) where


import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Bimap                           as BM
import           Data.Function                        ((&))
import qualified Data.IntMap.Strict                   as IntMap
import           Data.Maybe                           (fromJust)
import qualified Data.Text                            as T
import           Fmt
import           GHC.Float
import           Lens.Micro.Aeson
import           Lens.Micro.Platform
import qualified Network.Wreq                         as W
import qualified Network.Wreq.Session                 as WS (Session, post)
import           System.Directory                     (removeFile)
import           System.Random                        (randomRIO)
import qualified System.Timeout                       as ST
import           Text.Pretty.Simple                   (pPrint)

import           TD.Data.AuthorizationState
import           TD.Data.CallbackQueryPayload
import           TD.Data.File
import qualified TD.Data.GeneralResult                as G
import           TD.Data.LocalFile
import qualified TD.Data.Message                      as M
import qualified TD.Data.MessageContent               as MC
import           TD.Data.Update
import           TD.Lib
import           TD.Query.CheckAuthenticationBotToken
import           TD.Query.CheckDatabaseEncryptionKey
import           TD.Query.SetTdlibParameters

import           Ray.Conf
import           Ray.TGBot.Defaults
import           Ray.TGBot.Types
import           Ray.TH

------------------------------------------------------------------------------------------

data ChatInfo = ChatInfo { theChatID       :: Int
                         , uploadURL       :: String
                         , theQueue        :: ChatQueue
                         , theChatPool     :: ChatPool
                         , theDownloadPool :: DownloadPool
                         , session         :: WS.Session
                         , tgClient        :: Client
                         , theBotID        :: Int
                         }

makeChatInfo :: ChatQueue -> ChatID -> AppEnv -> ChatInfo
makeChatInfo cq cID AppEnv{ userConfig = UserConfig {..}, ..} =
   ChatInfo { theChatID = cID
            , uploadURL = uploadURL
            , theQueue = cq
            , theChatPool = chatPool
            , theDownloadPool = downloadPool
            , session = session
            , tgClient = tgClient
            , theBotID = currentBotID }

------------------------------------------------------------------------------------------

handleGeneralResults :: G.GeneralResult -> TGBot AppEnv () ()
handleGeneralResults (G.Update up) = handleUpdate up
handleGeneralResults _             = pure ()

------------------------------------------------------------------------------------------


handleUpdate :: Update -> TGBot AppEnv () ()
handleUpdate up = do
  appEnv@AppEnv {..} <- ask
  chatPool_ <- liftIO $ getPool chatPool
  case up of
    UpdateAuthorizationState {} -> handleAuthorizationState up
    UpdateNewMessage (Just M.Message { chat_id }) -> do
      let cid = fromJust chat_id in
        do
          case chatPool_ of
            Nothing -> do
              pPrint "Making new user : case 1"
              newLine <- liftIO $ newTBQueueIO $ maxQueueBoundForChat userConfig
              liftIO $ async $ runTGBot newChat (makeChatInfo newLine cid appEnv) BM.empty
              _ <- let new = IntMap.singleton cid newLine  in
                     liftIO $ atomically $ putTMVar chatPool new
              liftIO $ atomically $ writeTBQueue newLine up
            Just y | y == IntMap.empty -> do
                       pPrint "Making new user : case 2"
                       get >>= pPrint
                       newLine <- liftIO $ newTBQueueIO $ maxQueueBoundForChat userConfig
                       liftIO $ async $ runTGBot newChat (makeChatInfo newLine cid appEnv) BM.empty
                       _ <- let new = IntMap.singleton cid newLine  in
                              liftIO $ atomically $ swapTMVar chatPool new
                       liftIO $ atomically $ writeTBQueue newLine up
            Just x | IntMap.notMember (fromJust chat_id) x -> do
                       pPrint "Making new user : case 3"
                       get >>= pPrint
                       newLine <- liftIO $ newTBQueueIO $ maxQueueBoundForChat userConfig
                       liftIO $ async $ runTGBot newChat (makeChatInfo newLine cid appEnv) BM.empty
                       _ <- let new = IntMap.insert cid newLine x  in
                         liftIO $ atomically $ swapTMVar chatPool new
                       liftIO $ atomically $ writeTBQueue newLine up
            _ -> do
              get >>= pPrint
              pPrint "UpdateNewMessage -- to old user"
              liftIO $ atomically $ writeTBQueue (fromJust chatPool_ IntMap.! fromJust chat_id) up
    UpdateFile (Just File { _id = Just __id }) ->
      -- liftIO $ (IntMap.! __id) <$> fromJust <$> (getPool downloadPool) >>= (\q -> atomically $ writeTBQueue q up)
      -- Alternative
      liftIO $ (getPool downloadPool >>= (\q -> atomically $ writeTBQueue q up) . ((IntMap.! __id) . fromJust))
      `catch` (\(e :: SomeException) -> pPrint "Indexing a non-existing value")
    UpdateMessageSendSucceeded { message = (Just M.Message {chat_id = Just _chat_id})} -> do
      pPrint "UpdateMessageSendSucceeded -- to old user"
      liftIO $ atomically (writeTBQueue (fromJust chatPool_ IntMap.! _chat_id) up) `catch` (\(e :: SomeException) -> pPrint e)
    UpdateNewCallbackQuery {chat_id = Just _chat_id} -> do
      pPrint "UpdateNewCallbackQuery -- to old user"
      liftIO $ atomically (writeTBQueue (fromJust chatPool_ IntMap.! _chat_id) up) `catch` (\(e :: SomeException) -> pPrint e)
    _ -> pure ()



getPool :: TMVar a -> IO (Maybe a)
getPool = atomically . tryReadTMVar


------------------------------------------------------------------------------------------

-- sendToChat :: ChatID -> TGBot AppEnv [Int] ()
-- sendToChat cID = liftIO $ atomically (writeTBQueue (fromJust chatPool_ IntMap.! cID) up) `catch` (\(e :: SomeException) -> pPrint e)

------------------------------------------------------------------------------------------

-- | DocID to MessageID BiMap
type NewChatState = BM.Bimap DocID MessageID


newChat :: TGBot ChatInfo NewChatState ()
newChat = forever $ do
  ChatInfo {..} <- ask
  state_ <- get
  pPrint $ "In the user .... " <> show theChatID
  theUpdate <- liftIO $ ST.timeout (10 * 1000000) $ atomically $ readTBQueue theQueue
  case theUpdate of
    Nothing -> when (null (BM.toList state_)) (liftIO $ deleteChatFromChatPool theChatPool theChatID)
    Just a@UpdateNewMessage {} -> handleUpdateNewMessage a
    Just a@UpdateFile {} -> handleUpdateFile a
    Just a@UpdateMessageSendSucceeded {} -> handleUpdateMessageSendSucceeded a
    Just a@UpdateNewCallbackQuery {} -> handleUpdateNewCallbackQuery a
    _ -> return ()


handleUpdateNewMessage :: Update -> TGBot ChatInfo NewChatState ()
handleUpdateNewMessage (UpdateNewMessage Nothing) = return ()
handleUpdateNewMessage (UpdateNewMessage (Just M.Message {_id = Just __id, ..})) =  do
  pPrint "At ......... handleUpdateNewMessage"
  ChatInfo {..} <- ask
  when (sender_id ^.. _Just . user_id_MSr . _Just /= [theBotID]) $
    case fromJust content of
      MC.MessageText {..}     -> do
        [msg] <- return $ text ^.. _Just . text_FTt . _Just
        case msg of
          "/start" -> liftIO $ send tgClient $ _sendMessage theChatID "Welcome"
          _        ->  return ()
      MC.MessageDocument {document} ->
        do
          [_doc_id] <- return $ document ^.. _Just . document_Dt . _Just . _id_Fe . _Just
          modify (BM.insert _doc_id _doc_id)
          liftIO $ send tgClient $ _replyMessage theChatID __id "Downloading..."
        -- putIDtoState _doc_id
      MC.MessageVideo {video} ->
        do
          [_video_id] <- return $ video ^.. _Just . video_Vo . _Just . _id_Fe . _Just
          modify (BM.insert _video_id _video_id)
          liftIO $ send tgClient $ _replyMessage theChatID __id "Downloading..."
        -- putIDtoState _video_id
      _ -> return ()



handleUpdateNewCallbackQuery :: Update -> TGBot ChatInfo NewChatState ()
handleUpdateNewCallbackQuery UpdateNewCallbackQuery
  { payload = (Just (CallbackQueryPayloadData (Just query))), message_id = Just _message_id } = do
  pPrint "------------------------------------------------------------------------------------------"
  ChatInfo {..} <- ask
  mapVar <- get
  let fID = mapVar BM.!> _message_id in
    case query of
      "cancel0000000000" -> do
        modify (BM.deleteR _message_id)
        liftIO $ send tgClient $ _cancelDownloadFile fID
        liftIO $ send tgClient $ _editMessageText theChatID _message_id "Cancelled ..."
      _ -> return ()
handleUpdateNewCallbackQuery _ = return ()


-- putIDtoState :: FileID -> TGBot ChatInfo NewChatState ()
-- putIDtoState fileID = do
--   ChatInfo {tgClient, theChatID} <- ask
--   modify (BM.insert fileID fileID)
--   liftIO $ send tgClient $ _sendMessage theChatID "Downloading..."

------------------------------------------------------------------------------------------

handleUpdateMessageSendSucceeded :: Update -> TGBot ChatInfo NewChatState ()
handleUpdateMessageSendSucceeded (UpdateMessageSendSucceeded { message = (Just M.Message {..})}) = do
  [msg] <- return $ content ^.. _Just . text_MCt . _Just . text_FTt . _Just
  when (msg == "Downloading...") $ do
    ChatInfo {..} <- ask
    biMap <- get
    pPrint biMap
    let (fileID, _) = head $ BM.toList $ BM.filter (==) biMap
        mID = fromJust _id in do
      modify (BM.insert fileID mID)
      pPrint $ "RHS : " <> show fileID
      liftIO $ addFileIDtoDownloadPool theQueue theDownloadPool fileID
      pPrint "Downlading Now ..."
      liftIO $ send tgClient $ _downloadFile fileID

------------------------------------------------------------------------------------------

handleUpdateFile :: Update -> TGBot ChatInfo NewChatState ()
handleUpdateFile (UpdateFile (Just File {local = (Just LocalFile {..}), ..})) = do
  ChatInfo {..} <- ask
  biMap <- get
  let fileID = fromJust _id
      tSize = fromJust size
      dlSize = fromJust downloaded_size
      uploadStatus = fromJust is_downloading_completed
      filePath = fromJust path
      messageID = biMap BM.! fileID in do
    pPrint "In updateFile"
    pPrint biMap
    if uploadStatus then do
        -- liftIO $ send tgClient $ _editMessageText theChatID messageID "100..........%"
        modify (BM.delete fileID)
        pPrint "Removed a fileID from state"
        liftIO $ deleteFileIDFromDownloadPool theDownloadPool fileID
        pPrint "Removed a fileID from dowloadpool"
        liftIO $ async $ withAsync (do
          send tgClient $ _editMessageText theChatID messageID "Uploading Now ..."
          r <- WS.post session uploadURL (W.partFileSource "file" filePath)
          let [fileLink] = r ^.. W.responseBody . key "data" . key "file" . key "url" . key "full" . _String
          send tgClient $ _editMessageText theChatID messageID (T.unpack fileLink)
          removeFile filePath)
          (\a -> wait a `catch` (\(e :: SomeException) -> do
                                      send tgClient $ _editMessageText theChatID messageID "Something went wrong, Try again."
                                      removeFile filePath))
        return ()
        -- Progress
      else liftIO $ do
      let progress = floor $ (int2Float dlSize / int2Float tSize) * 100 in do
        ran <- randomRIO (1 :: Int, 15)
        when (ran == 1 || ran == 10) . send tgClient $ _sendMessageInlineKeyboardEdited theChatID messageID (progressBar progress 20)
          where
            progressBar :: Int -> Int -> String
            progressBar per s = let p = floor $ int2Float (per * s) / 100 in
                                  ("Progress : \n" +| show per |+ "\n" +| replicate p '◽' <> replicate (s - p) '◾' |+ "")





deleteFileIDFromDownloadPool :: DownloadPool -> FileID -> IO ()
deleteFileIDFromDownloadPool dp fileID = do
  Just downloadPool_ <- atomically $ tryReadTMVar dp
  let new = IntMap.delete fileID downloadPool_
  atomically $ swapTMVar dp new
  pPrint $ "Removed ... " <> show fileID


addFileIDtoDownloadPool :: ChatQueue -> DownloadPool -> FileID -> IO ()
addFileIDtoDownloadPool cq dP fileID  = do
  Just downloadPool_ <- atomically $ tryReadTMVar dP
  let new = IntMap.insert fileID cq downloadPool_
  atomically $ swapTMVar dP new
  pPrint $ "Added ... " <> show fileID <> " : queue"


deleteChatFromChatPool :: ChatPool -> ChatID -> IO ()
deleteChatFromChatPool pool cID  = do
  Just chatPool_ <- atomically $ tryReadTMVar pool
  let new = IntMap.delete cID chatPool_
  atomically $ swapTMVar pool new
  pPrint $ "Chat: ... " <> show cID <> " ... Exited"
  fail $ "Timeout for " <> show cID

------------------------------------------------------------------------------------------

handleAuthorizationState :: Update -> TGBot AppEnv s ()
handleAuthorizationState UpdateAuthorizationState {..} = do
  AppEnv {..} <- ask
  case authorization_state of
    Just AuthorizationStateWaitTdlibParameters ->
      liftIO
      $ pPrint "At ... StateWaitTdlibParameters"
      >> send tgClient SetTdlibParameters { parameters = Just (tdlibParameters userConfig) }
    Just (AuthorizationStateWaitEncryptionKey _) ->
      liftIO
      $ pPrint "At ... StateWaitEncryptionKey"
      >> send tgClient CheckDatabaseEncryptionKey {encryption_key = Just "randomencryption"}
    Just AuthorizationStateWaitPhoneNumber ->
      liftIO
      $ pPrint "At ... StateWaitPhoneNumber"
      >> send tgClient CheckAuthenticationBotToken {token = Just (botToken userConfig)}
    _ -> pure ()
handleAuthorizationState _ = pure ()

------------------------------------------------------------------------------------------

-- automically writeTBQueue ((fromJust <$> getPool downloadPool) IntMap.! __id) up
-- (IntMap.! __id) <$> fromJust <$> (getPool downloadPool) >>= (\q -> automically $ writeTBQueue q up)

------------------------------------------------------------------------------------------
  -- (do
  --     vQueue <- (\x y -> y IntMap.! (x IntMap.! __id))
  --       <$> (fromJust <$> getPool downloadPool)
  --       <*> pure (fromJust chatPool_)
  --     atomically $ writeTBQueue vQueue up) `catch` (\(e :: SomeException) -> pPrint e)
