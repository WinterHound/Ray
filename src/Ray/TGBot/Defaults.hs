{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Ray.TGBot.Defaults where

import           Lens.Micro.Platform
import           TD.Data.BotCommand
import           TD.Data.FormattedText
import           TD.Data.InlineKeyboardButton
import           TD.Data.InlineKeyboardButtonType
import           TD.Data.InputFile
import           TD.Data.InputMessageContent
import           TD.Data.KeyboardButton
import           TD.Data.KeyboardButtonType
import           TD.Data.MessageSendOptions
import           TD.Data.ReplyMarkup
import           TD.Data.TextEntities
import           TD.Data.TextEntity
import           TD.Query.AddFileToDownloads
import           TD.Query.CancelDownloadFile
import           TD.Query.DownloadFile
import           TD.Query.EditInlineMessageText
import           TD.Query.EditMessageText
import           TD.Query.SearchFileDownloads
import           TD.Query.SendMessage
import           TD.Query.SetCommands

import           Ray.Conf                         (ChatID, ChatQueue, FileID,
                                                   MessageID, UserID)
import           Ray.TH


------------------------------------------------------------------------------------------


_SearchFileDownloads :: SearchFileDownloads
_SearchFileDownloads = SearchFileDownloads
                       { limit = Just 100
                       , offset = Just ""
                       , only_completed = Just False
                       , only_active = Just False
                       , query = Just  ""
                       }

------------------------------------------------------------------------------------------

_AddFileToDownloads :: AddFileToDownloads
_AddFileToDownloads = AddFileToDownloads
                      { priority = Just 1
                      , message_id = Nothing
                      , chat_id = Nothing
                      , file_id = Nothing
                      }


------------------------------------------------------------------------------------------


_downloadFile :: FileID -> DownloadFile
_downloadFile fid = _DownloadFile & file_id_DFe ?~ fid


_DownloadFile :: DownloadFile
_DownloadFile = DownloadFile
                { synchronous = Just True
                , limit = Just 0
                , offset = Just 0
                , priority = Just 1
                , file_id = Nothing
                }

------------------------------------------------------------------------------------------

_cancelDownloadFile :: FileID -> CancelDownloadFile
_cancelDownloadFile x = CancelDownloadFile (Just False) (Just x)


_CancelDownloadFile :: CancelDownloadFile
_CancelDownloadFile = CancelDownloadFile
                      { only_if_pending = Just False
                      , file_id = Nothing }

------------------------------------------------------------------------------------------

_ReplyMarkupShowKeyboard :: ReplyMarkup
_ReplyMarkupShowKeyboard = ReplyMarkupShowKeyboard
      { input_field_placeholder = Just "Marss ... Duness"
      , is_personal = Just False
      , one_time = Just True
      , resize_keyboard = Just True
      , rows = Just [ keyboard $ Just <$> ["Start", "Settings"]
                    , keyboard $ Just <$> ["Dunes", "Gold"]
                    , [KeyboardButton (Just (KeyboardButtonTypeWebApp (Just "https://anonfiles.com"))) (Just "AnonFiles")]]
      }

keyboard :: [Maybe String] -> [KeyboardButton]
keyboard = (<$>) (KeyboardButton (Just KeyboardButtonTypeText))

------------------------------------------------------------------------------------------

type Message = String

_sendMessage :: ChatID -> Message -> SendMessage
_sendMessage cid msg = _SendMessage
                      & chat_id_SMe ?~ cid
                      & input_message_content_SMe ?~ _InputMessageContent
                      & input_message_content_SMe . _Just . text_IMCt . _Just . text_FTt ?~ msg


_replyMessage :: ChatID -> MessageID -> Message -> SendMessage
_replyMessage cID mID msg = _SendMessage
                      & chat_id_SMe ?~ cID
                      & reply_to_message_id_SMe ?~ mID
                      & input_message_content_SMe ?~ _InputMessageContent
                      & input_message_content_SMe . _Just . text_IMCt . _Just . text_FTt ?~ msg




_sendFile :: FileID -> String -> SendMessage
_sendFile cID msg = _SendMessage
                    & chat_id_SMe ?~ cID
                    & input_message_content_SMe ?~ _InputMessageDocument
                    & input_message_content_SMe . _Just . document_IMCt . _Just . path_IFe ?~ msg




_sendMessageInlineKeyboard :: ChatID -> Message -> SendMessage
_sendMessageInlineKeyboard cid msg = _SendMessage
                                     & chat_id_SMe ?~ cid
                                     & reply_markup_SMe ?~ _ReplyMarkupInlineKeyboard
                                     & input_message_content_SMe ?~ _InputMessageContent
                                     & input_message_content_SMe . _Just . text_IMCt . _Just . text_FTt ?~ msg



_sendMessageKeyboard :: ChatID -> Message -> SendMessage
_sendMessageKeyboard cid msg = _SendMessage
                      & chat_id_SMe ?~ cid
                      & reply_markup_SMe ?~ _ReplyMarkupShowKeyboard
                      & input_message_content_SMe ?~ _InputMessageContent
                      & input_message_content_SMe . _Just . text_IMCt . _Just . text_FTt ?~ msg


_SendMessage :: SendMessage
_SendMessage =  SendMessage
                      { input_message_content = Nothing
                      , reply_markup = Nothing
                      , options = Just _MessageSendOptions
                      , reply_to_message_id = Just 0
                      , message_thread_id = Just 0
                      , chat_id = Nothing
                      }


_ReplyMarkupInlineKeyboard ::  ReplyMarkup
_ReplyMarkupInlineKeyboard =  ReplyMarkupInlineKeyboard { _rows = Just [[cancelButton]] }


-- bb_5 :: InlineKeyboardButton
-- bb_5 = InlineKeyboardButton { _type  = Just (InlineKeyboardButtonTypeUrl (Just "https://anonfiles.com/"))
--                             , text = Just "anon"
--                             }



cancelButton :: InlineKeyboardButton
cancelButton = InlineKeyboardButton { _type = Just (InlineKeyboardButtonTypeCallback (Just "cancel0000000000"))
                                      , text = Just "Cancel"
                                      }



------------------------------------------------------------------------------------------
-- Functions



_editMessageText :: ChatID -> MessageID -> String -> EditMessageText
_editMessageText cID mID msg = _EditMessageText
                               & chat_id_EMTt ?~ cID
                               & message_id_EMTt ?~ mID
                               & input_message_content_EMTt ?~ _InputMessageContent
                               & input_message_content_EMTt . _Just . text_IMCt . _Just . text_FTt ?~ msg


_sendMessageInlineKeyboardEdited :: ChatID -> MessageID -> Message -> EditMessageText
_sendMessageInlineKeyboardEdited cID mID msg = _EditMessageText
                                               & chat_id_EMTt ?~ cID
                                               & message_id_EMTt ?~ mID
                                               & reply_markup_EMTt ?~ _ReplyMarkupInlineKeyboard
                                               & input_message_content_EMTt ?~ _InputMessageContent
                                               & input_message_content_EMTt . _Just . text_IMCt . _Just . text_FTt ?~ msg




_editInlineMessageText :: MessageID -> Message -> EditInlineMessageText
_editInlineMessageText mID msg = _EditInlineMessageText
                                 & input_message_content_EIMTt ?~ _InputMessageContent
                                 & input_message_content_EIMTt . _Just . text_IMCt . _Just . text_FTt ?~ msg
                                 & reply_markup_EIMTt ?~ _ReplyMarkupInlineKeyboard
                                 & inline_message_id_EIMTt ?~ show mID



_EditInlineMessageText :: EditInlineMessageText
_EditInlineMessageText = EditInlineMessageText
                         { input_message_content = Nothing
                         , reply_markup = Just _ReplyMarkupInlineKeyboard
                         , inline_message_id = Nothing
                         }



_EditMessageText :: EditMessageText
_EditMessageText = EditMessageText
                   { input_message_content = Nothing
                   , reply_markup = Nothing
                   , message_id = Nothing
                   , chat_id = Nothing
                   }


------------------------------------------------------------------------------------------
-- Text

_InputMessageContent :: InputMessageContent
_InputMessageContent =  InputMessageText
                              { clear_draft = Just True
                              , disable_web_page_preview = Just True
                              , text = Just _FormattedText
                              }



_FormattedText :: FormattedText
_FormattedText = FormattedText
                       { entities = Just []
                       , text = Nothing
                       }


-- _TextEntities :: TextEntities
-- _TextEntities =  TextEntities
--                  {
--                    entities = Just [_TextEntity]
--                  }

_TextEntity :: TextEntity
_TextEntity = TextEntity
              { _type = Nothing
              , _length = Just 100
              ,  offset = Just 0
              }



------------------------------------------------------------------------------------------
-- Doc


_InputMessageDocument :: InputMessageContent
_InputMessageDocument = InputMessageDocument
                              { caption = Nothing
                              , disable_content_type_detection = Just True
                              , thumbnail = Nothing
                              , document = Just _InputFile
                              }



_InputFile :: InputFile
_InputFile = InputFileLocal { path = Just "" }


------------------------------------------------------------------------------------------

_MessageSendOptions :: MessageSendOptions
_MessageSendOptions = MessageSendOptions
                            { scheduling_state = Nothing
                            , protect_content = Just True
                            , from_background = Just True
                            , disable_notification = Just True
                            }

------------------------------------------------------------------------------------------

_SetCommands :: SetCommands
_SetCommands = SetCommands { commands = Just _BotCommand
                           , language_code = Nothing
                           , scope = Nothing
                           }

_BotCommand :: [BotCommand]
_BotCommand = [ BotCommand (Just "Welcome") (Just "start")]

------------------------------------------------------------------------------------------
