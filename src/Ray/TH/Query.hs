{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Ray.TH.Query where

import           Language.Haskell.TH
import           Lens.Micro.Platform
import           Ray.TH.Utils

import           TD.Query.AcceptCall
import           TD.Query.AcceptTermsOfService
import           TD.Query.AddChatMember
import           TD.Query.AddChatMembers
import           TD.Query.AddChatToList
import           TD.Query.AddContact
import           TD.Query.AddCustomServerLanguagePack
import           TD.Query.AddFavoriteSticker
import           TD.Query.AddFileToDownloads
import           TD.Query.AddLocalMessage
import           TD.Query.AddLogMessage
import           TD.Query.AddNetworkStatistics
import           TD.Query.AddProxy
import           TD.Query.AddRecentSticker
import           TD.Query.AddRecentlyFoundChat
import           TD.Query.AddSavedAnimation
import           TD.Query.AddSavedNotificationSound
import           TD.Query.AddStickerToSet
import           TD.Query.AnswerCallbackQuery
import           TD.Query.AnswerCustomQuery
import           TD.Query.AnswerInlineQuery
import           TD.Query.AnswerPreCheckoutQuery
import           TD.Query.AnswerShippingQuery
import           TD.Query.AnswerWebAppQuery
import           TD.Query.BanChatMember
import           TD.Query.BlockMessageSenderFromReplies
import           TD.Query.CanTransferOwnership
import           TD.Query.CancelDownloadFile
import           TD.Query.CancelPasswordReset
import           TD.Query.CancelUploadFile
import           TD.Query.ChangeImportedContacts
import           TD.Query.ChangePhoneNumber
import           TD.Query.ChangeStickerSet
import           TD.Query.CheckAuthenticationBotToken
import           TD.Query.CheckAuthenticationCode
import           TD.Query.CheckAuthenticationPassword
import           TD.Query.CheckAuthenticationPasswordRecoveryCode
import           TD.Query.CheckChangePhoneNumberCode
import           TD.Query.CheckChatInviteLink
import           TD.Query.CheckChatUsername
import           TD.Query.CheckCreatedPublicChatsLimit
import           TD.Query.CheckDatabaseEncryptionKey
import           TD.Query.CheckEmailAddressVerificationCode
import           TD.Query.CheckPasswordRecoveryCode
import           TD.Query.CheckPhoneNumberConfirmationCode
import           TD.Query.CheckPhoneNumberVerificationCode
import           TD.Query.CheckRecoveryEmailAddressCode
import           TD.Query.CheckStickerSetName
import           TD.Query.CleanFileName
import           TD.Query.ClearAllDraftMessages
import           TD.Query.ClearImportedContacts
import           TD.Query.ClearRecentStickers
import           TD.Query.ClearRecentlyFoundChats
import           TD.Query.ClickAnimatedEmojiMessage
import           TD.Query.Close
import           TD.Query.CloseChat
import           TD.Query.CloseSecretChat
import           TD.Query.CloseWebApp
import           TD.Query.ConfirmQrCodeAuthentication
import           TD.Query.CreateBasicGroupChat
import           TD.Query.CreateCall
import           TD.Query.CreateChatFilter
import           TD.Query.CreateChatInviteLink
import           TD.Query.CreateNewBasicGroupChat
import           TD.Query.CreateNewSecretChat
import           TD.Query.CreateNewStickerSet
import           TD.Query.CreateNewSupergroupChat
import           TD.Query.CreatePrivateChat
import           TD.Query.CreateSecretChat
import           TD.Query.CreateSupergroupChat
import           TD.Query.CreateTemporaryPassword
import           TD.Query.CreateVideoChat
import           TD.Query.DeleteAccount
import           TD.Query.DeleteAllCallMessages
import           TD.Query.DeleteAllRevokedChatInviteLinks
import           TD.Query.DeleteChat
import           TD.Query.DeleteChatFilter
import           TD.Query.DeleteChatHistory
import           TD.Query.DeleteChatMessagesByDate
import           TD.Query.DeleteChatMessagesBySender
import           TD.Query.DeleteChatReplyMarkup
import           TD.Query.DeleteCommands
import           TD.Query.DeleteFile
import           TD.Query.DeleteLanguagePack
import           TD.Query.DeleteMessages
import           TD.Query.DeletePassportElement
import           TD.Query.DeleteProfilePhoto
import           TD.Query.DeleteRevokedChatInviteLink
import           TD.Query.DeleteSavedCredentials
import           TD.Query.DeleteSavedOrderInfo
import           TD.Query.Destroy
import           TD.Query.DisableProxy
import           TD.Query.DiscardCall
import           TD.Query.DisconnectAllWebsites
import           TD.Query.DisconnectWebsite
import           TD.Query.DownloadFile
import           TD.Query.EditChatFilter
import           TD.Query.EditChatInviteLink
import           TD.Query.EditCustomLanguagePackInfo
import           TD.Query.EditInlineMessageCaption
import           TD.Query.EditInlineMessageLiveLocation
import           TD.Query.EditInlineMessageMedia
import           TD.Query.EditInlineMessageReplyMarkup
import           TD.Query.EditInlineMessageText
import           TD.Query.EditMessageCaption
import           TD.Query.EditMessageLiveLocation
import           TD.Query.EditMessageMedia
import           TD.Query.EditMessageReplyMarkup
import           TD.Query.EditMessageSchedulingState
import           TD.Query.EditMessageText
import           TD.Query.EditProxy
import           TD.Query.EnableProxy
import           TD.Query.EndGroupCall
import           TD.Query.EndGroupCallRecording
import           TD.Query.EndGroupCallScreenSharing
import           TD.Query.FinishFileGeneration
import           TD.Query.ForwardMessages
import           TD.Query.GetAccountTtl
import           TD.Query.GetActiveLiveLocationMessages
import           TD.Query.GetActiveSessions
import           TD.Query.GetAllPassportElements
import           TD.Query.GetAnimatedEmoji
import           TD.Query.GetApplicationConfig
import           TD.Query.GetApplicationDownloadLink
import           TD.Query.GetArchivedStickerSets
import           TD.Query.GetAttachedStickerSets
import           TD.Query.GetAttachmentMenuBot
import           TD.Query.GetAuthorizationState
import           TD.Query.GetAutoDownloadSettingsPresets
import           TD.Query.GetBackgroundUrl
import           TD.Query.GetBackgrounds
import           TD.Query.GetBankCardInfo
import           TD.Query.GetBasicGroup
import           TD.Query.GetBasicGroupFullInfo
import           TD.Query.GetBlockedMessageSenders
import           TD.Query.GetCallbackQueryAnswer
import           TD.Query.GetCallbackQueryMessage
import           TD.Query.GetChat
import           TD.Query.GetChatAdministrators
import           TD.Query.GetChatAvailableMessageSenders
import           TD.Query.GetChatEventLog
import           TD.Query.GetChatFilter
import           TD.Query.GetChatFilterDefaultIconName
import           TD.Query.GetChatHistory
import           TD.Query.GetChatInviteLink
import           TD.Query.GetChatInviteLinkCounts
import           TD.Query.GetChatInviteLinkMembers
import           TD.Query.GetChatInviteLinks
import           TD.Query.GetChatJoinRequests
import           TD.Query.GetChatListsToAddChat
import           TD.Query.GetChatMember
import           TD.Query.GetChatMessageByDate
import           TD.Query.GetChatMessageCalendar
import           TD.Query.GetChatMessageCount
import           TD.Query.GetChatNotificationSettingsExceptions
import           TD.Query.GetChatPinnedMessage
import           TD.Query.GetChatScheduledMessages
import           TD.Query.GetChatSparseMessagePositions
import           TD.Query.GetChatSponsoredMessage
import           TD.Query.GetChatStatistics
import           TD.Query.GetChats
import           TD.Query.GetCommands
import           TD.Query.GetConnectedWebsites
import           TD.Query.GetContacts
import           TD.Query.GetCountries
import           TD.Query.GetCountryCode
import           TD.Query.GetCreatedPublicChats
import           TD.Query.GetCurrentState
import           TD.Query.GetDatabaseStatistics
import           TD.Query.GetDeepLinkInfo
import           TD.Query.GetEmojiSuggestionsUrl
import           TD.Query.GetExternalLink
import           TD.Query.GetExternalLinkInfo
import           TD.Query.GetFavoriteStickers
import           TD.Query.GetFile
import           TD.Query.GetFileDownloadedPrefixSize
import           TD.Query.GetFileExtension
import           TD.Query.GetFileMimeType
import           TD.Query.GetGameHighScores
import           TD.Query.GetGroupCall
import           TD.Query.GetGroupCallInviteLink
import           TD.Query.GetGroupCallStreamSegment
import           TD.Query.GetGroupCallStreams
import           TD.Query.GetGroupsInCommon
import           TD.Query.GetImportedContactCount
import           TD.Query.GetInactiveSupergroupChats
import           TD.Query.GetInlineGameHighScores
import           TD.Query.GetInlineQueryResults
import           TD.Query.GetInstalledStickerSets
import           TD.Query.GetInternalLinkType
import           TD.Query.GetJsonString
import           TD.Query.GetJsonValue
import           TD.Query.GetLanguagePackInfo
import           TD.Query.GetLanguagePackString
import           TD.Query.GetLanguagePackStrings
import           TD.Query.GetLocalizationTargetInfo
import           TD.Query.GetLogStream
import           TD.Query.GetLogTagVerbosityLevel
import           TD.Query.GetLogTags
import           TD.Query.GetLogVerbosityLevel
import           TD.Query.GetLoginUrl
import           TD.Query.GetLoginUrlInfo
import           TD.Query.GetMapThumbnailFile
import           TD.Query.GetMarkdownText
import           TD.Query.GetMe
import           TD.Query.GetMenuButton
import           TD.Query.GetMessage
import           TD.Query.GetMessageAddedReactions
import           TD.Query.GetMessageAvailableReactions
import           TD.Query.GetMessageEmbeddingCode
import           TD.Query.GetMessageFileType
import           TD.Query.GetMessageImportConfirmationText
import           TD.Query.GetMessageLink
import           TD.Query.GetMessageLinkInfo
import           TD.Query.GetMessageLocally
import           TD.Query.GetMessagePublicForwards
import           TD.Query.GetMessageStatistics
import           TD.Query.GetMessageThread
import           TD.Query.GetMessageThreadHistory
import           TD.Query.GetMessageViewers
import           TD.Query.GetMessages
import           TD.Query.GetNetworkStatistics
import           TD.Query.GetOption
import           TD.Query.GetPassportAuthorizationForm
import           TD.Query.GetPassportAuthorizationFormAvailableElements
import           TD.Query.GetPassportElement
import           TD.Query.GetPasswordState
import           TD.Query.GetPaymentForm
import           TD.Query.GetPaymentReceipt
import           TD.Query.GetPhoneNumberInfo
import           TD.Query.GetPhoneNumberInfoSync
import           TD.Query.GetPollVoters
import           TD.Query.GetPreferredCountryLanguage
import           TD.Query.GetProxies
import           TD.Query.GetProxyLink
import           TD.Query.GetPushReceiverId
import           TD.Query.GetRecentInlineBots
import           TD.Query.GetRecentStickers
import           TD.Query.GetRecentlyOpenedChats
import           TD.Query.GetRecentlyVisitedTMeUrls
import           TD.Query.GetRecommendedChatFilters
import           TD.Query.GetRecoveryEmailAddress
import           TD.Query.GetRemoteFile
import           TD.Query.GetRepliedMessage
import           TD.Query.GetSavedAnimations
import           TD.Query.GetSavedNotificationSound
import           TD.Query.GetSavedNotificationSounds
import           TD.Query.GetSavedOrderInfo
import           TD.Query.GetScopeNotificationSettings
import           TD.Query.GetSecretChat
import           TD.Query.GetStatisticalGraph
import           TD.Query.GetStickerEmojis
import           TD.Query.GetStickerSet
import           TD.Query.GetStickers
import           TD.Query.GetStorageStatistics
import           TD.Query.GetStorageStatisticsFast
import           TD.Query.GetSuggestedFileName
import           TD.Query.GetSuggestedStickerSetName
import           TD.Query.GetSuitableDiscussionChats
import           TD.Query.GetSupergroup
import           TD.Query.GetSupergroupFullInfo
import           TD.Query.GetSupergroupMembers
import           TD.Query.GetSupportUser
import           TD.Query.GetTemporaryPasswordState
import           TD.Query.GetTextEntities
import           TD.Query.GetThemeParametersJsonString
import           TD.Query.GetTopChats
import           TD.Query.GetTrendingStickerSets
import           TD.Query.GetUser
import           TD.Query.GetUserFullInfo
import           TD.Query.GetUserPrivacySettingRules
import           TD.Query.GetUserProfilePhotos
import           TD.Query.GetVideoChatAvailableParticipants
import           TD.Query.GetVideoChatRtmpUrl
import           TD.Query.GetWebAppUrl
import           TD.Query.GetWebPageInstantView
import           TD.Query.GetWebPagePreview
import           TD.Query.HideSuggestedAction
import           TD.Query.ImportContacts
import           TD.Query.ImportMessages
import           TD.Query.InviteGroupCallParticipants
import           TD.Query.JoinChat
import           TD.Query.JoinChatByInviteLink
import           TD.Query.JoinGroupCall
import           TD.Query.LeaveChat
import           TD.Query.LeaveGroupCall
import           TD.Query.LoadChats
import           TD.Query.LoadGroupCallParticipants
import           TD.Query.LogOut
import           TD.Query.OpenChat
import           TD.Query.OpenMessageContent
import           TD.Query.OpenWebApp
import           TD.Query.OptimizeStorage
import           TD.Query.ParseMarkdown
import           TD.Query.ParseTextEntities
import           TD.Query.PinChatMessage
import           TD.Query.PingProxy
import           TD.Query.ProcessChatJoinRequest
import           TD.Query.ProcessChatJoinRequests
import           TD.Query.ProcessPushNotification
import           TD.Query.ReadAllChatMentions
import           TD.Query.ReadAllChatReactions
import           TD.Query.ReadFilePart
import           TD.Query.RecoverAuthenticationPassword
import           TD.Query.RecoverPassword
import           TD.Query.RegisterDevice
import           TD.Query.RegisterUser
import           TD.Query.RemoveAllFilesFromDownloads
import           TD.Query.RemoveBackground
import           TD.Query.RemoveChatActionBar
import           TD.Query.RemoveContacts
import           TD.Query.RemoveFavoriteSticker
import           TD.Query.RemoveFileFromDownloads
import           TD.Query.RemoveNotification
import           TD.Query.RemoveNotificationGroup
import           TD.Query.RemoveProxy
import           TD.Query.RemoveRecentHashtag
import           TD.Query.RemoveRecentSticker
import           TD.Query.RemoveRecentlyFoundChat
import           TD.Query.RemoveSavedAnimation
import           TD.Query.RemoveSavedNotificationSound
import           TD.Query.RemoveStickerFromSet
import           TD.Query.RemoveTopChat
import           TD.Query.ReorderChatFilters
import           TD.Query.ReorderInstalledStickerSets
import           TD.Query.ReplacePrimaryChatInviteLink
import           TD.Query.ReplaceVideoChatRtmpUrl
import           TD.Query.ReportChat
import           TD.Query.ReportChatPhoto
import           TD.Query.ReportSupergroupSpam
import           TD.Query.RequestAuthenticationPasswordRecovery
import           TD.Query.RequestPasswordRecovery
import           TD.Query.RequestQrCodeAuthentication
import           TD.Query.ResendAuthenticationCode
import           TD.Query.ResendChangePhoneNumberCode
import           TD.Query.ResendEmailAddressVerificationCode
import           TD.Query.ResendMessages
import           TD.Query.ResendPhoneNumberConfirmationCode
import           TD.Query.ResendPhoneNumberVerificationCode
import           TD.Query.ResendRecoveryEmailAddressCode
import           TD.Query.ResetAllNotificationSettings
import           TD.Query.ResetBackgrounds
import           TD.Query.ResetNetworkStatistics
import           TD.Query.ResetPassword
import           TD.Query.RevokeChatInviteLink
import           TD.Query.RevokeGroupCallInviteLink
import           TD.Query.SaveApplicationLogEvent
import           TD.Query.SearchBackground
import           TD.Query.SearchCallMessages
import           TD.Query.SearchChatMembers
import           TD.Query.SearchChatMessages
import           TD.Query.SearchChatRecentLocationMessages
import           TD.Query.SearchChats
import           TD.Query.SearchChatsNearby
import           TD.Query.SearchChatsOnServer
import           TD.Query.SearchContacts
import           TD.Query.SearchEmojis
import           TD.Query.SearchFileDownloads
import           TD.Query.SearchHashtags
import           TD.Query.SearchInstalledStickerSets
import           TD.Query.SearchMessages
import           TD.Query.SearchOutgoingDocumentMessages
import           TD.Query.SearchPublicChat
import           TD.Query.SearchPublicChats
import           TD.Query.SearchSecretMessages
import           TD.Query.SearchStickerSet
import           TD.Query.SearchStickerSets
import           TD.Query.SearchStickers
import           TD.Query.SearchUserByPhoneNumber
import           TD.Query.SendBotStartMessage
import           TD.Query.SendCallDebugInformation
import           TD.Query.SendCallLog
import           TD.Query.SendCallRating
import           TD.Query.SendCallSignalingData
import           TD.Query.SendChatAction
import           TD.Query.SendChatScreenshotTakenNotification
import           TD.Query.SendCustomRequest
import           TD.Query.SendEmailAddressVerificationCode
import           TD.Query.SendInlineQueryResultMessage
import           TD.Query.SendMessage
import           TD.Query.SendMessageAlbum
import           TD.Query.SendPassportAuthorizationForm
import           TD.Query.SendPaymentForm
import           TD.Query.SendPhoneNumberConfirmationCode
import           TD.Query.SendPhoneNumberVerificationCode
import           TD.Query.SendWebAppData
import           TD.Query.SetAccountTtl
import           TD.Query.SetAlarm
import           TD.Query.SetAuthenticationPhoneNumber
import           TD.Query.SetAutoDownloadSettings
import           TD.Query.SetBackground
import           TD.Query.SetBio
import           TD.Query.SetBotUpdatesStatus
import           TD.Query.SetChatAvailableReactions
import           TD.Query.SetChatClientData
import           TD.Query.SetChatDescription
import           TD.Query.SetChatDiscussionGroup
import           TD.Query.SetChatDraftMessage
import           TD.Query.SetChatLocation
import           TD.Query.SetChatMemberStatus
import           TD.Query.SetChatMessageSender
import           TD.Query.SetChatMessageTtl
import           TD.Query.SetChatNotificationSettings
import           TD.Query.SetChatPermissions
import           TD.Query.SetChatPhoto
import           TD.Query.SetChatSlowModeDelay
import           TD.Query.SetChatTheme
import           TD.Query.SetChatTitle
import           TD.Query.SetCommands
import           TD.Query.SetCustomLanguagePack
import           TD.Query.SetCustomLanguagePackString
import           TD.Query.SetDatabaseEncryptionKey
import           TD.Query.SetDefaultChannelAdministratorRights
import           TD.Query.SetDefaultGroupAdministratorRights
import           TD.Query.SetFileGenerationProgress
import           TD.Query.SetGameScore
import           TD.Query.SetGroupCallParticipantIsSpeaking
import           TD.Query.SetGroupCallParticipantVolumeLevel
import           TD.Query.SetGroupCallTitle
import           TD.Query.SetInactiveSessionTtl
import           TD.Query.SetInlineGameScore
import           TD.Query.SetLocation
import           TD.Query.SetLogStream
import           TD.Query.SetLogTagVerbosityLevel
import           TD.Query.SetLogVerbosityLevel
import           TD.Query.SetMenuButton
import           TD.Query.SetMessageReaction
import           TD.Query.SetName
import           TD.Query.SetNetworkType
import           TD.Query.SetOption
import           TD.Query.SetPassportElement
import           TD.Query.SetPassportElementErrors
import           TD.Query.SetPassword
import           TD.Query.SetPinnedChats
import           TD.Query.SetPollAnswer
import           TD.Query.SetProfilePhoto
import           TD.Query.SetRecoveryEmailAddress
import           TD.Query.SetScopeNotificationSettings
import           TD.Query.SetStickerPositionInSet
import           TD.Query.SetStickerSetThumbnail
import           TD.Query.SetSupergroupStickerSet
import           TD.Query.SetSupergroupUsername
import           TD.Query.SetTdlibParameters
import           TD.Query.SetUserPrivacySettingRules
import           TD.Query.SetUsername
import           TD.Query.SetVideoChatDefaultParticipant
import           TD.Query.SharePhoneNumber
import           TD.Query.StartGroupCallRecording
import           TD.Query.StartGroupCallScreenSharing
import           TD.Query.StartScheduledGroupCall
import           TD.Query.StopPoll
import           TD.Query.SynchronizeLanguagePack
import           TD.Query.TerminateAllOtherSessions
import           TD.Query.TerminateSession
import           TD.Query.TestCallBytes
import           TD.Query.TestCallEmpty
import           TD.Query.TestCallString
import           TD.Query.TestCallVectorInt
import           TD.Query.TestCallVectorIntObject
import           TD.Query.TestCallVectorString
import           TD.Query.TestCallVectorStringObject
import           TD.Query.TestGetDifference
import           TD.Query.TestNetwork
import           TD.Query.TestProxy
import           TD.Query.TestReturnError
import           TD.Query.TestSquareInt
import           TD.Query.TestUseUpdate
import           TD.Query.ToggleAllDownloadsArePaused
import           TD.Query.ToggleBotIsAddedToAttachmentMenu
import           TD.Query.ToggleChatDefaultDisableNotification
import           TD.Query.ToggleChatHasProtectedContent
import           TD.Query.ToggleChatIsMarkedAsUnread
import           TD.Query.ToggleChatIsPinned
import           TD.Query.ToggleDownloadIsPaused
import           TD.Query.ToggleGroupCallEnabledStartNotification
import           TD.Query.ToggleGroupCallIsMyVideoEnabled
import           TD.Query.ToggleGroupCallIsMyVideoPaused
import           TD.Query.ToggleGroupCallMuteNewParticipants
import           TD.Query.ToggleGroupCallParticipantIsHandRaised
import           TD.Query.ToggleGroupCallParticipantIsMuted
import           TD.Query.ToggleGroupCallScreenSharingIsPaused
import           TD.Query.ToggleMessageSenderIsBlocked
import           TD.Query.ToggleSessionCanAcceptCalls
import           TD.Query.ToggleSessionCanAcceptSecretChats
import           TD.Query.ToggleSupergroupIsAllHistoryAvailable
import           TD.Query.ToggleSupergroupIsBroadcastGroup
import           TD.Query.ToggleSupergroupSignMessages
import           TD.Query.TransferChatOwnership
import           TD.Query.TranslateText
import           TD.Query.UnpinAllChatMessages
import           TD.Query.UnpinChatMessage
import           TD.Query.UpgradeBasicGroupChatToSupergroupChat
import           TD.Query.UploadFile
import           TD.Query.UploadStickerFile
import           TD.Query.ValidateOrderInfo
import           TD.Query.ViewMessages
import           TD.Query.ViewTrendingStickerSets
import           TD.Query.WriteGeneratedFilePart


------------------------------------------------------------------------------------------

concat <$> mapM goA
 [ ''AddFileToDownloads
 , ''DownloadFile
 , ''SearchFileDownloads
 , ''SendMessage
 , ''SetCommands
 , ''EditMessageText
 , ''CancelDownloadFile
 , ''EditInlineMessageText
 ]


-- concat <$> mapM goA
--   [ ''AcceptCall
--   , ''AcceptTermsOfService
--   , ''AddChatMember
--   , ''AddChatMembers
--   , ''AddChatToList
--   , ''AddContact
--   , ''AddCustomServerLanguagePack
--   , ''AddFavoriteSticker
--   , ''AddFileToDownloads
--   , ''AddLocalMessage
--   , ''AddLogMessage
--   , ''AddNetworkStatistics
--   , ''AddProxy
--   , ''AddRecentSticker
--   , ''AddRecentlyFoundChat
--   , ''AddSavedAnimation
--   , ''AddSavedNotificationSound
--   , ''AddStickerToSet
--   , ''AnswerCallbackQuery
--   , ''AnswerCustomQuery
--   , ''AnswerInlineQuery
--   , ''AnswerPreCheckoutQuery
--   , ''AnswerShippingQuery
--   , ''AnswerWebAppQuery
--   , ''BanChatMember
--   , ''BlockMessageSenderFromReplies
--   , ''CanTransferOwnership
--   , ''CancelDownloadFile
--   , ''CancelPasswordReset
--   , ''CancelUploadFile
--   , ''ChangeImportedContacts
--   , ''ChangePhoneNumber
--   , ''ChangeStickerSet
--   , ''CheckAuthenticationBotToken
--   , ''CheckAuthenticationCode
--   , ''CheckAuthenticationPassword
--   , ''CheckAuthenticationPasswordRecoveryCode
--   , ''CheckChangePhoneNumberCode
--   , ''CheckChatInviteLink
--   , ''CheckChatUsername
--   , ''CheckCreatedPublicChatsLimit
--   , ''CheckDatabaseEncryptionKey
--   , ''CheckEmailAddressVerificationCode
--   , ''CheckPasswordRecoveryCode
--   , ''CheckPhoneNumberConfirmationCode
--   , ''CheckPhoneNumberVerificationCode
--   , ''CheckRecoveryEmailAddressCode
--   , ''CheckStickerSetName
--   , ''CleanFileName
--   , ''ClearAllDraftMessages
--   , ''ClearImportedContacts
--   , ''ClearRecentStickers
--   , ''ClearRecentlyFoundChats
--   , ''ClickAnimatedEmojiMessage
--   , ''Close
--   , ''CloseChat
--   , ''CloseWebApp
--   , ''ConfirmQrCodeAuthentication
--   , ''CreateBasicGroupChat
--   , ''CreateCall
--   , ''CreateChatFilter
--   , ''CreateChatInviteLink
--   , ''CreateNewBasicGroupChat
--   , ''CreateNewSecretChat
--   , ''CreateNewStickerSet
--   , ''CreateNewSupergroupChat
--   , ''CreatePrivateChat
--   , ''CreateTemporaryPassword
--   , ''CreateVideoChat
--   , ''DeleteAccount
--   , ''DeleteAllCallMessages
--   , ''DeleteAllRevokedChatInviteLinks
--   , ''DeleteChat
--   , ''DeleteChatFilter
--   , ''DeleteChatHistory
--   , ''DeleteChatMessagesByDate
--   , ''DeleteChatMessagesBySender
--   , ''DeleteChatReplyMarkup
--   , ''DeleteCommands
--   , ''DeleteLanguagePack
--   , ''DeleteMessages
--   , ''DeletePassportElement
--   , ''DeleteProfilePhoto
--   , ''DeleteRevokedChatInviteLink
--   , ''DeleteSavedCredentials
--   , ''DeleteSavedOrderInfo
--   , ''Destroy
--   , ''DisableProxy
--   , ''DiscardCall
--   , ''DisconnectAllWebsites
--   , ''DisconnectWebsite
--   , ''DownloadFile
--   , ''EditChatFilter
--   , ''EditChatInviteLink
--   , ''EditCustomLanguagePackInfo
--   , ''EditInlineMessageCaption
--   , ''EditInlineMessageLiveLocation
--   , ''EditInlineMessageMedia
--   , ''EditInlineMessageReplyMarkup
--   , ''EditInlineMessageText
--   , ''EditMessageCaption
--   , ''EditMessageLiveLocation
--   , ''EditMessageMedia
--   , ''EditMessageReplyMarkup
--   , ''EditMessageSchedulingState
--   , ''EditMessageText
--   , ''EndGroupCall
--   , ''EndGroupCallRecording
--   , ''EndGroupCallScreenSharing
--   , ''FinishFileGeneration
--   , ''ForwardMessages
--   , ''GetAccountTtl
--   , ''GetActiveLiveLocationMessages
--   , ''GetActiveSessions
--   , ''GetAllPassportElements
--   , ''GetAnimatedEmoji
--   , ''GetApplicationConfig
--   , ''GetApplicationDownloadLink
--   , ''GetArchivedStickerSets
--   , ''GetAttachedStickerSets
--   , ''GetAttachmentMenuBot
--   , ''GetAuthorizationState
--   , ''GetAutoDownloadSettingsPresets
--   , ''GetBackgroundUrl
--   , ''GetBackgrounds
--   , ''GetBankCardInfo
--   , ''GetBasicGroup
--   , ''GetBasicGroupFullInfo
--   , ''GetBlockedMessageSenders
--   , ''GetCallbackQueryAnswer
--   , ''GetCallbackQueryMessage
--   , ''GetChat
--   , ''GetChatAdministrators
--   , ''GetChatAvailableMessageSenders
--   , ''GetChatEventLog
--   , ''GetChatFilter
--   , ''GetChatFilterDefaultIconName
--   , ''GetChatHistory
--   , ''GetChatInviteLink
--   , ''GetChatInviteLinkCounts
--   , ''GetChatInviteLinkMembers
--   , ''GetChatInviteLinks
--   , ''GetChatJoinRequests
--   , ''GetChatListsToAddChat
--   , ''GetChatMember
--   , ''GetChatMessageByDate
--   , ''GetChatMessageCalendar
--   , ''GetChatMessageCount
--   , ''GetChatNotificationSettingsExceptions
--   , ''GetChatPinnedMessage
--   , ''GetChatScheduledMessages
--   , ''GetChatSparseMessagePositions
--   , ''GetChatSponsoredMessage
--   , ''GetChatStatistics
--   , ''GetChats
--   , ''GetCommands
--   , ''GetConnectedWebsites
--   , ''GetContacts
--   , ''GetCountries
--   , ''GetCountryCode
--   , ''GetCreatedPublicChats
--   , ''GetCurrentState
--   , ''GetDatabaseStatistics
--   , ''GetDeepLinkInfo
--   , ''GetEmojiSuggestionsUrl
--   , ''GetExternalLink
--   , ''GetExternalLinkInfo
--   , ''GetFavoriteStickers
--   , ''GetFile
--   , ''GetFileDownloadedPrefixSize
--   , ''GetFileExtension
--   , ''GetFileMimeType
--   , ''GetGameHighScores
--   , ''GetGroupCall
--   , ''GetGroupCallInviteLink
--   , ''GetGroupCallStreamSegment
--   , ''GetGroupCallStreams
--   , ''GetGroupsInCommon
--   , ''GetImportedContactCount
--   , ''GetInactiveSupergroupChats
--   , ''GetInlineGameHighScores
--   , ''GetInlineQueryResults
--   , ''GetInstalledStickerSets
--   , ''GetInternalLinkType
--   , ''GetJsonString
--   , ''GetJsonValue
--   , ''GetLanguagePackInfo
--   , ''GetLanguagePackString
--   , ''GetLanguagePackStrings
--   , ''GetLocalizationTargetInfo
--   , ''GetLogStream
--   , ''GetLogTagVerbosityLevel
--   , ''GetLogTags
--   , ''GetLogVerbosityLevel
--   , ''GetLoginUrl
--   , ''GetLoginUrlInfo
--   , ''GetMapThumbnailFile
--   , ''GetMarkdownText
--   , ''GetMe
--   , ''GetMenuButton
--   , ''GetMessage
--   , ''GetMessageEmbeddingCode
--   , ''GetMessageFileType
--   , ''GetMessageImportConfirmationText
--   , ''GetMessageLink
--   , ''GetMessageLinkInfo
--   , ''GetMessageLocally
--   , ''GetMessagePublicForwards
--   , ''GetMessageStatistics
--   , ''GetMessageThread
--   , ''GetMessageThreadHistory
--   , ''GetMessageViewers
--   , ''GetMessages
--   , ''GetNetworkStatistics
--   , ''GetOption
--   , ''GetPassportAuthorizationForm
--   , ''GetPassportAuthorizationFormAvailableElements
--   , ''GetPassportElement
--   , ''GetPasswordState
--   , ''GetPaymentForm
--   , ''GetPaymentReceipt
--   , ''GetPhoneNumberInfo
--   , ''GetPhoneNumberInfoSync
--   , ''GetPollVoters
--   , ''GetPreferredCountryLanguage
--   , ''GetProxies
--   , ''GetProxyLink
--   , ''GetPushReceiverId
--   , ''GetRecentInlineBots
--   , ''GetRecentStickers
--   , ''GetRecentlyOpenedChats
--   , ''GetRecentlyVisitedTMeUrls
--   , ''GetRecommendedChatFilters
--   , ''GetRecoveryEmailAddress
--   , ''GetRemoteFile
--   , ''GetRepliedMessage
--   , ''GetSavedAnimations
--   , ''GetSavedNotificationSound
--   , ''GetSavedNotificationSounds
--   , ''GetSavedOrderInfo
--   , ''GetScopeNotificationSettings
--   , ''GetSecretChat
--   , ''GetStatisticalGraph
--   , ''GetStickerEmojis
--   , ''GetStickerSet
--   , ''GetStickers
--   , ''GetStorageStatistics
--   , ''GetStorageStatisticsFast
--   , ''GetSuggestedFileName
--   , ''GetSuggestedStickerSetName
--   , ''GetSuitableDiscussionChats
--   , ''GetSupergroup
--   , ''GetSupergroupFullInfo
--   , ''GetSupergroupMembers
--   , ''GetSupportUser
--   , ''GetTemporaryPasswordState
--   , ''GetTextEntities
--   , ''GetThemeParametersJsonString
--   , ''GetTopChats
--   , ''GetTrendingStickerSets
--   , ''GetUser
--   , ''GetUserFullInfo
--   , ''GetUserPrivacySettingRules
--   , ''GetUserProfilePhotos
--   , ''GetVideoChatAvailableParticipants
--   , ''GetVideoChatRtmpUrl
--   , ''GetWebAppUrl
--   , ''GetWebPageInstantView
--   , ''GetWebPagePreview
--   , ''HideSuggestedAction
--   , ''ImportContacts
--   , ''ImportMessages
--   , ''InviteGroupCallParticipants
--   , ''JoinChat
--   , ''JoinChatByInviteLink
--   , ''JoinGroupCall
--   , ''LeaveChat
--   , ''LeaveGroupCall
--   , ''LoadChats
--   , ''LoadGroupCallParticipants
--   , ''LogOut
--   , ''OpenChat
--   , ''OpenMessageContent
--   , ''OpenWebApp
--   , ''OptimizeStorage
--   , ''ParseMarkdown
--   , ''ParseTextEntities
--   , ''PinChatMessage
--   , ''PingProxy
--   , ''ProcessChatJoinRequest
--   , ''ProcessChatJoinRequests
--   , ''ProcessPushNotification
--   , ''ReadAllChatMentions
--   , ''ReadAllChatReactions
--   , ''ReadFilePart
--   , ''RecoverAuthenticationPassword
--   , ''RecoverPassword
--   , ''RegisterDevice
--   , ''RegisterUser
--   , ''RemoveAllFilesFromDownloads
--   , ''RemoveBackground
--   , ''RemoveChatActionBar
--   , ''RemoveContacts
--   , ''RemoveFavoriteSticker
--   , ''RemoveFileFromDownloads
--   , ''RemoveNotification
--   , ''RemoveNotificationGroup
--   , ''RemoveProxy
--   , ''RemoveRecentHashtag
--   , ''RemoveRecentSticker
--   , ''RemoveRecentlyFoundChat
--   , ''RemoveSavedAnimation
--   , ''RemoveSavedNotificationSound
--   , ''RemoveStickerFromSet
--   , ''RemoveTopChat
--   , ''ReorderChatFilters
--   , ''ReorderInstalledStickerSets
--   , ''ReplacePrimaryChatInviteLink
--   , ''ReplaceVideoChatRtmpUrl
--   , ''ReportChat
--   , ''ReportChatPhoto
--   , ''ReportSupergroupSpam
--   , ''RequestAuthenticationPasswordRecovery
--   , ''RequestPasswordRecovery
--   , ''RequestQrCodeAuthentication
--   , ''ResendAuthenticationCode
--   , ''ResendChangePhoneNumberCode
--   , ''ResendEmailAddressVerificationCode
--   , ''ResendMessages
--   , ''ResendPhoneNumberConfirmationCode
--   , ''ResendPhoneNumberVerificationCode
--   , ''ResendRecoveryEmailAddressCode
--   , ''ResetAllNotificationSettings
--   , ''ResetBackgrounds
--   , ''ResetNetworkStatistics
--   , ''ResetPassword
--   , ''RevokeChatInviteLink
--   , ''RevokeGroupCallInviteLink
--   , ''SaveApplicationLogEvent
--   , ''SearchBackground
--   , ''SearchChatRecentLocationMessages
--   , ''SearchChatsNearby
--   , ''SearchChatsOnServer
--   , ''SearchContacts
--   , ''SearchEmojis
--   , ''SearchFileDownloads
--   , ''SearchHashtags
--   , ''SearchInstalledStickerSets
--   , ''SearchMessages
--   , ''SearchOutgoingDocumentMessages
--   , ''SearchPublicChat
--   , ''SearchPublicChats
--   , ''SearchSecretMessages
--   , ''SearchStickerSet
--   , ''SearchStickerSets
--   , ''SearchStickers
--   , ''SearchUserByPhoneNumber
--   , ''SendBotStartMessage
--   , ''SendCallDebugInformation
--   , ''SendCallLog
--   , ''SendCallRating
--   , ''SendCallSignalingData
--   , ''SendChatAction
--   , ''SendChatScreenshotTakenNotification
--   , ''SendCustomRequest
--   , ''SendEmailAddressVerificationCode
--   , ''SendInlineQueryResultMessage
--   , ''SendMessage
--   , ''SendMessageAlbum
--   , ''SendPassportAuthorizationForm
--   , ''SendPaymentForm
--   , ''SendPhoneNumberConfirmationCode
--   , ''SendPhoneNumberVerificationCode
--   , ''SendWebAppData
--   , ''SetAccountTtl
--   , ''SetAlarm
--   , ''SetAuthenticationPhoneNumber
--   , ''SetAutoDownloadSettings
--   , ''SetBackground
--   , ''SetBio
--   , ''SetBotUpdatesStatus
--   , ''SetChatAvailableReactions
--   , ''SetChatClientData
--   , ''SetChatDescription
--   , ''SetChatDiscussionGroup
--   , ''SetChatDraftMessage
--   , ''SetChatLocation
--   , ''SetChatMemberStatus
--   , ''SetChatMessageSender
--   , ''SetChatMessageTtl
--   , ''SetChatNotificationSettings
--   , ''SetChatPermissions
--   , ''SetChatPhoto
--   , ''SetChatSlowModeDelay
--   , ''SetCommands
--   , ''SetCustomLanguagePack
--   , ''SetCustomLanguagePackString
--   , ''SetDatabaseEncryptionKey
--   , ''SetDefaultChannelAdministratorRights
--   , ''SetDefaultGroupAdministratorRights
--   , ''SetFileGenerationProgress
--   , ''SetGameScore
--   , ''SetGroupCallParticipantIsSpeaking
--   , ''SetGroupCallParticipantVolumeLevel
--   , ''SetGroupCallTitle
--   , ''SetInactiveSessionTtl
--   , ''SetInlineGameScore
--   , ''SetLocation
--   , ''SetLogStream
--   , ''SetLogTagVerbosityLevel
--   , ''SetLogVerbosityLevel
--   , ''SetMenuButton
--   , ''SetMessageReaction
--   , ''SetName
--   , ''SetNetworkType
--   , ''SetOption
--   , ''SetPassportElement
--   , ''SetPassportElementErrors
--   , ''SetPassword
--   , ''SetPinnedChats
--   , ''SetPollAnswer
--   , ''SetProfilePhoto
--   , ''SetRecoveryEmailAddress
--   , ''SetScopeNotificationSettings
--   , ''SetStickerPositionInSet
--   , ''SetStickerSetThumbnail
--   , ''SetSupergroupStickerSet
--   , ''SetSupergroupUsername
--   , ''SetTdlibParameters
--   , ''SetUserPrivacySettingRules
--   , ''SetUsername
--   , ''SetVideoChatDefaultParticipant
--   , ''SharePhoneNumber
--   , ''StartGroupCallRecording
--   , ''StartGroupCallScreenSharing
--   , ''StartScheduledGroupCall
--   , ''StopPoll
--   , ''SynchronizeLanguagePack
--   , ''TerminateAllOtherSessions
--   , ''TerminateSession
--   , ''TestCallBytes
--   , ''TestCallEmpty
--   , ''TestCallString
--   , ''TestCallVectorInt
--   , ''TestCallVectorIntObject
--   , ''TestCallVectorString
--   , ''TestCallVectorStringObject
--   , ''TestGetDifference
--   , ''TestNetwork
--   , ''TestProxy
--   , ''TestReturnError
--   , ''TestSquareInt
--   , ''TestUseUpdate
--   , ''ToggleAllDownloadsArePaused
--   , ''ToggleBotIsAddedToAttachmentMenu
--   , ''ToggleChatDefaultDisableNotification
--   , ''ToggleChatHasProtectedContent
--   , ''ToggleChatIsMarkedAsUnread
--   , ''ToggleChatIsPinned
--   , ''ToggleDownloadIsPaused
--   , ''ToggleGroupCallEnabledStartNotification
--   , ''ToggleGroupCallIsMyVideoEnabled
--   , ''ToggleGroupCallIsMyVideoPaused
--   , ''ToggleGroupCallMuteNewParticipants
--   , ''ToggleGroupCallParticipantIsHandRaised
--   , ''ToggleGroupCallParticipantIsMuted
--   , ''ToggleGroupCallScreenSharingIsPaused
--   , ''ToggleMessageSenderIsBlocked
--   , ''ToggleSessionCanAcceptCalls
--   , ''ToggleSessionCanAcceptSecretChats
--   , ''ToggleSupergroupIsAllHistoryAvailable
--   , ''ToggleSupergroupIsBroadcastGroup
--   , ''ToggleSupergroupSignMessages
--   , ''TransferChatOwnership
--   , ''TranslateText
--   , ''UnpinAllChatMessages
--   , ''UnpinChatMessage
--   , ''UpgradeBasicGroupChatToSupergroupChat
--   , ''UploadFile
--   , ''UploadStickerFile
--   , ''ValidateOrderInfo
--   , ''ViewMessages
--   , ''ViewTrendingStickerSets
--   , ''WriteGeneratedFilePart
--   ]

------------------------------------------------------------------------------------------

-- concat <$> mapM goN
--   [ ''CloseSecretChat
--   , ''CreateSecretChat
--   , ''CreateSupergroupChat
--   , ''EditProxy
--   , ''EnableProxy
--   , ''GetMessageAddedReactions
--   , ''SearchChatMembers
--   , ''SearchChatMessages
--   , ''SearchChats
--   , ''DeleteFile
--   , ''SetChatTheme
--   , ''SetChatTitle
--   ]
