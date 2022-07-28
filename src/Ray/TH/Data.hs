{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Ray.TH.Data where


import           Language.Haskell.TH
import           Lens.Micro.Platform
import           Ray.TH.Utils

import           TD.Data.AccountTtl
import           TD.Data.AddedReaction
import           TD.Data.AddedReactions
import           TD.Data.Address
import           TD.Data.AnimatedChatPhoto
import           TD.Data.AnimatedEmoji
import           TD.Data.Animation
import           TD.Data.Animations
import           TD.Data.AttachmentMenuBot
import           TD.Data.AttachmentMenuBotColor
import           TD.Data.Audio
import           TD.Data.AuthenticationCodeInfo
import           TD.Data.AuthenticationCodeType
import           TD.Data.AuthorizationState
import           TD.Data.AutoDownloadSettings
import           TD.Data.AutoDownloadSettingsPresets
import           TD.Data.AvailableReactions
import           TD.Data.Background
import           TD.Data.BackgroundFill
import           TD.Data.BackgroundType
import           TD.Data.Backgrounds
import           TD.Data.BankCardActionOpenUrl
import           TD.Data.BankCardInfo
import           TD.Data.BasicGroup
import           TD.Data.BasicGroupFullInfo
import           TD.Data.BotCommand
import           TD.Data.BotCommandScope
import           TD.Data.BotCommands
import           TD.Data.BotInfo
import           TD.Data.BotMenuButton
import           TD.Data.Call
import           TD.Data.CallDiscardReason
import           TD.Data.CallId
import           TD.Data.CallProblem
import           TD.Data.CallProtocol
import           TD.Data.CallServer
import           TD.Data.CallServerType
import           TD.Data.CallState
import           TD.Data.CallbackQueryAnswer
import           TD.Data.CallbackQueryPayload
import           TD.Data.CanTransferOwnershipResult
import           TD.Data.Chat
import           TD.Data.ChatAction
import           TD.Data.ChatActionBar
import           TD.Data.ChatAdministrator
import           TD.Data.ChatAdministratorRights
import           TD.Data.ChatAdministrators
import           TD.Data.ChatEvent
import           TD.Data.ChatEventAction
import           TD.Data.ChatEventLogFilters
import           TD.Data.ChatEvents
import           TD.Data.ChatFilter
import           TD.Data.ChatFilterInfo
import           TD.Data.ChatInviteLink
import           TD.Data.ChatInviteLinkCount
import           TD.Data.ChatInviteLinkCounts
import           TD.Data.ChatInviteLinkInfo
import           TD.Data.ChatInviteLinkMember
import           TD.Data.ChatInviteLinkMembers
import           TD.Data.ChatInviteLinks
import           TD.Data.ChatJoinRequest
import           TD.Data.ChatJoinRequests
import           TD.Data.ChatJoinRequestsInfo
import           TD.Data.ChatList
import           TD.Data.ChatLists
import           TD.Data.ChatLocation
import           TD.Data.ChatMember
import           TD.Data.ChatMemberStatus
import           TD.Data.ChatMembers
import           TD.Data.ChatMembersFilter
import           TD.Data.ChatNearby
import           TD.Data.ChatNotificationSettings
import           TD.Data.ChatPermissions
import           TD.Data.ChatPhoto
import           TD.Data.ChatPhotoInfo
import           TD.Data.ChatPhotos
import           TD.Data.ChatPosition
import           TD.Data.ChatReportReason
import           TD.Data.ChatSource
import           TD.Data.ChatStatistics
import           TD.Data.ChatStatisticsAdministratorActionsInfo
import           TD.Data.ChatStatisticsInviterInfo
import           TD.Data.ChatStatisticsMessageInteractionInfo
import           TD.Data.ChatStatisticsMessageSenderInfo
import           TD.Data.ChatTheme
import           TD.Data.ChatType
import           TD.Data.Chats
import           TD.Data.ChatsNearby
import           TD.Data.CheckChatUsernameResult
import           TD.Data.CheckStickerSetNameResult
import           TD.Data.ClosedVectorPath
import           TD.Data.ConnectedWebsite
import           TD.Data.ConnectedWebsites
import           TD.Data.ConnectionState
import           TD.Data.Contact
import           TD.Data.Count
import           TD.Data.Countries
import           TD.Data.CountryInfo
import           TD.Data.CustomRequestResult
import           TD.Data.DatabaseStatistics
import           TD.Data.Date
import           TD.Data.DateRange
import           TD.Data.DatedFile
import           TD.Data.DeepLinkInfo
import           TD.Data.DeviceToken
import           TD.Data.DiceStickers
import           TD.Data.Document
import           TD.Data.DownloadedFileCounts
import           TD.Data.DraftMessage
import           TD.Data.EmailAddressAuthenticationCodeInfo
import           TD.Data.Emojis
import           TD.Data.EncryptedCredentials
import           TD.Data.EncryptedPassportElement
import           TD.Data.Error
import           TD.Data.File
import           TD.Data.FileDownload
import           TD.Data.FilePart
import           TD.Data.FileType
import           TD.Data.FormattedText
import           TD.Data.FoundFileDownloads
import           TD.Data.FoundMessages
import           TD.Data.Game
import           TD.Data.GameHighScore
import           TD.Data.GameHighScores
import qualified TD.Data.GeneralResult                          as GR
import           TD.Data.GroupCall
import           TD.Data.GroupCallId
import           TD.Data.GroupCallParticipant
import           TD.Data.GroupCallParticipantVideoInfo
import           TD.Data.GroupCallRecentSpeaker
import           TD.Data.GroupCallStream
import           TD.Data.GroupCallStreams
import           TD.Data.GroupCallVideoQuality
import           TD.Data.GroupCallVideoSourceGroup
import           TD.Data.Hashtags
import           TD.Data.HttpUrl
import           TD.Data.IdentityDocument
import           TD.Data.ImportedContacts
import           TD.Data.InlineKeyboardButton
import           TD.Data.InlineKeyboardButtonType
import           TD.Data.InlineQueryResult
import           TD.Data.InlineQueryResults
import           TD.Data.InputBackground
import           TD.Data.InputChatPhoto
import           TD.Data.InputCredentials
import           TD.Data.InputFile
import           TD.Data.InputIdentityDocument
import           TD.Data.InputInlineQueryResult
import           TD.Data.InputMessageContent
import           TD.Data.InputPassportElement
import           TD.Data.InputPassportElementError
import           TD.Data.InputPassportElementErrorSource
import           TD.Data.InputPersonalDocument
import           TD.Data.InputSticker
import           TD.Data.InputThumbnail
import           TD.Data.InternalLinkType
import           TD.Data.Invoice
import           TD.Data.JsonObjectMember
import           TD.Data.JsonValue
import           TD.Data.KeyboardButton
import           TD.Data.KeyboardButtonType
import           TD.Data.LabeledPricePart
import           TD.Data.LanguagePackInfo
import           TD.Data.LanguagePackString
import           TD.Data.LanguagePackStringValue
import           TD.Data.LanguagePackStrings
import           TD.Data.LocalFile
import           TD.Data.LocalizationTargetInfo
import           TD.Data.Location
import           TD.Data.LogStream
import           TD.Data.LogTags
import           TD.Data.LogVerbosityLevel
import           TD.Data.LoginUrlInfo
import           TD.Data.MaskPoint
import           TD.Data.MaskPosition
import           TD.Data.Message
import           TD.Data.MessageCalendar
import           TD.Data.MessageCalendarDay
import           TD.Data.MessageContent
import           TD.Data.MessageCopyOptions
import           TD.Data.MessageFileType
import           TD.Data.MessageForwardInfo
import           TD.Data.MessageForwardOrigin
import           TD.Data.MessageInteractionInfo
import           TD.Data.MessageLink
import           TD.Data.MessageLinkInfo
import           TD.Data.MessagePosition
import           TD.Data.MessagePositions
import           TD.Data.MessageReaction
import           TD.Data.MessageReplyInfo
import           TD.Data.MessageSchedulingState
import           TD.Data.MessageSendOptions
import           TD.Data.MessageSender
import           TD.Data.MessageSenders
import           TD.Data.MessageSendingState
import           TD.Data.MessageStatistics
import           TD.Data.MessageThreadInfo
import           TD.Data.Messages
import           TD.Data.Minithumbnail
import           TD.Data.NetworkStatistics
import           TD.Data.NetworkStatisticsEntry
import           TD.Data.NetworkType
import           TD.Data.Notification
import           TD.Data.NotificationGroup
import           TD.Data.NotificationGroupType
import           TD.Data.NotificationSettingsScope
import           TD.Data.NotificationSound
import           TD.Data.NotificationSounds
import           TD.Data.NotificationType
import           TD.Data.Ok
import           TD.Data.OptionValue
import           TD.Data.OrderInfo
import           TD.Data.PageBlock
import           TD.Data.PageBlockCaption
import           TD.Data.PageBlockHorizontalAlignment
import           TD.Data.PageBlockListItem
import           TD.Data.PageBlockRelatedArticle
import           TD.Data.PageBlockTableCell
import           TD.Data.PageBlockVerticalAlignment
import           TD.Data.PassportAuthorizationForm
import           TD.Data.PassportElement
import           TD.Data.PassportElementError
import           TD.Data.PassportElementErrorSource
import           TD.Data.PassportElementType
import           TD.Data.PassportElements
import           TD.Data.PassportElementsWithErrors
import           TD.Data.PassportRequiredElement
import           TD.Data.PassportSuitableElement
import           TD.Data.PasswordState
import           TD.Data.PaymentForm
import           TD.Data.PaymentReceipt
import           TD.Data.PaymentResult
import           TD.Data.PaymentsProviderStripe
import           TD.Data.PersonalDetails
import           TD.Data.PersonalDocument
import           TD.Data.PhoneNumberAuthenticationSettings
import           TD.Data.PhoneNumberInfo
import           TD.Data.Photo
import           TD.Data.PhotoSize
import           TD.Data.Point
import           TD.Data.Poll
import           TD.Data.PollOption
import           TD.Data.PollType
import           TD.Data.ProfilePhoto
import           TD.Data.Proxies
import           TD.Data.Proxy
import           TD.Data.ProxyType
import           TD.Data.PublicChatType
import           TD.Data.PushMessageContent
import           TD.Data.PushReceiverId
import           TD.Data.Reaction
import           TD.Data.RecommendedChatFilter
import           TD.Data.RecommendedChatFilters
import           TD.Data.RecoveryEmailAddress
import           TD.Data.RemoteFile
import           TD.Data.ReplyMarkup
import           TD.Data.ResetPasswordResult
import           TD.Data.RichText
import           TD.Data.RtmpUrl
import           TD.Data.SavedCredentials
import           TD.Data.ScopeNotificationSettings
import           TD.Data.SearchMessagesFilter
import           TD.Data.Seconds
import           TD.Data.SecretChat
import           TD.Data.SecretChatState
import           TD.Data.SentWebAppMessage
import           TD.Data.Session
import           TD.Data.SessionType
import           TD.Data.Sessions
import           TD.Data.ShippingOption
import           TD.Data.SponsoredMessage
import           TD.Data.StatisticalGraph
import           TD.Data.StatisticalValue
import           TD.Data.Sticker
import           TD.Data.StickerSet
import           TD.Data.StickerSetInfo
import           TD.Data.StickerSets
import           TD.Data.StickerType
import           TD.Data.Stickers
import           TD.Data.StorageStatistics
import           TD.Data.StorageStatisticsByChat
import           TD.Data.StorageStatisticsByFileType
import           TD.Data.StorageStatisticsFast
import           TD.Data.SuggestedAction
import           TD.Data.Supergroup
import           TD.Data.SupergroupFullInfo
import           TD.Data.SupergroupMembersFilter
import           TD.Data.TMeUrl
import           TD.Data.TMeUrlType
import           TD.Data.TMeUrls
import           TD.Data.TdlibParameters
import           TD.Data.TemporaryPasswordState
import           TD.Data.TermsOfService
import           TD.Data.TestBytes
import           TD.Data.TestInt
import           TD.Data.TestString
import           TD.Data.TestVectorInt
import           TD.Data.TestVectorIntObject
import           TD.Data.TestVectorString
import           TD.Data.TestVectorStringObject
import           TD.Data.Text
import           TD.Data.TextEntities
import           TD.Data.TextEntity
import           TD.Data.TextEntityType
import           TD.Data.TextParseMode
import           TD.Data.ThemeParameters
import           TD.Data.ThemeSettings
import           TD.Data.Thumbnail
import           TD.Data.ThumbnailFormat
import           TD.Data.TopChatCategory
import           TD.Data.UnreadReaction
import           TD.Data.Update
import           TD.Data.Updates
import           TD.Data.User
import           TD.Data.UserFullInfo
import           TD.Data.UserPrivacySetting
import           TD.Data.UserPrivacySettingRule
import           TD.Data.UserPrivacySettingRules
import           TD.Data.UserStatus
import           TD.Data.UserType
import           TD.Data.Users
import           TD.Data.ValidatedOrderInfo
import           TD.Data.VectorPathCommand
import           TD.Data.Venue
import           TD.Data.Video
import           TD.Data.VideoChat
import           TD.Data.VideoNote
import           TD.Data.VoiceNote
import           TD.Data.WebAppInfo
import           TD.Data.WebPage
import           TD.Data.WebPageInstantView

------------------------------------------------------------------------------------------

concat <$> mapM goA
 [ ''BotCommand
 , ''FormattedText
 , ''InputFile
 , ''InputMessageContent
 , ''MessageSendOptions
 , ''File
 , ''LocalFile
 , ''Message
 , ''MessageContent
 , ''RemoteFile
 , ''Document
 , ''MessageSender
 , ''Video
 , ''ReplyMarkup
 , ''InlineKeyboardButton
 , ''InlineKeyboardButtonType
 , ''KeyboardButtonType
 , ''KeyboardButton
 ]

-- concat <$> mapM goA
--   [ ''AccountTtl
--   , ''AddedReaction
--   , ''Address
--   , ''AnimatedChatPhoto
--   , ''AnimatedEmoji
--   , ''Animation
--   , ''Animations
--   , ''AttachmentMenuBot
--   , ''AttachmentMenuBotColor
--   , ''Audio
--   , ''AuthenticationCodeInfo
--   , ''AuthenticationCodeType
--   , ''AuthorizationState
--   , ''AutoDownloadSettings
--   , ''AutoDownloadSettingsPresets
--   , ''Background
--   , ''BackgroundFill
--   , ''BackgroundType
--   , ''Backgrounds
--   , ''BankCardActionOpenUrl
--   , ''BankCardInfo
--   , ''BasicGroup
--   , ''BasicGroupFullInfo
--   , ''BotCommand
--   , ''BotCommandScope
--   , ''BotCommands
--   , ''BotInfo
--   , ''BotMenuButton
--   , ''Call
--   , ''CallDiscardReason
--   , ''CallId
--   , ''CallProblem
--   , ''CallProtocol
--   , ''CallServer
--   , ''CallServerType
--   , ''CallState
--   , ''CallbackQueryAnswer
--   , ''CallbackQueryPayload
--   , ''CanTransferOwnershipResult
--   , ''Chat
--   , ''ChatAction
--   , ''ChatActionBar
--   , ''ChatAdministrator
--   , ''ChatAdministratorRights
--   , ''ChatAdministrators
--   , ''ChatEvent
--   , ''ChatEventAction
--   , ''ChatEventLogFilters
--   , ''ChatEvents
--   , ''ChatFilter
--   , ''ChatFilterInfo
--   , ''ChatInviteLink
--   , ''ChatInviteLinkCount
--   , ''ChatInviteLinkCounts
--   , ''ChatInviteLinkInfo
--   , ''ChatInviteLinkMember
--   , ''ChatInviteLinkMembers
--   , ''ChatInviteLinks
--   , ''ChatJoinRequest
--   , ''ChatJoinRequests
--   , ''ChatJoinRequestsInfo
--   , ''ChatList
--   , ''ChatLists
--   , ''ChatLocation
--   , ''ChatMember
--   , ''ChatMemberStatus
--   , ''ChatMembers
--   , ''ChatMembersFilter
--   , ''ChatNearby
--   , ''ChatNotificationSettings
--   , ''ChatPermissions
--   , ''ChatPhoto
--   , ''ChatPhotoInfo
--   , ''ChatPhotos
--   , ''ChatPosition
--   , ''ChatReportReason
--   , ''ChatSource
--   , ''ChatStatistics
--   , ''ChatStatisticsAdministratorActionsInfo
--   , ''ChatStatisticsInviterInfo
--   , ''ChatStatisticsMessageInteractionInfo
--   , ''ChatStatisticsMessageSenderInfo
--   , ''ChatTheme
--   , ''ChatType
--   , ''Chats
--   , ''ChatsNearby
--   , ''CheckChatUsernameResult
--   , ''CheckStickerSetNameResult
--   , ''ClosedVectorPath
--   , ''ConnectedWebsite
--   , ''ConnectedWebsites
--   , ''ConnectionState
--   , ''Contact
--   , ''Count
--   , ''Countries
--   , ''CountryInfo
--   , ''CustomRequestResult
--   , ''DatabaseStatistics
--   , ''Date
--   , ''DateRange
--   , ''DatedFile
--   , ''DeepLinkInfo
--   , ''DeviceToken
--   , ''DiceStickers
--   , ''Document
--   , ''DownloadedFileCounts
--   , ''DraftMessage
--   , ''EmailAddressAuthenticationCodeInfo
--   , ''Emojis
--   , ''EncryptedCredentials
--   , ''EncryptedPassportElement
--   , ''Error
--   , ''File
--   , ''FileDownload
--   , ''FilePart
--   , ''FileType
--   , ''FormattedText
--   , ''FoundFileDownloads
--   , ''FoundMessages
--   , ''GR.GeneralResult
--   , ''Game
--   , ''GameHighScore
--   , ''GameHighScores
--   , ''GroupCall
--   , ''GroupCallId
--   , ''GroupCallParticipant
--   , ''GroupCallParticipantVideoInfo
--   , ''GroupCallRecentSpeaker
--   , ''GroupCallStream
--   , ''GroupCallStreams
--   , ''GroupCallVideoQuality
--   , ''GroupCallVideoSourceGroup
--   , ''Hashtags
--   , ''HttpUrl
--   , ''IdentityDocument
--   , ''ImportedContacts
--   , ''InlineKeyboardButton
--   , ''InlineKeyboardButtonType
--   , ''InlineQueryResult
--   , ''InlineQueryResults
--   , ''InputBackground
--   , ''InputChatPhoto
--   , ''InputCredentials
--   , ''InputFile
--   , ''InputIdentityDocument
--   , ''InputInlineQueryResult
--   , ''InputMessageContent
--   , ''InputPassportElement
--   , ''InputPassportElementError
--   , ''InputPassportElementErrorSource
--   , ''InputPersonalDocument
--   , ''InputSticker
--   , ''InputThumbnail
--   , ''InternalLinkType
--   , ''Invoice
--   , ''JsonObjectMember
--   , ''JsonValue
--   , ''KeyboardButton
--   , ''KeyboardButtonType
--   , ''LabeledPricePart
--   , ''LanguagePackInfo
--   , ''LanguagePackString
--   , ''LanguagePackStringValue
--   , ''LanguagePackStrings
--   , ''LocalFile
--   , ''LocalizationTargetInfo
--   , ''Location
--   , ''LogStream
--   , ''LogTags
--   , ''LogVerbosityLevel
--   , ''LoginUrlInfo
--   , ''MaskPoint
--   , ''MaskPosition
--   , ''Message
--   , ''MessageCalendar
--   , ''MessageCalendarDay
--   , ''MessageContent
--   , ''MessageCopyOptions
--   , ''MessageFileType
--   , ''MessageForwardInfo
--   , ''MessageForwardOrigin
--   , ''MessageInteractionInfo
--   , ''MessageLink
--   , ''MessageLinkInfo
--   , ''MessagePosition
--   , ''MessagePositions
--   , ''MessageReaction
--   , ''MessageReplyInfo
--   , ''MessageSchedulingState
--   , ''MessageSendOptions
--   , ''MessageSender
--   , ''MessageSenders
--   , ''MessageSendingState
--   , ''MessageStatistics
--   , ''MessageThreadInfo
--   , ''Messages
--   , ''Minithumbnail
--   , ''NetworkStatistics
--   , ''NetworkStatisticsEntry
--   , ''NetworkType
--   , ''Notification
--   , ''NotificationGroup
--   , ''NotificationGroupType
--   , ''NotificationSettingsScope
--   , ''NotificationSound
--   , ''NotificationSounds
--   , ''NotificationType
--   , ''Ok
--   , ''OptionValue
--   , ''OrderInfo
--   , ''PageBlock
--   , ''PageBlockCaption
--   , ''PageBlockHorizontalAlignment
--   , ''PageBlockListItem
--   , ''PageBlockRelatedArticle
--   , ''PageBlockTableCell
--   , ''PageBlockVerticalAlignment
--   , ''PassportAuthorizationForm
--   , ''PassportElement
--   , ''PassportElementError
--   , ''PassportElementErrorSource
--   , ''PassportElementType
--   , ''PassportElements
--   , ''PassportElementsWithErrors
--   , ''PassportRequiredElement
--   , ''PassportSuitableElement
--   , ''PasswordState
--   , ''PaymentForm
--   , ''PaymentReceipt
--   , ''PaymentResult
--   , ''PaymentsProviderStripe
--   , ''PersonalDetails
--   , ''PersonalDocument
--   , ''PhoneNumberAuthenticationSettings
--   , ''PhoneNumberInfo
--   , ''Photo
--   , ''PhotoSize
--   , ''Point
--   , ''Poll
--   , ''PollOption
--   , ''PollType
--   , ''ProfilePhoto
--   , ''Proxies
--   , ''Proxy
--   , ''ProxyType
--   , ''PublicChatType
--   , ''PushMessageContent
--   , ''PushReceiverId
--   , ''Reaction
--   , ''RecommendedChatFilter
--   , ''RecommendedChatFilters
--   , ''RecoveryEmailAddress
--   , ''RemoteFile
--   , ''ReplyMarkup
--   , ''ResetPasswordResult
--   , ''RichText
--   , ''RtmpUrl
--   , ''SavedCredentials
--   , ''ScopeNotificationSettings
--   , ''SearchMessagesFilter
--   , ''Seconds
--   , ''SecretChat
--   , ''SecretChatState
--   , ''SentWebAppMessage
--   , ''Session
--   , ''SessionType
--   , ''Sessions
--   , ''ShippingOption
--   , ''SponsoredMessage
--   , ''StatisticalGraph
--   , ''StatisticalValue
--   , ''Sticker
--   , ''StickerSet
--   , ''StickerSetInfo
--   , ''StickerSets
--   , ''StickerType
--   , ''Stickers
--   , ''StorageStatistics
--   , ''StorageStatisticsByChat
--   , ''StorageStatisticsByFileType
--   , ''StorageStatisticsFast
--   , ''SuggestedAction
--   , ''Supergroup
--   , ''SupergroupFullInfo
--   , ''SupergroupMembersFilter
--   , ''TMeUrl
--   , ''TMeUrlType
--   , ''TMeUrls
--   , ''TdlibParameters
--   , ''TemporaryPasswordState
--   , ''TermsOfService
--   , ''TestBytes
--   , ''TestInt
--   , ''TestString
--   , ''TestVectorInt
--   , ''TestVectorIntObject
--   , ''TestVectorString
--   , ''TestVectorStringObject
--   , ''Text
--   , ''TextEntities
--   , ''TextEntity
--   , ''TextEntityType
--   , ''TextParseMode
--   , ''ThemeParameters
--   , ''ThemeSettings
--   , ''Thumbnail
--   , ''ThumbnailFormat
--   , ''TopChatCategory
--   , ''UnreadReaction
--   , ''Update
--   , ''Updates
--   , ''User
--   , ''UserFullInfo
--   , ''UserPrivacySetting
--   , ''UserPrivacySettingRule
--   , ''UserPrivacySettingRules
--   , ''UserStatus
--   , ''UserType
--   , ''Users
--   , ''ValidatedOrderInfo
--   , ''VectorPathCommand
--   , ''Venue
--   , ''Video
--   , ''VideoChat
--   , ''WebAppInfo
--   , ''WebPage
--   , ''WebPageInstantView
--   ]


-- concat <$> mapM goN
--   [ ''AddedReactions
--   , ''AvailableReactions
--   , ''VideoNote
--   , ''VoiceNote
--   ]

------------------------------------------------------------------------------------------

-- concat <$> traverse gof
--   [ ("AdRs", ''AddedReactions)
--   , ("AvRs", ''AvailableReactions)
--   , ("ViNe", ''VideoNote)
--   ]




------------------------------------------------------------------------------------------

-- concat <$> mapM makeLenses [''Foo, ''Bar, ''Quux]
-- join $ gof <$> pure "ViNe" <*> pure ''VoiceNote

-- import qualified TD.Data.FormattedText       as FT
-- import qualified TD.Data.InputMessageContent as IMC
-- import qualified TD.Data.Message             as M
-- import qualified TD.Data.MessageContent      as MC
-- import qualified TD.Data.MessageSendOptions  as MSO
-- import qualified TD.Data.Update              as U
-- import qualified TD.Query.AddFileToDownloads as AFD
-- import qualified TD.Query.DownloadFile       as DF
-- import qualified TD.Query.SendMessage        as SM


-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_AFD"))])) ''AFD.AddFileToDownloads



-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_DF"))])) ''DF.DownloadFile


-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_M"))])) ''M.Message



-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_MC"))])) ''MC.MessageContent



-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_U"))])) ''U.Update



-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_FT"))])) ''FT.FormattedText


-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_SM"))])) ''SM.SendMessage


-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_MSO"))])) ''MSO.MessageSendOptions


-- makeLensesWith (lensRules & lensField .~ (\_ _ n -> case nameBase n of
--                                                       name -> [TopName (mkName (name <> "_IMC"))])) ''IMC.InputMessageContent



------------------------------------------------------------------------------------------

-- makeLenses ''Message

-- concat <$> mapM (makeLensesWith (lensRules & lensField .~ (\_ _ n ->
--                                             case nameBase n of
--                                               name -> [TopName (mkName (name <> "_"))]
--                                               _ -> []))) [''M.Message, ''MC.MessageContent]



-- m :: String -> Name -> DecsQ
-- m l m = \l m -> makeLensesWith (lensRules & lensField .~ (\_ _ n -> [TopName (mkName ((nameBase n) <> l))])) m

