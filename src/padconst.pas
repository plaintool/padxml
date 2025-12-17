//-----------------------------------------------------------------------------------
//  PadXml © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/padxml
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit padconst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  // Enumerations for PAD standard values
  TPadInstallSupport = (
    pisNone,
    pisInstallAndUninstall,
    pisInstallOnly,
    pisNoInstallSupport,
    pisUninstallOnly
    );

  TPadProgramType = (
    pptNone,
    pptShareware,
    pptFreeware,
    pptAdware,
    pptDemo,
    pptCommercial,
    pptDataOnly
    );

  TPadReleaseStatus = (
    prsNone,
    prsUpdate,
    prsMajorUpdate,
    prsMinorUpdate,
    prsNewRelease,
    prsBeta,
    prsAlpha,
    prsMediaOnly
    );

  TPadExpireBasedOn = (
    pebNone,
    pebDays,
    pebUses,
    pebEitherOr
    );

  TPadNewsFeedType = (
    pnftNone,
    pnftRSS090,
    pnftRSS091,
    pnftRSS092,
    pnftRSS093,
    pnftRSS094,
    pnftRSS10,
    pnftRSS20,
    pnftAtom03,
    pnftAtom10,
    pnftAtom,
    pnftCDF,
    pnftOther
    );

  TPadProgramCategoryClass = (
    pccNone,
    // Audio & Multimedia
    pccAudioMultimedia_AudioEncodersDecoders,
    pccAudioMultimedia_AudioFilePlayers,
    pccAudioMultimedia_AudioFileRecorders,
    pccAudioMultimedia_CDBurners,
    pccAudioMultimedia_CDPlayers,
    pccAudioMultimedia_MultimediaCreationTools,
    pccAudioMultimedia_MusicComposers,
    pccAudioMultimedia_Other,
    pccAudioMultimedia_PresentationTools,
    pccAudioMultimedia_RippersConverters,
    pccAudioMultimedia_Speech,
    pccAudioMultimedia_VideoTools,

    // Business
    pccBusiness_AccountingFinance,
    pccBusiness_CalculatorsConverters,
    pccBusiness_DatabasesTools,
    pccBusiness_HelpdeskRemotePC,
    pccBusiness_InventoryBarcoding,
    pccBusiness_InvestmentTools,
    pccBusiness_MathScientificTools,
    pccBusiness_OfficeSuitesTools,
    pccBusiness_Other,
    pccBusiness_PIMSCalendars,
    pccBusiness_ProjectManagement,
    pccBusiness_VerticalMarketApps,

    // Communications
    pccCommunications_ChatInstantMessaging,
    pccCommunications_DialUpConnectionTools,
    pccCommunications_EmailClients,
    pccCommunications_EmailListManagement,
    pccCommunications_FaxTools,
    pccCommunications_NewsgroupClients,
    pccCommunications_OtherCommsTools,
    pccCommunications_OtherEmailTools,
    pccCommunications_PagerTools,
    pccCommunications_Telephony,
    pccCommunications_VideoCams,

    // Desktop
    pccDesktop_ClocksAlarms,
    pccDesktop_CursorsFonts,
    pccDesktop_Icons,
    pccDesktop_Other,
    pccDesktop_ScreenSaversArt,
    pccDesktop_ScreenSaversCartoons,
    pccDesktop_ScreenSaversNature,
    pccDesktop_ScreenSaversOther,
    pccDesktop_ScreenSaversPeople,
    pccDesktop_ScreenSaversScience,
    pccDesktop_ScreenSaversSeasonal,
    pccDesktop_ScreenSaversVehicles,
    pccDesktop_ThemesWallpaper,

    // Development
    pccDevelopment_ActiveX,
    pccDevelopment_BasicVBVBDotNet,
    pccDevelopment_C_CPlusPlus_CSharp,
    pccDevelopment_CompilersInterpreters,
    pccDevelopment_ComponentsLibraries,
    pccDevelopment_Debugging,
    pccDevelopment_Delphi,
    pccDevelopment_HelpTools,
    pccDevelopment_InstallSetup,
    pccDevelopment_ManagementDistribution,
    pccDevelopment_Other,
    pccDevelopment_SourceEditors,

    // Education
    pccEducation_Computer,
    pccEducation_Dictionaries,
    pccEducation_Geography,
    pccEducation_Kids,
    pccEducation_Languages,
    pccEducation_Mathematics,
    pccEducation_Other,
    pccEducation_ReferenceTools,
    pccEducation_Science,
    pccEducation_TeachingTrainingTools,

    // Games & Entertainment
    pccGamesEntertainment_Action,
    pccGamesEntertainment_AdventureRoleplay,
    pccGamesEntertainment_Arcade,
    pccGamesEntertainment_Board,
    pccGamesEntertainment_Card,
    pccGamesEntertainment_CasinoGambling,
    pccGamesEntertainment_Kids,
    pccGamesEntertainment_OnlineGaming,
    pccGamesEntertainment_Other,
    pccGamesEntertainment_PuzzleWordGames,
    pccGamesEntertainment_Simulation,
    pccGamesEntertainment_Sports,
    pccGamesEntertainment_StrategyWarGames,
    pccGamesEntertainment_ToolsEditors,

    // Graphic Apps
    pccGraphicApps_AnimationTools,
    pccGraphicApps_CAD,
    pccGraphicApps_ConvertersOptimizers,
    pccGraphicApps_Editors,
    pccGraphicApps_FontTools,
    pccGraphicApps_GalleryCatalogingTools,
    pccGraphicApps_IconTools,
    pccGraphicApps_Other,
    pccGraphicApps_ScreenCapture,
    pccGraphicApps_Viewers,

    // Home & Hobby
    pccHomeHobby_AstrologyBiorhythmsMystic,
    pccHomeHobby_Astronomy,
    pccHomeHobby_Cataloging,
    pccHomeHobby_FoodDrink,
    pccHomeHobby_Genealogy,
    pccHomeHobby_HealthNutrition,
    pccHomeHobby_Other,
    pccHomeHobby_PersonalFinance,
    pccHomeHobby_PersonalInterest,
    pccHomeHobby_Recreation,
    pccHomeHobby_Religion,

    // Network & Internet
    pccNetworkInternet_AdBlockers,
    pccNetworkInternet_BrowserTools,
    pccNetworkInternet_Browsers,
    pccNetworkInternet_DownloadManagers,
    pccNetworkInternet_FileSharingPeerToPeer,
    pccNetworkInternet_FTPClients,
    pccNetworkInternet_NetworkMonitoring,
    pccNetworkInternet_Other,
    pccNetworkInternet_RemoteComputing,
    pccNetworkInternet_SearchLookupTools,
    pccNetworkInternet_TerminalTelnetClients,
    pccNetworkInternet_TimersTimeSynch,
    pccNetworkInternet_TracePingTools,

    // Security & Privacy
    pccSecurityPrivacy_AccessControl,
    pccSecurityPrivacy_AntiSpamAntiSpyTools,
    pccSecurityPrivacy_AntiVirusTools,
    pccSecurityPrivacy_CovertSurveillance,
    pccSecurityPrivacy_EncryptionTools,
    pccSecurityPrivacy_Other,
    pccSecurityPrivacy_PasswordManagers,

    // Servers
    pccServers_FirewallProxyServers,
    pccServers_FTPServers,
    pccServers_MailServers,
    pccServers_NewsServers,
    pccServers_OtherServerApplications,
    pccServers_TelnetServers,
    pccServers_WebServers,

    // System Utilities
    pccSystemUtilities_AutomationTools,
    pccSystemUtilities_BackupRestore,
    pccSystemUtilities_Benchmarking,
    pccSystemUtilities_ClipboardTools,
    pccSystemUtilities_FileDiskManagement,
    pccSystemUtilities_FileCompression,
    pccSystemUtilities_LaunchersTaskManagers,
    pccSystemUtilities_Other,
    pccSystemUtilities_Printer,
    pccSystemUtilities_RegistryTools,
    pccSystemUtilities_ShellTools,
    pccSystemUtilities_SystemMaintenance,
    pccSystemUtilities_TextDocumentEditors,

    // Web Development
    pccWebDevelopment_ASPPHP,
    pccWebDevelopment_ECommerce,
    pccWebDevelopment_FlashTools,
    pccWebDevelopment_HTMLTools,
    pccWebDevelopment_JavaJavaScript,
    pccWebDevelopment_LogAnalysers,
    pccWebDevelopment_Other,
    pccWebDevelopment_SiteAdministration,
    pccWebDevelopment_WizardsComponents,
    pccWebDevelopment_XMLCSSTools
    );

  // Unix/Linux/Mac family
  TPadOSUnixLinux = (
    posAndroid,
    posBlackBerry,
    posHandheldMobileOther,
    posiPhone,
    posiPad,
    posiPod,
    posiTouch,
    posJava,
    posLinux,
    posLinuxConsole,
    posLinuxGnome,
    posLinuxGPL,
    posLinuxOpenSource,
    posMacOSX,
    posMacOther,
    posMSDOS,
    posNetware,
    posOpenVMS,
    posPalm,
    posPocketPC,
    posSymbian,
    posUnix,
    posMacPPC,
    posMacOS9,
    posMacOSX101,
    posMacOSX102,
    posMacOSX103,
    posMacOSX104,
    posMacOSX105
    );

  // Windows family
  TPadOSWindows = (
    posWin2000,
    posWin7x32,
    posWin7x64,
    posWin95,
    posWin98,
    posWinME,
    posWinNT4x,
    posWindows2000,
    posWinMobile,
    posWinOther,
    posWinServer,
    posWinVista,
    posWinVistax64,
    posWinXP,
    posWindows2003,
    posWindowsCE,
    posWindowsMobile2003,
    posWindowsMobile2005
    );

  // Mobile and Embedded
  TPadOSOther = (
    posOther,
    posNotApplicable
    );

  // Modern OS
  TPadOSModern = (
    posWindowsXP,
    posWindowsTabletPCEdition2005,
    posWindowsMediaCenterEdition2005,
    posWindowsVistaStarter,
    posWindowsVistaHomeBasic,
    posWindowsVistaHomePremium,
    posWindowsVistaBusiness,
    posWindowsVistaEnterprise,
    posWindowsVistaUltimate,
    posWindowsVistaStarter64,
    posWindowsVistaHomeBasic64,
    posWindowsVistaHomePremium64,
    posWindowsVistaBusiness64,
    posWindowsVistaEnterprise64,
    posWindowsVistaUltimat64,
    posWindows7,
    posWindows8,
    posWindows10,
    posWindows11,
    posWindows12,
    posWindowsServer2000,
    posWindowsServer2003,
    posWindowsServer2008,
    posWindowsServer2008R2,
    posWindowsServer2012,
    posWindowsServer2012R2,
    posWindowsServer2016,
    posWindowsServer2019,
    posWindowsServer2022,
    posWindowsServer2025,
    posMacOs
    );

  // Language groups - split by geographic regions
  // European languages (31 items)
  TPadLangEuropean = (
    plEnglish,
    plFrench,
    plFrisian,
    plGerman,
    plSpanish,
    plItalian,
    plDutch,
    plPortuguese,
    plSwedish,
    plDanish,
    plNorwegian,
    plFinnish,
    plRussian,
    plPolish,
    plCzech,
    plHungarian,
    plRomanian,
    plBulgarian,
    plGreek,
    plTurkish,
    plAlbanian,
    plCroatian,
    plSerbian,
    plSlovak,
    plSlovenian,
    plUkrainian,
    plBelarusian,
    plLithuanian,
    plLatvian,
    plEstonian,
    plIcelandic,
    plMaltese
    );

  // Asian languages (29 items)
  TPadLangAsian = (
    plJapanese,
    plChinese,
    plChineseSimplified,
    plChineseTraditional,
    plKorean,
    plArabic,
    plHebrew,
    plHindi,
    plBengali,
    plUrdu,
    plPersian,
    plThai,
    plVietnamese,
    plIndonesian,
    plMalay,
    plFilipino,
    plBurmese,
    plCambodian,
    plLao,
    plMongolian,
    plNepali,
    plSinhalese,
    plTamil,
    plTelugu,
    plMarathi,
    plGujarati,
    plPunjabi,
    plKannada,
    plMalayalam
    );

  // Other major languages (27 items)
  TPadLangOtherMajor = (
    plAfrikaans,
    plAmharic,
    plArmenian,
    plAzerbaijani,
    plBasque,
    plCatalan,
    plEsperanto,
    plFarsi,
    plGeorgian,
    plHausa,
    plIrish,
    plKazakh,
    plKyrgyz,
    plLatin,
    plLuxembourgish,
    plMacedonian,
    plMaori,
    plPashto,
    plRomansh,
    plSanskrit,
    plScottishGaelic,
    plSwahili,
    plTajik,
    plTatar,
    plTurkmen,
    plUzbek,
    plWelsh,
    plOther
    );

  // Additional world languages (32 items)
  TPadLangWorld = (
    plAkan,
    plAssamese,
    plAymara,
    plBambara,
    plBashkir,
    plBelarusianLatin,
    plBislama,
    plBosnian,
    plBreton,
    plChichewa,
    plCorsican,
    plDivehi,
    plDzongkha,
    plFaroese,
    plFijian,
    plGalician,
    plGreenlandic,
    plGuarani,
    plHaitianCreole,
    plHawaiian,
    plInuktitut,
    plJavanese,
    plKinyarwanda,
    plKurdish,
    plLingala,
    plMalagasy,
    plMarshallese,
    plNauru,
    plOromo,
    plQuechua,
    plSamoan,
    plYoruba
    );

  // Sets for OS groups
  TPadOSUnixLinuxSet = set of TPadOSUnixLinux;
  TPadOSWindowsSet = set of TPadOSWindows;
  TPadOSOtherSet = set of TPadOSOther;
  TPadOSModernSet = set of TPadOSModern;

  // Sets for language groups
  TPadLangEuropeanSet = set of TPadLangEuropean;
  TPadLangAsianSet = set of TPadLangAsian;
  TPadLangOtherMajorSet = set of TPadLangOtherMajor;
  TPadLangWorldSet = set of TPadLangWorld;

  // XML Encoding types
  TPadEncoding = (
    peUTF8,          // UTF-8 (default)
    peUTF16,         // UTF-16
    peUTF16BE,       // UTF-16 Big Endian
    peUTF16LE,       // UTF-16 Little Endian
    peISO88591,      // ISO-8859-1 (Latin-1)
    peISO88592,      // ISO-8859-2 (Latin-2)
    peISO885915,     // ISO-8859-15 (Latin-9)
    peWindows1250,   // Windows-1250 (Central European)
    peWindows1251,   // Windows-1251 (Cyrillic)
    peWindows1252,   // Windows-1252 (Western European)
    peWindows1253,   // Windows-1253 (Greek)
    peWindows1254,   // Windows-1254 (Turkish)
    peWindows1255,   // Windows-1255 (Hebrew)
    peWindows1256,   // Windows-1256 (Arabic)
    peWindows1257,   // Windows-1257 (Baltic)
    peWindows1258,   // Windows-1258 (Vietnamese)
    peKOI8R,         // KOI8-R (Russian)
    peKOI8U,         // KOI8-U (Ukrainian)
    peGB2312,        // GB2312 (Simplified Chinese)
    peGBK,           // GBK (Simplified Chinese)
    peGB18030,       // GB18030 (Simplified Chinese)
    peBig5,          // Big5 (Traditional Chinese)
    peShiftJIS,      // Shift_JIS (Japanese)
    peEUCJP,         // EUC-JP (Japanese)
    peEUCKR,         // EUC-KR (Korean)
    peISO2022JP,     // ISO-2022-JP (Japanese)
    peASCII,         // ASCII (US-ASCII)
    peNone           // No encoding specified
    );

  TPadEmptyTagType = (
    ettWithoutSpace,   // <Empty/> (default)
    ettWithSpace,      // <Empty />
    ettDoubleSided     // <Empty></Empty>
    );

const
  CategoryCount = Ord(High(TPadProgramCategoryClass)) - Ord(Low(TPadProgramCategoryClass)) + 1;

  // News Feed Type strings
  PadNewsFeedTypeStrings: array[TPadNewsFeedType] of string = (
    '',
    'RSS 0.90',
    'RSS 0.91',
    'RSS 0.92',
    'RSS 0.93',
    'RSS 0.94',
    'RSS 1.0',
    'RSS 2.0',
    'Atom 0.3',
    'Atom 1.0',
    'Atom',
    'CDF',
    'Other'
    );

  PadProgramCategoryClassStrings: array[TPadProgramCategoryClass] of string = (
    '',
    // Audio & Multimedia
    'Audio & Multimedia::Audio Encoders/Decoders',
    'Audio & Multimedia::Audio File Players',
    'Audio & Multimedia::Audio File Recorders',
    'Audio & Multimedia::CD Burners',
    'Audio & Multimedia::CD Players',
    'Audio & Multimedia::Multimedia Creation Tools',
    'Audio & Multimedia::Music Composers',
    'Audio & Multimedia::Other',
    'Audio & Multimedia::Presentation Tools',
    'Audio & Multimedia::Rippers & Converters',
    'Audio & Multimedia::Speech',
    'Audio & Multimedia::Video Tools',

    // Business
    'Business::Accounting & Finance',
    'Business::Calculators & Converters',
    'Business::Databases & Tools',
    'Business::Helpdesk & Remote PC',
    'Business::Inventory & Barcoding',
    'Business::Investment Tools',
    'Business::Math & Scientific Tools',
    'Business::Office Suites & Tools',
    'Business::Other',
    'Business::PIMS & Calendars',
    'Business::Project Management',
    'Business::Vertical Market Apps',

    // Communications
    'Communications::Chat & Instant Messaging',
    'Communications::Dial Up & Connection Tools',
    'Communications::E-Mail Clients',
    'Communications::E-Mail List Management',
    'Communications::Fax Tools',
    'Communications::Newsgroup Clients',
    'Communications::Other Comms Tools',
    'Communications::Other E-Mail Tools',
    'Communications::Pager Tools',
    'Communications::Telephony',
    'Communications::Web/Video Cams',

    // Desktop
    'Desktop::Clocks & Alarms',
    'Desktop::Cursors & Fonts',
    'Desktop::Icons',
    'Desktop::Other',
    'Desktop::Screen Savers: Art',
    'Desktop::Screen Savers: Cartoons',
    'Desktop::Screen Savers: Nature',
    'Desktop::Screen Savers: Other',
    'Desktop::Screen Savers: People',
    'Desktop::Screen Savers: Science',
    'Desktop::Screen Savers: Seasonal',
    'Desktop::Screen Savers: Vehicles',
    'Desktop::Themes & Wallpaper',

    // Development
    'Development::Active X',
    'Development::Basic, VB, VB DotNet',
    'Development::C / C++ / C#',
    'Development::Compilers & Interpreters',
    'Development::Components & Libraries',
    'Development::Debugging',
    'Development::Delphi',
    'Development::Help Tools',
    'Development::Install & Setup',
    'Development::Management & Distribution',
    'Development::Other',
    'Development::Source Editors',

    // Education
    'Education::Computer',
    'Education::Dictionaries',
    'Education::Geography',
    'Education::Kids',
    'Education::Languages',
    'Education::Mathematics',
    'Education::Other',
    'Education::Reference Tools',
    'Education::Science',
    'Education::Teaching & Training Tools',

    // Games & Entertainment
    'Games & Entertainment::Action',
    'Games & Entertainment::Adventure & Roleplay',
    'Games & Entertainment::Arcade',
    'Games & Entertainment::Board',
    'Games & Entertainment::Card',
    'Games & Entertainment::Casino & Gambling',
    'Games & Entertainment::Kids',
    'Games & Entertainment::Online Gaming',
    'Games & Entertainment::Other',
    'Games & Entertainment::Puzzle & Word Games',
    'Games & Entertainment::Simulation',
    'Games & Entertainment::Sports',
    'Games & Entertainment::Strategy & War Games',
    'Games & Entertainment::Tools & Editors',

    // Graphic Apps
    'Graphic Apps::Animation Tools',
    'Graphic Apps::CAD',
    'Graphic Apps::Converters & Optimizers',
    'Graphic Apps::Editors',
    'Graphic Apps::Font Tools',
    'Graphic Apps::Gallery & Cataloging Tools',
    'Graphic Apps::Icon Tools',
    'Graphic Apps::Other',
    'Graphic Apps::Screen Capture',
    'Graphic Apps::Viewers',

    // Home & Hobby
    'Home & Hobby::Astrology/Biorhythms/Mystic',
    'Home & Hobby::Astronomy',
    'Home & Hobby::Cataloging',
    'Home & Hobby::Food & Drink',
    'Home & Hobby::Genealogy',
    'Home & Hobby::Health & Nutrition',
    'Home & Hobby::Other',
    'Home & Hobby::Personal Finance',
    'Home & Hobby::Personal Interest',
    'Home & Hobby::Recreation',
    'Home & Hobby::Religion',

    // Network & Internet
    'Network & Internet::Ad Blockers',
    'Network & Internet::Browser Tools',
    'Network & Internet::Browsers',
    'Network & Internet::Download Managers',
    'Network & Internet::File Sharing/Peer to Peer',
    'Network & Internet::FTP Clients',
    'Network & Internet::Network Monitoring',
    'Network & Internet::Other',
    'Network & Internet::Remote Computing',
    'Network & Internet::Search/Lookup Tools',
    'Network & Internet::Terminal & Telnet Clients',
    'Network & Internet::Timers & Time Synch',
    'Network & Internet::Trace & Ping Tools',

    // Security & Privacy
    'Security & Privacy::Access Control',
    'Security & Privacy::Anti-Spam & Anti-Spy Tools',
    'Security & Privacy::Anti-Virus Tools',
    'Security & Privacy::Covert Surveillance',
    'Security & Privacy::Encryption Tools',
    'Security & Privacy::Other',
    'Security & Privacy::Password Managers',

    // Servers
    'Servers::Firewall & Proxy Servers',
    'Servers::FTP Servers',
    'Servers::Mail Servers',
    'Servers::News Servers',
    'Servers::Other Server Applications',
    'Servers::Telnet Servers',
    'Servers::Web Servers',

    // System Utilities
    'System Utilities::Automation Tools',
    'System Utilities::Backup & Restore',
    'System Utilities::Benchmarking',
    'System Utilities::Clipboard Tools',
    'System Utilities::File & Disk Management',
    'System Utilities::File Compression',
    'System Utilities::Launchers & Task Managers',
    'System Utilities::Other',
    'System Utilities::Printer',
    'System Utilities::Registry Tools',
    'System Utilities::Shell Tools',
    'System Utilities::System Maintenance',
    'System Utilities::Text/Document Editors',

    // Web Development
    'Web Development::ASP & PHP',
    'Web Development::E-Commerce',
    'Web Development::Flash Tools',
    'Web Development::HTML Tools',
    'Web Development::Java & JavaScript',
    'Web Development::Log Analysers',
    'Web Development::Other',
    'Web Development::Site Administration',
    'Web Development::Wizards & Components',
    'Web Development::XML/CSS Tools'
    );

  PadOSUnixLinuxStrings: array[TPadOSUnixLinux] of string = (
    'Android',
    'BlackBerry',
    'Handheld/Mobile Other',
    'iPhone',
    'iPad',
    'iPod',
    'iTouch',
    'Java',
    'Linux',
    'Linux Console',
    'Linux Gnome',
    'Linux GPL',
    'Linux Open Source',
    'Mac OS X',
    'Mac Other',
    'MS-DOS',
    'Netware',
    'OpenVMS',
    'Palm',
    'Pocket PC',
    'Symbian',
    'Unix',
    'Mac PPC',
    'Mac OS 9',
    'Mac OS X 10.1',
    'Mac OS X 10.2',
    'Mac OS X 10.3',
    'Mac OS X 10.4',
    'Mac OS X 10.5'
    );

  // String arrays for OS groups - based on original list from ProgramOs.txt
  PadOSWindowsStrings: array[TPadOSWindows] of string = (
    'Win2000',
    'Win7 x32',
    'Win7 x64',
    'Win95',
    'Win98',
    'WinME',
    'WinNT 4.x',
    'Windows2000',
    'WinMobile',
    'WinOther',
    'WinServer',
    'WinVista',
    'WinVista x64',
    'WinXP',
    'Windows2003',
    'Windows CE',
    'Windows Mobile 2003',
    'Windows Mobile 2005'
    );

  PadOSOtherStrings: array[TPadOSOther] of string = (
    'Other',
    'Not Applicable');

  PadOSModernStrings: array[TPadOSModern] of string = (
    'Windows XP',
    'Windows Tablet PC Edition 2005',
    'Windows Media Center Edition 2005',
    'Windows Vista Starter',
    'Windows Vista Home Basic',
    'Windows Vista Home Premium',
    'Windows Vista Business',
    'Windows Vista Enterprise',
    'Windows Vista Ultimate',
    'Windows Vista Starter x64',
    'Windows Vista Home Basic x64',
    'Windows Vista Home Premium x64',
    'Windows Vista Business x64',
    'Windows Vista Enterprise x64',
    'Windows Vista Ultimate x64',
    'Windows 7',
    'Windows 8',
    'Windows 10',
    'Windows 11',
    'Windows 12',
    'Windows Server 2000',
    'Windows Server 2003',
    'Windows Server 2008',
    'Windows Server 2008R2',
    'Windows Server 2012',
    'Windows Server 2012R2',
    'Windows Server 2016',
    'Windows Server 2019',
    'Windows Server 2022',
    'Windows Server 2025',
    'MacOS'
    );

  // String arrays for language groups
  PadLangEuropeanStrings: array[TPadLangEuropean] of string = (
    'English',
    'French',
    'Frisian',
    'German',
    'Spanish',
    'Italian',
    'Dutch',
    'Portuguese',
    'Swedish',
    'Danish',
    'Norwegian',
    'Finnish',
    'Russian',
    'Polish',
    'Czech',
    'Hungarian',
    'Romanian',
    'Bulgarian',
    'Greek',
    'Turkish',
    'Albanian',
    'Croatian',
    'Serbian',
    'Slovak',
    'Slovenian',
    'Ukrainian',
    'Byelorussian',
    'Lithuanian',
    'Latvian',
    'Estonian',
    'Icelandic',
    'Maltese'
    );

  PadLangAsianStrings: array[TPadLangAsian] of string = (
    'Japanese',
    'Chinese',
    'ChineseSimplified',
    'ChineseTraditional',
    'Korean',
    'Arabic',
    'Hebrew',
    'Hindi',
    'Bengali',
    'Urdu',
    'Persian',
    'Thai',
    'Vietnamese',
    'Indonesian',
    'Malay',
    'Filipino',
    'Burmese',
    'Cambodian',
    'Lao',
    'Mongolian',
    'Nepali',
    'Sinhalese',
    'Tamil',
    'Telugu',
    'Marathi',
    'Gujarati',
    'Punjabi',
    'Kannada',
    'Malayalam'
    );

  PadLangOtherMajorStrings: array[TPadLangOtherMajor] of string = (
    'Afrikaans',
    'Amharic',
    'Armenian',
    'Azerbaijani',
    'Basque',
    'Catalan',
    'Esperanto',
    'Farsi',
    'Georgian',
    'Hausa',
    'Irish',
    'Kazakh',
    'Kyrgyz',
    'Latin',
    'Luxembourgish',
    'Macedonian',
    'Maori',
    'Pashto',
    'Romansh',
    'Sanskrit',
    'Scottish Gaelic',
    'Swahili',
    'Tajik',
    'Tatar',
    'Turkmen',
    'Uzbek',
    'Welsh',
    'Other'
    );

  PadLangWorldStrings: array[TPadLangWorld] of string = (
    'Akan',
    'Assamese',
    'Aymara',
    'Bambara',
    'Bashkir',
    'Belarusian (Latin)',
    'Bislama',
    'Bosnian',
    'Breton',
    'Chichewa',
    'Corsican',
    'Divehi',
    'Dzongkha',
    'Faroese',
    'Fijian',
    'Galician',
    'Greenlandic',
    'Guarani',
    'Haitian Creole',
    'Hawaiian',
    'Inuktitut',
    'Javanese',
    'Kinyarwanda',
    'Kurdish',
    'Lingala',
    'Malagasy',
    'Marshallese',
    'Nauru',
    'Oromo',
    'Quechua',
    'Samoan',
    'Yoruba'
    );

  PadEncodingStrings: array[TPadEncoding] of string = (
    'UTF-8',
    'UTF-16',
    'UTF-16BE',
    'UTF-16LE',
    'ISO-8859-1',
    'ISO-8859-2',
    'ISO-8859-15',
    'Windows-1250',
    'Windows-1251',
    'Windows-1252',
    'Windows-1253',
    'Windows-1254',
    'Windows-1255',
    'Windows-1256',
    'Windows-1257',
    'Windows-1258',
    'KOI8-R',
    'KOI8-U',
    'GB2312',
    'GBK',
    'GB18030',
    'Big5',
    'Shift_JIS',
    'EUC-JP',
    'EUC-KR',
    'ISO-2022-JP',
    'ASCII',
    ''
    );

implementation

end.
