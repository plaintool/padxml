//-----------------------------------------------------------------------------------
//  PadXml © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/padxml
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit padformat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  TypInfo,
  DOM,
  XMLRead,
  XMLWrite,
  padconst;

type
  // Forward declarations
  TPadContactInfo = class;
  TPadSupportInfo = class;
  TPadFileInfo = class;
  TPadExpireInfo = class;
  TPadEnglishDescription = class;
  TPadApplicationURLs = class;
  TPadDownloadURLs = class;

  { TPadMasterVersionInfo }
  TPadMasterVersionInfo = class(TPersistent)
  private
    FVersion: double;
    FMasterPadVersion: string;
    FMasterPadEditor: string;
    FMasterPadEditorUrl: string;
    FMasterPadInfo: string;
    procedure SetMasterPadVersion(Value: string);
  protected
    property Version: double read FVersion write FVersion;
  published
    property MasterPadVersion: string read FMasterPadVersion write SetMasterPadVersion;
    property MasterPadEditor: string read FMasterPadEditor write FMasterPadEditor;
    property MasterPadEditorUrl: string read FMasterPadEditorUrl write FMasterPadEditorUrl;
    property MasterPadInfo: string read FMasterPadInfo write FMasterPadInfo;
  end;

  { TPadContactInfo }
  TPadContactInfo = class(TPersistent)
  private
    FAuthorFirstName: string;
    FAuthorLastName: string;
    FAuthorEmail: string;
    FContactFirstName: string;
    FContactLastName: string;
    FContactEmail: string;
  published
    property AuthorFirstName: string read FAuthorFirstName write FAuthorFirstName;
    property AuthorLastName: string read FAuthorLastName write FAuthorLastName;
    property AuthorEmail: string read FAuthorEmail write FAuthorEmail;
    property ContactFirstName: string read FContactFirstName write FContactFirstName;
    property ContactLastName: string read FContactLastName write FContactLastName;
    property ContactEmail: string read FContactEmail write FContactEmail;
  end;

  { TPadSupportInfo }
  TPadSupportInfo = class(TPersistent)
  private
    FSalesEmail: string;
    FSupportEmail: string;
    FGeneralEmail: string;
    FSalesPhone: string;
    FSupportPhone: string;
    FGeneralPhone: string;
    FFaxPhone: string;
  published
    property SalesEmail: string read FSalesEmail write FSalesEmail;
    property SupportEmail: string read FSupportEmail write FSupportEmail;
    property GeneralEmail: string read FGeneralEmail write FGeneralEmail;
    property SalesPhone: string read FSalesPhone write FSalesPhone;
    property SupportPhone: string read FSupportPhone write FSupportPhone;
    property GeneralPhone: string read FGeneralPhone write FGeneralPhone;
    property FaxPhone: string read FFaxPhone write FFaxPhone;
  end;

  { TPadCompanyInfo }
  TPadCompanyInfo = class(TPersistent)
  private
    FCompanyName: string;
    FAddress1: string;
    FAddress2: string;
    FCityTown: string;
    FStateProvince: string;
    FZipPostalCode: string;
    FCountry: string;
    FCompanyWebsiteURL: string;
    FContactInfo: TPadContactInfo;
    FSupportInfo: TPadSupportInfo;
    FGooglePlusPage: string;
    FLinkedinPage: string;
    FTwitterCompanyPage: string;
    FFacebookCompanyPage: string;
    FCompanyStorePage: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property CompanyName: string read FCompanyName write FCompanyName;
    property Address1: string read FAddress1 write FAddress1;
    property Address2: string read FAddress2 write FAddress2;
    property CityTown: string read FCityTown write FCityTown;
    property StateProvince: string read FStateProvince write FStateProvince;
    property ZipPostalCode: string read FZipPostalCode write FZipPostalCode;
    property Country: string read FCountry write FCountry;
    property CompanyWebsiteURL: string read FCompanyWebsiteURL write FCompanyWebsiteURL;
    property ContactInfo: TPadContactInfo read FContactInfo write FContactInfo;
    property SupportInfo: TPadSupportInfo read FSupportInfo write FSupportInfo;
    property GooglePlusPage: string read FGooglePlusPage write FGooglePlusPage;
    property LinkedinPage: string read FLinkedinPage write FLinkedinPage;
    property TwitterCompanyPage: string read FTwitterCompanyPage write FTwitterCompanyPage;
    property FacebookCompanyPage: string read FFacebookCompanyPage write FFacebookCompanyPage;
    property CompanyStorePage: string read FCompanyStorePage write FCompanyStorePage;
  end;

  { TPadNewsFeed }
  TPadNewsFeed = class(TPersistent)
  private
    // Original fields for XML serialization
    FNewsFeed_FORM: boolean;
    FNewsFeed_VERSION: string;
    FNewsFeed_URL: string;
    FNewsFeed_Type: TPadNewsFeedType;
    FNewsFeed_Language: string;
    FNewsFeed_Purpose: string;
    FNewsFeed_Author_Email: string;
    FNewsFeed_Author_First_Name: string;
    FNewsFeed_Author_Last_Name: string;
    FNewsFeed_DESCRIPTION: string;
    FNewsFeed_Feed_URL: string;
    FNewsFeed_Site_Name: string;
    FNewsFeed_Site_URL: string;
    FNewsFeed_Title: string;
    FNewsFeed_Keywords: string;
    FNewsFeed_Description_70: string;
    FNewsFeed_Description_250: string;

    // TStrings field for long description
    FNewsFeed_Description_250_Strings: TStrings;

    // Property getters/setters for TStrings
    function GetNewsFeed_Description_250_Strings: TStrings;
    procedure SetNewsFeed_Description_250_Strings(Value: TStrings);

    // Helper methods for NewsFeed_Type conversion
    function GetNewsFeed_TypeAsString: string;
    procedure SetNewsFeed_TypeAsString(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    // Synchronization methods
    procedure SyncStringsToStrings;
    procedure SyncStringToStrings;
  protected
    property NewsFeed_Description_250: string read FNewsFeed_Description_250 write FNewsFeed_Description_250;
    property NewsFeed_TypeAsString: string read GetNewsFeed_TypeAsString write SetNewsFeed_TypeAsString stored False;
  published
    // Simple string properties
    property NewsFeed_FORM: boolean read FNewsFeed_FORM write FNewsFeed_FORM;
    property NewsFeed_VERSION: string read FNewsFeed_VERSION write FNewsFeed_VERSION;
    property NewsFeed_URL: string read FNewsFeed_URL write FNewsFeed_URL;
    property NewsFeed_Language: string read FNewsFeed_Language write FNewsFeed_Language;
    property NewsFeed_Purpose: string read FNewsFeed_Purpose write FNewsFeed_Purpose;
    property NewsFeed_Author_Email: string read FNewsFeed_Author_Email write FNewsFeed_Author_Email;
    property NewsFeed_Author_First_Name: string read FNewsFeed_Author_First_Name write FNewsFeed_Author_First_Name;
    property NewsFeed_Author_Last_Name: string read FNewsFeed_Author_Last_Name write FNewsFeed_Author_Last_Name;
    property NewsFeed_DESCRIPTION: string read FNewsFeed_DESCRIPTION write FNewsFeed_DESCRIPTION;
    property NewsFeed_Feed_URL: string read FNewsFeed_Feed_URL write FNewsFeed_Feed_URL;
    property NewsFeed_Site_Name: string read FNewsFeed_Site_Name write FNewsFeed_Site_Name;
    property NewsFeed_Site_URL: string read FNewsFeed_Site_URL write FNewsFeed_Site_URL;
    property NewsFeed_Title: string read FNewsFeed_Title write FNewsFeed_Title;
    property NewsFeed_Keywords: string read FNewsFeed_Keywords write FNewsFeed_Keywords;
    property NewsFeed_Description_70: string read FNewsFeed_Description_70 write FNewsFeed_Description_70;

    // Enum property for NewsFeed_Type with string conversion
    property NewsFeed_Type: TPadNewsFeedType read FNewsFeed_Type write FNewsFeed_Type;

    // TStrings property for PropertyGrid (long description only)
    property NewsFeed_Description_250_Strings: TStrings read GetNewsFeed_Description_250_Strings
      write SetNewsFeed_Description_250_Strings stored False;
  end;

  { TPadFileInfo }
  TPadFileInfo = class(TPersistent)
  private
    FFileSizeBytes: cardinal;
    FFileSizeK: cardinal;
    FFileSizeMB: double;
  published
    property FileSizeBytes: cardinal read FFileSizeBytes write FFileSizeBytes;
    property FileSizeK: cardinal read FFileSizeK write FFileSizeK;
    property FileSizeMB: double read FFileSizeMB write FFileSizeMB;
  end;

  { TPadExpireInfo }
  TPadExpireInfo = class(TPersistent)
  private
    FHasExpireInfo: boolean;
    FExpireCount: integer;
    FExpireBasedOn: TPadExpireBasedOn;
    FExpireOtherInfo: string;
    FExpireMonth: integer;
    FExpireDay: integer;
    FExpireYear: integer;
    function GetExpireBasedOnAsString: string;
    procedure SetExpireBasedOnAsString(const Value: string);
  protected
    property ExpireBasedOnAsString: string read GetExpireBasedOnAsString write SetExpireBasedOnAsString;
  published
    property HasExpireInfo: boolean read FHasExpireInfo write FHasExpireInfo;
    property ExpireCount: integer read FExpireCount write FExpireCount;
    property ExpireBasedOn: TPadExpireBasedOn read FExpireBasedOn write FExpireBasedOn;
    property ExpireOtherInfo: string read FExpireOtherInfo write FExpireOtherInfo;
    property ExpireMonth: integer read FExpireMonth write FExpireMonth;
    property ExpireDay: integer read FExpireDay write FExpireDay;
    property ExpireYear: integer read FExpireYear write FExpireYear;
  end;

  { TPadProgramInfo }
  TPadProgramInfo = class(TPersistent)
  private
    FProgramName: string;
    FProgramVersion: string;
    FProgramReleaseMonth: byte;
    FProgramReleaseDay: byte;
    FProgramReleaseYear: word;
    FProgramCostDollars: string;
    FProgramCostOtherCode: string;
    FProgramCostOther: word;
    FProgramType: TPadProgramType;
    FProgramReleaseStatus: TPadReleaseStatus;
    FProgramInstallSupport: TPadInstallSupport;
    // OS Support groups
    FProgramOSSupportWindows: TPadOSWindowsSet;
    FProgramOSSupportUnixLinux: TPadOSUnixLinuxSet;
    FProgramOSSupportOther: TPadOSOtherSet;
    // Language groups
    FProgramLanguageEuropean: TPadLangEuropeanSet;
    FProgramLanguageAsian: TPadLangAsianSet;
    FProgramLanguageOtherMajor: TPadLangOtherMajorSet;
    FProgramLanguageWorld: TPadLangWorldSet;
    FProgramChangeInfo: string;
    FProgramSpecificCategory: string;
    FProgramCategoryClass: TPadProgramCategoryClass;
    FProgramSystemRequirements: string;
    FFileInfo: TPadFileInfo;
    FExpireInfo: TPadExpireInfo;

    function GetProgramTypeAsString: string;
    procedure SetProgramTypeAsString(const Value: string);
    function GetProgramReleaseStatusAsString: string;
    procedure SetProgramReleaseStatusAsString(const Value: string);
    function GetProgramInstallSupportAsString: string;
    procedure SetProgramInstallSupportAsString(const Value: string);
    function GetProgramCategoryClassAsString: string;
    procedure SetProgramCategoryClassAsString(const Value: string);
    function GetProgramOSSupportAsString: string;
    procedure SetProgramOSSupportAsString(const Value: string);
    function GetProgramLanguageAsString: string;
    procedure SetProgramLanguageAsString(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
  protected
    property ProgramTypeAsString: string read GetProgramTypeAsString write SetProgramTypeAsString;
    property ProgramReleaseStatusAsString: string read GetProgramReleaseStatusAsString write SetProgramReleaseStatusAsString;
    property ProgramInstallSupportAsString: string read GetProgramInstallSupportAsString write SetProgramInstallSupportAsString;
    // Combined OS Support property for XML
    property ProgramOSSupportAsString: string read GetProgramOSSupportAsString write SetProgramOSSupportAsString;
    // Combined Language property for XML
    property ProgramLanguageAsString: string read GetProgramLanguageAsString write SetProgramLanguageAsString;
    property ProgramCategoryClassAsString: string read GetProgramCategoryClassAsString write SetProgramCategoryClassAsString;
  published
    property ProgramName: string read FProgramName write FProgramName;
    property ProgramVersion: string read FProgramVersion write FProgramVersion;
    property ProgramReleaseMonth: byte read FProgramReleaseMonth write FProgramReleaseMonth;
    property ProgramReleaseDay: byte read FProgramReleaseDay write FProgramReleaseDay;
    property ProgramReleaseYear: word read FProgramReleaseYear write FProgramReleaseYear;
    property ProgramCostDollars: string read FProgramCostDollars write FProgramCostDollars;
    property ProgramCostOtherCode: string read FProgramCostOtherCode write FProgramCostOtherCode;
    property ProgramCostOther: word read FProgramCostOther write FProgramCostOther;
    property ProgramType: TPadProgramType read FProgramType write FProgramType;
    property ProgramReleaseStatus: TPadReleaseStatus read FProgramReleaseStatus write FProgramReleaseStatus;
    property ProgramInstallSupport: TPadInstallSupport read FProgramInstallSupport write FProgramInstallSupport;

    // OS Support properties for designer
    property ProgramOSSupportWindows: TPadOSWindowsSet read FProgramOSSupportWindows write FProgramOSSupportWindows;
    property ProgramOSSupportUnixLinux: TPadOSUnixLinuxSet read FProgramOSSupportUnixLinux write FProgramOSSupportUnixLinux;
    property ProgramOSSupportOther: TPadOSOtherSet read FProgramOSSupportOther write FProgramOSSupportOther;

    // Language properties for designer
    property ProgramLanguageEuropean: TPadLangEuropeanSet read FProgramLanguageEuropean write FProgramLanguageEuropean;
    property ProgramLanguageAsian: TPadLangAsianSet read FProgramLanguageAsian write FProgramLanguageAsian;
    property ProgramLanguageOtherMajor: TPadLangOtherMajorSet read FProgramLanguageOtherMajor write FProgramLanguageOtherMajor;
    property ProgramLanguageWorld: TPadLangWorldSet read FProgramLanguageWorld write FProgramLanguageWorld;

    property ProgramChangeInfo: string read FProgramChangeInfo write FProgramChangeInfo;
    property ProgramSpecificCategory: string read FProgramSpecificCategory write FProgramSpecificCategory;
    property ProgramCategoryClass: TPadProgramCategoryClass read FProgramCategoryClass write FProgramCategoryClass;
    property ProgramSystemRequirements: string read FProgramSystemRequirements write FProgramSystemRequirements;
    property FileInfo: TPadFileInfo read FFileInfo write FFileInfo;
    property ExpireInfo: TPadExpireInfo read FExpireInfo write FExpireInfo;
  end;

  { TPadEnglishDescription }
  TPadEnglishDescription = class(TPersistent)
  private
    // Original string fields for XML serialization
    FKeywords: string;
    FCharDesc45: string;
    FCharDesc80: string;
    FCharDesc250: string;
    FCharDesc450: string;
    FCharDesc2000: string;

    // TStrings fields for PropertyGrid (long descriptions only)
    FCharDesc250Strings: TStrings;
    FCharDesc450Strings: TStrings;
    FCharDesc2000Strings: TStrings;

    // Property getters/setters for TStrings
    function GetCharDesc250Strings: TStrings;
    procedure SetCharDesc250Strings(Value: TStrings);
    function GetCharDesc450Strings: TStrings;
    procedure SetCharDesc450Strings(Value: TStrings);
    function GetCharDesc2000Strings: TStrings;
    procedure SetCharDesc2000Strings(Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    // Helper methods for string<->TStrings conversion
    procedure SyncStringsToStrings; // Call after loading from XML
    procedure SyncStringToStrings;  // Call before saving to XML
  protected
    // Long descriptions - string properties for XML
    property CharDesc250: string read FCharDesc250 write FCharDesc250;
    property CharDesc450: string read FCharDesc450 write FCharDesc450;
    property CharDesc2000: string read FCharDesc2000 write FCharDesc2000;
  published
    // Short descriptions - keep as simple string properties
    property Keywords: string read FKeywords write FKeywords;
    property CharDesc45: string read FCharDesc45 write FCharDesc45;
    property CharDesc80: string read FCharDesc80 write FCharDesc80;

    // TStrings properties for PropertyGrid (long descriptions only)
    property CharDesc250Strings: TStrings read GetCharDesc250Strings write SetCharDesc250Strings stored False;
    property CharDesc450Strings: TStrings read GetCharDesc450Strings write SetCharDesc450Strings stored False;
    property CharDesc2000Strings: TStrings read GetCharDesc2000Strings write SetCharDesc2000Strings stored False;
  end;

  { TPadProgramDescriptions }
  TPadProgramDescriptions = class(TPersistent)
  private
    FEnglish: TPadEnglishDescription;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property English: TPadEnglishDescription read FEnglish write FEnglish;
  end;

  { TPadApplicationURLs }
  TPadApplicationURLs = class(TPersistent)
  private
    FApplicationInfoURL: string;
    FApplicationOrderURL: string;
    FApplicationScreenshotURL: string;
    FApplicationIconURL: string;
    FApplicationXMLFileURL: string;
  published
    property ApplicationInfoURL: string read FApplicationInfoURL write FApplicationInfoURL;
    property ApplicationOrderURL: string read FApplicationOrderURL write FApplicationOrderURL;
    property ApplicationScreenshotURL: string read FApplicationScreenshotURL write FApplicationScreenshotURL;
    property ApplicationIconURL: string read FApplicationIconURL write FApplicationIconURL;
    property ApplicationXMLFileURL: string read FApplicationXMLFileURL write FApplicationXMLFileURL;
  end;

  { TPadDownloadURLs }
  TPadDownloadURLs = class(TPersistent)
  private
    FPrimaryDownloadURL: string;
    FSecondaryDownloadURL: string;
    FAdditionalDownloadURL1: string;
    FAdditionalDownloadURL2: string;
  published
    property PrimaryDownloadURL: string read FPrimaryDownloadURL write FPrimaryDownloadURL;
    property SecondaryDownloadURL: string read FSecondaryDownloadURL write FSecondaryDownloadURL;
    property AdditionalDownloadURL1: string read FAdditionalDownloadURL1 write FAdditionalDownloadURL1;
    property AdditionalDownloadURL2: string read FAdditionalDownloadURL2 write FAdditionalDownloadURL2;
  end;

  { TPadWebInfo }
  TPadWebInfo = class(TPersistent)
  private
    FApplicationURLs: TPadApplicationURLs;
    FDownloadURLs: TPadDownloadURLs;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ApplicationURLs: TPadApplicationURLs read FApplicationURLs write FApplicationURLs;
    property DownloadURLs: TPadDownloadURLs read FDownloadURLs write FDownloadURLs;
  end;

  { TPadPermissions }
  TPadPermissions = class(TPersistent)
  private
    // Original string fields for XML serialization
    FDistributionPermissions: string;
    FEULA: string;

    // TStrings fields for PropertyGrid
    FDistributionPermissionsStrings: TStrings;
    FEULAStrings: TStrings;

    // Property getters/setters for TStrings
    function GetDistributionPermissionsStrings: TStrings;
    procedure SetDistributionPermissionsStrings(Value: TStrings);
    function GetEULAStrings: TStrings;
    procedure SetEULAStrings(Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    // Synchronization methods
    procedure SyncStringsToStrings;
    procedure SyncStringToStrings;
  protected
    // String properties for XML
    property DistributionPermissions: string read FDistributionPermissions write FDistributionPermissions;
    property EULA: string read FEULA write FEULA;
  published
    // TStrings properties for PropertyGrid
    property DistributionPermissionsStrings: TStrings read GetDistributionPermissionsStrings
      write SetDistributionPermissionsStrings stored False;
    property EULAStrings: TStrings read GetEULAStrings write SetEULAStrings stored False;
  end;

  { TPadAffiliateCompany }
  TPadAffiliateCompany = class(TPersistent)
  private
    FOrderPage: string;
    FVendorID: string;
    FProductID: string;
    FMaximumCommissionRate: string;
  published
    property OrderPage: string read FOrderPage write FOrderPage;
    property VendorID: string read FVendorID write FVendorID;
    property ProductID: string read FProductID write FProductID;
    property MaximumCommissionRate: string read FMaximumCommissionRate write FMaximumCommissionRate;
  end;

  { TPadAffiliates }
  TPadAffiliates = class(TPersistent)
  private
    FAffiliates_FORM: boolean;
    FAffiliates_VERSION: string;
    FAffiliates_URL: string;
    FAffiliates_Information_Page: string;

    // Individual affiliate companies
    FAvangate: TPadAffiliateCompany;
    FBMTMicro: TPadAffiliateCompany;
    FCleverbridge: TPadAffiliateCompany;  // New in version 1.4
    FClixGalore: TPadAffiliateCompany;
    FCommissionJunction: TPadAffiliateCompany;
    FDigiBuy: TPadAffiliateCompany;
    FDigitalCandle: TPadAffiliateCompany;
    FEmetrix: TPadAffiliateCompany;
    FeSellerate: TPadAffiliateCompany;
    FKagi: TPadAffiliateCompany;
    FLinkShare: TPadAffiliateCompany;
    FNorthStarSol: TPadAffiliateCompany;
    FOneNetworkDirect: TPadAffiliateCompany;  // New in version 1.4
    FOrder1: TPadAffiliateCompany;
    FOsolis: TPadAffiliateCompany;
    FPlimus: TPadAffiliateCompany;
    FRegnet: TPadAffiliateCompany;
    FRegnow: TPadAffiliateCompany;
    FRegsoft: TPadAffiliateCompany;
    FShareIt: TPadAffiliateCompany;
    FShareasale: TPadAffiliateCompany;
    FSWReg: TPadAffiliateCompany;
    FVShare: TPadAffiliateCompany;
    FVFree: TPadAffiliateCompany;
    FYaskifo: TPadAffiliateCompany;

    // Helper to check if we should save full section
    function ShouldSaveFullSection: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  published
    // Main properties
    property Affiliates_FORM: boolean read FAffiliates_FORM write FAffiliates_FORM;
    property Affiliates_VERSION: string read FAffiliates_VERSION write FAffiliates_VERSION;
    property Affiliates_URL: string read FAffiliates_URL write FAffiliates_URL;
    property Affiliates_Information_Page: string read FAffiliates_Information_Page write FAffiliates_Information_Page;

    // Affiliate companies
    property Avangate: TPadAffiliateCompany read FAvangate write FAvangate;
    property BMTMicro: TPadAffiliateCompany read FBMTMicro write FBMTMicro;
    property Cleverbridge: TPadAffiliateCompany read FCleverbridge write FCleverbridge;
    property ClixGalore: TPadAffiliateCompany read FClixGalore write FClixGalore;
    property CommissionJunction: TPadAffiliateCompany read FCommissionJunction write FCommissionJunction;
    property DigiBuy: TPadAffiliateCompany read FDigiBuy write FDigiBuy;
    property DigitalCandle: TPadAffiliateCompany read FDigitalCandle write FDigitalCandle;
    property Emetrix: TPadAffiliateCompany read FEmetrix write FEmetrix;
    property eSellerate: TPadAffiliateCompany read FeSellerate write FeSellerate;
    property Kagi: TPadAffiliateCompany read FKagi write FKagi;
    property LinkShare: TPadAffiliateCompany read FLinkShare write FLinkShare;
    property NorthStarSol: TPadAffiliateCompany read FNorthStarSol write FNorthStarSol;
    property OneNetworkDirect: TPadAffiliateCompany read FOneNetworkDirect write FOneNetworkDirect;
    property Order1: TPadAffiliateCompany read FOrder1 write FOrder1;
    property Osolis: TPadAffiliateCompany read FOsolis write FOsolis;
    property Plimus: TPadAffiliateCompany read FPlimus write FPlimus;
    property Regnet: TPadAffiliateCompany read FRegnet write FRegnet;
    property Regnow: TPadAffiliateCompany read FRegnow write FRegnow;
    property Regsoft: TPadAffiliateCompany read FRegsoft write FRegsoft;
    property ShareIt: TPadAffiliateCompany read FShareIt write FShareIt;
    property Shareasale: TPadAffiliateCompany read FShareasale write FShareasale;
    property SWReg: TPadAffiliateCompany read FSWReg write FSWReg;
    property VShare: TPadAffiliateCompany read FVShare write FVShare;
    property VFree: TPadAffiliateCompany read FVFree write FVFree;
    property Yaskifo: TPadAffiliateCompany read FYaskifo write FYaskifo;
  end;

  { TPadASP }
  TPadASP = class(TPersistent)
  private
    FASPForm: boolean;
    FASPMember: boolean;
    FASPMemberNumber: word;
  published
    property ASPForm: boolean read FASPForm write FASPForm;
    property ASPMember: boolean read FASPMember write FASPMember;
    property ASPMemberNumber: word read FASPMemberNumber write FASPMemberNumber;
  end;

  { TPadFormat }
  TPadFormat = class(TComponent)
  private
    FMasterPadVersionInfo: TPadMasterVersionInfo;
    FCompanyInfo: TPadCompanyInfo;
    FNewsFeed: TPadNewsFeed;
    FProgramInfo: TPadProgramInfo;
    FProgramDescriptions: TPadProgramDescriptions;
    FWebInfo: TPadWebInfo;
    FPermissions: TPadPermissions;
    FAffiliates: TPadAffiliates;
    FASP: TPadASP;
    function SetNodeText(Doc: TXMLDocument; ParentNode: TDOMNode; NodeName, NodeValue: string): TDOMNode;
    function AddChildNode(ParentNode: TDOMNode; NodeName: string): TDOMNode;
    procedure SetNodeTextValue(Node: TDOMNode; Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Method to load properties from XML
    procedure LoadFromXML(const XMLContent: string);

    // Method to save properties to XML
    function SaveToXML: string;

    // Method to clear all properties
    procedure Clear;
  published
    property MasterPadVersionInfo: TPadMasterVersionInfo read FMasterPadVersionInfo write FMasterPadVersionInfo;
    property CompanyInfo: TPadCompanyInfo read FCompanyInfo write FCompanyInfo;
    property NewsFeed: TPadNewsFeed read FNewsFeed write FNewsFeed;
    property ProgramInfo: TPadProgramInfo read FProgramInfo write FProgramInfo;
    property ProgramDescriptions: TPadProgramDescriptions read FProgramDescriptions write FProgramDescriptions;
    property WebInfo: TPadWebInfo read FWebInfo write FWebInfo;
    property Permissions: TPadPermissions read FPermissions write FPermissions;
    property Affiliates: TPadAffiliates read FAffiliates write FAffiliates;
    property ASP: TPadASP read FASP write FASP;
  end;

// Helper functions for conversions
function GetNodeText(Node: TDOMNode): string;

function StrToInt64Safe(const S: string): int64;
function StrToFloatSafe(const S: string): double;

function PadInstallSupportToString(Value: TPadInstallSupport): string;
function StringToPadInstallSupport(const Value: string): TPadInstallSupport;

function PadProgramTypeToString(Value: TPadProgramType): string;
function StringToPadProgramType(const Value: string): TPadProgramType;

function PadReleaseStatusToString(Value: TPadReleaseStatus): string;
function StringToPadReleaseStatus(const Value: string): TPadReleaseStatus;

function PadExpireBasedOnToString(Value: TPadExpireBasedOn): string;
function StringToPadExpireBasedOn(const Value: string): TPadExpireBasedOn;

function PadProgramCategoryClassToString(Value: TPadProgramCategoryClass): string;
function StringToPadProgramCategoryClass(const Value: string): TPadProgramCategoryClass;
function IsValidPadProgramCategoryClassString(const Value: string): boolean;
function GetPadProgramCategoryClassDisplayName(Value: TPadProgramCategoryClass): string;
function GetPadProgramCategoryClassMainCategory(Value: TPadProgramCategoryClass): string;
function GetPadProgramCategoryClassSubCategory(Value: TPadProgramCategoryClass): string;

// OS Support conversion functions
function PadOSWindowsToString(Value: TPadOSWindows): string;
function StringToPadOSWindows(const Value: string): TPadOSWindows;
function PadOSUnixLinuxToString(Value: TPadOSUnixLinux): string;
function StringToPadOSUnixLinux(const Value: string): TPadOSUnixLinux;
function PadOSOtherToString(Value: TPadOSOther): string;
function StringToPadOSOther(const Value: string): TPadOSOther;

// Combined OS Support conversion
function PadOSSupportToString(WindowsSet: TPadOSWindowsSet; UnixLinuxSet: TPadOSUnixLinuxSet; OtherSet: TPadOSOtherSet): string;
procedure StringToPadOSSupport(const Value: string; out WindowsSet: TPadOSWindowsSet; out UnixLinuxSet: TPadOSUnixLinuxSet;
  out OtherSet: TPadOSOtherSet);

// Language conversion functions
function PadLangEuropeanToString(Value: TPadLangEuropean): string;
function StringToPadLangEuropean(const Value: string): TPadLangEuropean;
function PadLangAsianToString(Value: TPadLangAsian): string;
function StringToPadLangAsian(const Value: string): TPadLangAsian;
function PadLangOtherMajorToString(Value: TPadLangOtherMajor): string;
function StringToPadLangOtherMajor(const Value: string): TPadLangOtherMajor;
function PadLangWorldToString(Value: TPadLangWorld): string;
function StringToPadLangWorld(const Value: string): TPadLangWorld;

// Combined Language conversion
function PadLanguagesToString(EuropeanSet: TPadLangEuropeanSet; AsianSet: TPadLangAsianSet;
  OtherMajorSet: TPadLangOtherMajorSet; WorldSet: TPadLangWorldSet): string;
procedure StringToPadLanguages(const Value: string; out EuropeanSet: TPadLangEuropeanSet; out AsianSet: TPadLangAsianSet;
  out OtherMajorSet: TPadLangOtherMajorSet; out WorldSet: TPadLangWorldSet);

procedure Register;

implementation

{ TPadMasterVersionInfo }

procedure TPadMasterVersionInfo.SetMasterPadVersion(Value: string);
begin
  FMasterPadVersion := Value;
  FVersion := StrToFloatSafe(FMasterPadVersion);
  if FVersion <= 0 then FVersion := 4;
end;

{ TPadNewsFeed }

constructor TPadNewsFeed.Create;
begin
  inherited Create;
  FNewsFeed_Description_250_Strings := TStringList.Create;
  FNewsFeed_Description_250_Strings.TrailingLineBreak := False;
  FNewsFeed_FORM := False;
  FNewsFeed_VERSION := '1.0';
  FNewsFeed_URL := 'http://Submit-Everywhere.com/extensions/NewsFeed.htm';
  FNewsFeed_Type := pnftRSS090; // Default value
end;

destructor TPadNewsFeed.Destroy;
begin
  FNewsFeed_Description_250_Strings.Free;
  inherited Destroy;
end;

function TPadNewsFeed.GetNewsFeed_Description_250_Strings: TStrings;
begin
  Result := FNewsFeed_Description_250_Strings;
end;

procedure TPadNewsFeed.SetNewsFeed_Description_250_Strings(Value: TStrings);
begin
  if Assigned(Value) then
    FNewsFeed_Description_250_Strings.Assign(Value)
  else
    FNewsFeed_Description_250_Strings.Clear;
end;

function TPadNewsFeed.GetNewsFeed_TypeAsString: string;
begin
  Result := PadNewsFeedTypeStrings[FNewsFeed_Type];
end;

procedure TPadNewsFeed.SetNewsFeed_TypeAsString(const Value: string);
var
  i: TPadNewsFeedType;
begin
  // Search for matching string in the array
  for i := Low(TPadNewsFeedType) to High(TPadNewsFeedType) do
  begin
    if SameText(PadNewsFeedTypeStrings[i], Value) then
    begin
      FNewsFeed_Type := i;
      Exit;
    end;
  end;

  // If not found, default to pnftRSS090
  FNewsFeed_Type := pnftRSS090;
end;

procedure TPadNewsFeed.SyncStringsToStrings;
begin
  FNewsFeed_Description_250_Strings.Text := FNewsFeed_Description_250;
end;

procedure TPadNewsFeed.SyncStringToStrings;
begin
  FNewsFeed_Description_250 := FNewsFeed_Description_250_Strings.Text;
end;

// Helper function for NewsFeed type conversion
function PadNewsFeedTypeToString(Value: TPadNewsFeedType): string;
begin
  if (Value >= Low(TPadNewsFeedType)) and (Value <= High(TPadNewsFeedType)) then
    Result := PadNewsFeedTypeStrings[Value]
  else
    Result := '';
end;

function StringToPadNewsFeedType(const Value: string): TPadNewsFeedType;
var
  NewsFeedType: TPadNewsFeedType;
begin
  for NewsFeedType := Low(TPadNewsFeedType) to High(TPadNewsFeedType) do
  begin
    if SameText(PadNewsFeedTypeStrings[NewsFeedType], Value) then
    begin
      Result := NewsFeedType;
      Exit;
    end;
  end;

  // If not found, return default
  Result := pnftRSS090;
end;

{ TPadExpireInfo }

function TPadExpireInfo.GetExpireBasedOnAsString: string;
begin
  Result := PadExpireBasedOnToString(FExpireBasedOn);
end;

procedure TPadExpireInfo.SetExpireBasedOnAsString(const Value: string);
begin
  FExpireBasedOn := StringToPadExpireBasedOn(Value);
end;

{ TPadProgramInfo }

constructor TPadProgramInfo.Create;
begin
  inherited Create;
  FFileInfo := TPadFileInfo.Create;
  FExpireInfo := TPadExpireInfo.Create;
  // Initialize OS sets
  FProgramOSSupportWindows := [];
  FProgramOSSupportUnixLinux := [];
  FProgramOSSupportOther := [];
  // Initialize language sets
  FProgramLanguageEuropean := [];
  FProgramLanguageAsian := [];
  FProgramLanguageOtherMajor := [];
  FProgramLanguageWorld := [];
end;

destructor TPadProgramInfo.Destroy;
begin
  FFileInfo.Free;
  FExpireInfo.Free;
  inherited Destroy;
end;

function TPadProgramInfo.GetProgramTypeAsString: string;
begin
  Result := PadProgramTypeToString(FProgramType);
end;

procedure TPadProgramInfo.SetProgramTypeAsString(const Value: string);
begin
  FProgramType := StringToPadProgramType(Value);
end;

function TPadProgramInfo.GetProgramReleaseStatusAsString: string;
begin
  Result := PadReleaseStatusToString(FProgramReleaseStatus);
end;

procedure TPadProgramInfo.SetProgramReleaseStatusAsString(const Value: string);
begin
  FProgramReleaseStatus := StringToPadReleaseStatus(Value);
end;

function TPadProgramInfo.GetProgramInstallSupportAsString: string;
begin
  Result := PadInstallSupportToString(FProgramInstallSupport);
end;

procedure TPadProgramInfo.SetProgramInstallSupportAsString(const Value: string);
begin
  FProgramInstallSupport := StringToPadInstallSupport(Value);
end;

function TPadProgramInfo.GetProgramCategoryClassAsString: string;
begin
  Result := PadProgramCategoryClassToString(FProgramCategoryClass);
end;

procedure TPadProgramInfo.SetProgramCategoryClassAsString(const Value: string);
begin
  FProgramCategoryClass := StringToPadProgramCategoryClass(Value);
end;

function TPadProgramInfo.GetProgramOSSupportAsString: string;
begin
  Result := PadOSSupportToString(FProgramOSSupportWindows, FProgramOSSupportUnixLinux, FProgramOSSupportOther);
end;

procedure TPadProgramInfo.SetProgramOSSupportAsString(const Value: string);
begin
  StringToPadOSSupport(Value, FProgramOSSupportWindows, FProgramOSSupportUnixLinux, FProgramOSSupportOther);
end;

function TPadProgramInfo.GetProgramLanguageAsString: string;
begin
  Result := PadLanguagesToString(FProgramLanguageEuropean, FProgramLanguageAsian, FProgramLanguageOtherMajor, FProgramLanguageWorld);
end;

procedure TPadProgramInfo.SetProgramLanguageAsString(const Value: string);
begin
  StringToPadLanguages(Value, FProgramLanguageEuropean, FProgramLanguageAsian,
    FProgramLanguageOtherMajor, FProgramLanguageWorld);
end;

{ TPadCompanyInfo }

constructor TPadCompanyInfo.Create;
begin
  inherited Create;
  FContactInfo := TPadContactInfo.Create;
  FSupportInfo := TPadSupportInfo.Create;
end;

destructor TPadCompanyInfo.Destroy;
begin
  FContactInfo.Free;
  FSupportInfo.Free;
  inherited Destroy;
end;

{ TPadEnglishDescription }

constructor TPadEnglishDescription.Create;
begin
  inherited Create;
  // Initialize TStrings objects for long descriptions only
  FCharDesc250Strings := TStringList.Create;
  FCharDesc250Strings.TrailingLineBreak := False;
  FCharDesc450Strings := TStringList.Create;
  FCharDesc450Strings.TrailingLineBreak := False;
  FCharDesc2000Strings := TStringList.Create;
  FCharDesc2000Strings.TrailingLineBreak := False;
end;

destructor TPadEnglishDescription.Destroy;
begin
  FCharDesc250Strings.Free;
  FCharDesc450Strings.Free;
  FCharDesc2000Strings.Free;
  inherited Destroy;
end;

function TPadEnglishDescription.GetCharDesc250Strings: TStrings;
begin
  // Always return the TStrings object
  Result := FCharDesc250Strings;
end;

procedure TPadEnglishDescription.SetCharDesc250Strings(Value: TStrings);
begin
  if Assigned(Value) then
    FCharDesc250Strings.Assign(Value)
  else
    FCharDesc250Strings.Clear;
end;

function TPadEnglishDescription.GetCharDesc450Strings: TStrings;
begin
  Result := FCharDesc450Strings;
end;

procedure TPadEnglishDescription.SetCharDesc450Strings(Value: TStrings);
begin
  if Assigned(Value) then
    FCharDesc450Strings.Assign(Value)
  else
    FCharDesc450Strings.Clear;
end;

function TPadEnglishDescription.GetCharDesc2000Strings: TStrings;
begin
  Result := FCharDesc2000Strings;
end;

procedure TPadEnglishDescription.SetCharDesc2000Strings(Value: TStrings);
begin
  if Assigned(Value) then
    FCharDesc2000Strings.Assign(Value)
  else
    FCharDesc2000Strings.Clear;
end;

// Call this after loading from XML to populate TStrings from string fields
procedure TPadEnglishDescription.SyncStringsToStrings;
begin
  FCharDesc250Strings.Text := FCharDesc250;
  FCharDesc450Strings.Text := FCharDesc450;
  FCharDesc2000Strings.Text := FCharDesc2000;
end;

// Call this before saving to XML to update string fields from TStrings
procedure TPadEnglishDescription.SyncStringToStrings;
begin
  FCharDesc250 := FCharDesc250Strings.Text;
  FCharDesc450 := FCharDesc450Strings.Text;
  FCharDesc2000 := FCharDesc2000Strings.Text;
end;

{ TPadProgramDescriptions }

constructor TPadProgramDescriptions.Create;
begin
  inherited Create;
  FEnglish := TPadEnglishDescription.Create;
end;

destructor TPadProgramDescriptions.Destroy;
begin
  FEnglish.Free;
  inherited Destroy;
end;

{ TPadWebInfo }

constructor TPadWebInfo.Create;
begin
  inherited Create;
  FApplicationURLs := TPadApplicationURLs.Create;
  FDownloadURLs := TPadDownloadURLs.Create;
end;

destructor TPadWebInfo.Destroy;
begin
  FApplicationURLs.Free;
  FDownloadURLs.Free;
  inherited Destroy;
end;

{ TPadPermissions }

constructor TPadPermissions.Create;
begin
  inherited Create;
  FDistributionPermissionsStrings := TStringList.Create;
  FDistributionPermissionsStrings.TrailingLineBreak := False;
  FEULAStrings := TStringList.Create;
  FEULAStrings.TrailingLineBreak := False;
end;

destructor TPadPermissions.Destroy;
begin
  FDistributionPermissionsStrings.Free;
  FEULAStrings.Free;
  inherited Destroy;
end;

function TPadPermissions.GetDistributionPermissionsStrings: TStrings;
begin
  Result := FDistributionPermissionsStrings;
end;

procedure TPadPermissions.SetDistributionPermissionsStrings(Value: TStrings);
begin
  if Assigned(Value) then
    FDistributionPermissionsStrings.Assign(Value)
  else
    FDistributionPermissionsStrings.Clear;
end;

function TPadPermissions.GetEULAStrings: TStrings;
begin
  Result := FEULAStrings;
end;

procedure TPadPermissions.SetEULAStrings(Value: TStrings);
begin
  if Assigned(Value) then
    FEULAStrings.Assign(Value)
  else
    FEULAStrings.Clear;
end;

procedure TPadPermissions.SyncStringsToStrings;
begin
  FDistributionPermissionsStrings.Text := FDistributionPermissions;
  FEULAStrings.Text := FEULA;
end;

procedure TPadPermissions.SyncStringToStrings;
begin
  FDistributionPermissions := FDistributionPermissionsStrings.Text;
  FEULA := FEULAStrings.Text;
end;

{ TPadAffiliates }

constructor TPadAffiliates.Create;
begin
  inherited Create;

  // Initialize default values - version will be set based on master version
  FAffiliates_FORM := True;
  FAffiliates_VERSION := '1.2'; // Default, will be updated based on master version
  FAffiliates_URL := 'http://www.asp-shareware.org/pad/extensions/Affiliates.htm';

  // Create affiliate company objects
  FAvangate := TPadAffiliateCompany.Create;
  FBMTMicro := TPadAffiliateCompany.Create;
  FCleverbridge := TPadAffiliateCompany.Create;
  FClixGalore := TPadAffiliateCompany.Create;
  FCommissionJunction := TPadAffiliateCompany.Create;
  FDigiBuy := TPadAffiliateCompany.Create;
  FDigitalCandle := TPadAffiliateCompany.Create;
  FEmetrix := TPadAffiliateCompany.Create;
  FeSellerate := TPadAffiliateCompany.Create;
  FKagi := TPadAffiliateCompany.Create;
  FLinkShare := TPadAffiliateCompany.Create;
  FNorthStarSol := TPadAffiliateCompany.Create;
  FOneNetworkDirect := TPadAffiliateCompany.Create;
  FOrder1 := TPadAffiliateCompany.Create;
  FOsolis := TPadAffiliateCompany.Create;
  FPlimus := TPadAffiliateCompany.Create;
  FRegnet := TPadAffiliateCompany.Create;
  FRegnow := TPadAffiliateCompany.Create;
  FRegsoft := TPadAffiliateCompany.Create;
  FShareIt := TPadAffiliateCompany.Create;
  FShareasale := TPadAffiliateCompany.Create;
  FSWReg := TPadAffiliateCompany.Create;
  FVShare := TPadAffiliateCompany.Create;
  FVFree := TPadAffiliateCompany.Create;
  FYaskifo := TPadAffiliateCompany.Create;
end;

destructor TPadAffiliates.Destroy;
begin
  // Free all affiliate company objects
  FAvangate.Free;
  FBMTMicro.Free;
  FCleverbridge.Free;
  FClixGalore.Free;
  FCommissionJunction.Free;
  FDigiBuy.Free;
  FDigitalCandle.Free;
  FEmetrix.Free;
  FeSellerate.Free;
  FKagi.Free;
  FLinkShare.Free;
  FNorthStarSol.Free;
  FOneNetworkDirect.Free;
  FOrder1.Free;
  FOsolis.Free;
  FPlimus.Free;
  FRegnet.Free;
  FRegnow.Free;
  FRegsoft.Free;
  FShareIt.Free;
  FShareasale.Free;
  FSWReg.Free;
  FVShare.Free;
  FVFree.Free;
  FYaskifo.Free;

  inherited Destroy;
end;

function TPadAffiliates.ShouldSaveFullSection: boolean;
begin
  // Check if Affiliates_Information_Page is filled
  // If it's empty, we need to save full section with all companies
  // If it's filled, we only save the header section
  Result := Trim(FAffiliates_Information_Page) = '';
end;

{ TPadFormat }

constructor TPadFormat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMasterPadVersionInfo := TPadMasterVersionInfo.Create;
  FCompanyInfo := TPadCompanyInfo.Create;
  FNewsFeed := TPadNewsFeed.Create;
  FProgramInfo := TPadProgramInfo.Create;
  FProgramDescriptions := TPadProgramDescriptions.Create;
  FWebInfo := TPadWebInfo.Create;
  FPermissions := TPadPermissions.Create;
  FAffiliates := TPadAffiliates.Create;
  FASP := TPadASP.Create;
end;

destructor TPadFormat.Destroy;
begin
  FMasterPadVersionInfo.Free;
  FCompanyInfo.Free;
  FNewsFeed.Free;
  FProgramInfo.Free;
  FProgramDescriptions.Free;
  FWebInfo.Free;
  FPermissions.Free;
  FAffiliates.Free;
  FASP.Free;
  inherited Destroy;
end;

procedure TPadFormat.LoadFromXML(const XMLContent: string);
var
  Doc: TXMLDocument;
  Stream: TStringStream;
  RootNode, Node, SubNode: TDOMNode;

  function GetNodeValue(Parent: TDOMNode; const NodeName: string): string;
  var
    TempNode: TDOMNode;
  begin
    TempNode := Parent.FindNode(DOMString(NodeName));
    if Assigned(TempNode) then
      Result := GetNodeText(TempNode)
    else
      Result := '';
  end;

  procedure LoadAffiliateCompany(Affiliate: TPadAffiliateCompany;
  const OrderPageTag, VendorIDTag, ProductIDTag, CommissionRateTag: string);
  begin
    if Assigned(Node) then
    begin
      Affiliate.OrderPage := GetNodeValue(Node, OrderPageTag);
      Affiliate.VendorID := GetNodeValue(Node, VendorIDTag);
      Affiliate.ProductID := GetNodeValue(Node, ProductIDTag);
      Affiliate.MaximumCommissionRate := GetNodeValue(Node, CommissionRateTag);
    end;
  end;

begin
  if XMLContent = '' then
    Exit;

  Stream := TStringStream.Create(XMLContent);
  try
    ReadXMLFile(Doc, Stream);
    try
      RootNode := Doc.DocumentElement;

      // Load Master Pad Version Info
      Node := RootNode.FindNode('MASTER_PAD_VERSION_INFO');
      if Assigned(Node) then
      begin
        FMasterPadVersionInfo.MasterPadVersion :=
          GetNodeValue(Node, 'MASTER_PAD_VERSION');
        FMasterPadVersionInfo.MasterPadEditor :=
          GetNodeValue(Node, 'MASTER_PAD_EDITOR');
        FMasterPadVersionInfo.MasterPadEditorUrl :=
          GetNodeValue(Node, 'MASTER_PAD_EDITOR_URL');
        FMasterPadVersionInfo.MasterPadInfo :=
          GetNodeValue(Node, 'MASTER_PAD_INFO');
      end;

      // Load Company Info
      Node := RootNode.FindNode('Company_Info');
      if Assigned(Node) then
      begin
        FCompanyInfo.CompanyName := GetNodeValue(Node, 'Company_Name');
        FCompanyInfo.Address1 := GetNodeValue(Node, 'Address_1');
        FCompanyInfo.Address2 := GetNodeValue(Node, 'Address_2');
        FCompanyInfo.CityTown := GetNodeValue(Node, 'City_Town');
        FCompanyInfo.StateProvince := GetNodeValue(Node, 'State_Province');
        FCompanyInfo.ZipPostalCode := GetNodeValue(Node, 'Zip_Postal_Code');
        FCompanyInfo.Country := GetNodeValue(Node, 'Country');
        FCompanyInfo.CompanyWebsiteURL := GetNodeValue(Node, 'Company_WebSite_URL');

        // Load Contact Info
        SubNode := Node.FindNode('Contact_Info');
        if Assigned(SubNode) then
        begin
          FCompanyInfo.ContactInfo.AuthorFirstName := GetNodeValue(SubNode, 'Author_First_Name');
          FCompanyInfo.ContactInfo.AuthorLastName := GetNodeValue(SubNode, 'Author_Last_Name');
          FCompanyInfo.ContactInfo.AuthorEmail := GetNodeValue(SubNode, 'Author_Email');
          FCompanyInfo.ContactInfo.ContactFirstName := GetNodeValue(SubNode, 'Contact_First_Name');
          FCompanyInfo.ContactInfo.ContactLastName := GetNodeValue(SubNode, 'Contact_Last_Name');
          FCompanyInfo.ContactInfo.ContactEmail := GetNodeValue(SubNode, 'Contact_Email');
        end;

        // Load Support Info
        SubNode := Node.FindNode('Support_Info');
        if Assigned(SubNode) then
        begin
          FCompanyInfo.SupportInfo.SalesEmail := GetNodeValue(SubNode, 'Sales_Email');
          FCompanyInfo.SupportInfo.SupportEmail := GetNodeValue(SubNode, 'Support_Email');
          FCompanyInfo.SupportInfo.GeneralEmail := GetNodeValue(SubNode, 'General_Email');
          FCompanyInfo.SupportInfo.SalesPhone := GetNodeValue(SubNode, 'Sales_Phone');
          FCompanyInfo.SupportInfo.SupportPhone := GetNodeValue(SubNode, 'Support_Phone');
          FCompanyInfo.SupportInfo.GeneralPhone := GetNodeValue(SubNode, 'General_Phone');
          FCompanyInfo.SupportInfo.FaxPhone := GetNodeValue(SubNode, 'Fax_Phone');
        end;

        // Social media pages
        FCompanyInfo.GooglePlusPage := GetNodeValue(Node, 'GooglePlusPage');
        FCompanyInfo.LinkedinPage := GetNodeValue(Node, 'LinkedinPage');
        FCompanyInfo.TwitterCompanyPage := GetNodeValue(Node, 'TwitterCompanyPage');
        FCompanyInfo.FacebookCompanyPage := GetNodeValue(Node, 'FacebookCompanyPage');
        FCompanyInfo.CompanyStorePage := GetNodeValue(Node, 'CompanyStorePage');
      end;

      // Load News Feed (updated with new fields)
      Node := RootNode.FindNode('NewsFeed');
      if Assigned(Node) then
      begin
        FNewsFeed.NewsFeed_FORM := UpperCase(GetNodeValue(Node, 'NewsFeed_FORM')) = 'Y';
        FNewsFeed.NewsFeed_VERSION := GetNodeValue(Node, 'NewsFeed_VERSION');
        FNewsFeed.NewsFeed_URL := GetNodeValue(Node, 'NewsFeed_URL');
        FNewsFeed.NewsFeed_TypeAsString := GetNodeValue(Node, 'NewsFeed_Type');
        FNewsFeed.NewsFeed_Language := GetNodeValue(Node, 'NewsFeed_Language');
        FNewsFeed.NewsFeed_Purpose := GetNodeValue(Node, 'NewsFeed_Purpose');
        FNewsFeed.NewsFeed_Author_Email := GetNodeValue(Node, 'NewsFeed_Author_Email');
        FNewsFeed.NewsFeed_Author_First_Name := GetNodeValue(Node, 'NewsFeed_Author_First_Name');
        FNewsFeed.NewsFeed_Author_Last_Name := GetNodeValue(Node, 'NewsFeed_Author_Last_Name');
        FNewsFeed.NewsFeed_DESCRIPTION := GetNodeValue(Node, 'NewsFeed_DESCRIPTION');
        FNewsFeed.NewsFeed_Feed_URL := GetNodeValue(Node, 'NewsFeed_Feed_URL');
        FNewsFeed.NewsFeed_Site_Name := GetNodeValue(Node, 'NewsFeed_Site_Name');
        FNewsFeed.NewsFeed_Site_URL := GetNodeValue(Node, 'NewsFeed_Site_URL');
        FNewsFeed.NewsFeed_Title := GetNodeValue(Node, 'NewsFeed_Title');
        FNewsFeed.NewsFeed_Keywords := GetNodeValue(Node, 'NewsFeed_Keywords');
        FNewsFeed.NewsFeed_Description_70 := GetNodeValue(Node, 'NewsFeed_Description_70');
        FNewsFeed.NewsFeed_Description_250 := GetNodeValue(Node, 'NewsFeed_Description_250');
      end;

      // Load Program Info
      Node := RootNode.FindNode('Program_Info');
      if Assigned(Node) then
      begin
        FProgramInfo.ProgramName := GetNodeValue(Node, 'Program_Name');
        FProgramInfo.ProgramVersion := GetNodeValue(Node, 'Program_Version');
        FProgramInfo.ProgramReleaseMonth := StrToIntDef(GetNodeValue(Node, 'Program_Release_Month'), 0);
        FProgramInfo.ProgramReleaseDay := StrToIntDef(GetNodeValue(Node, 'Program_Release_Day'), 0);
        FProgramInfo.ProgramReleaseYear := StrToIntDef(GetNodeValue(Node, 'Program_Release_Year'), 0);
        FProgramInfo.ProgramCostDollars := GetNodeValue(Node, 'Program_Cost_Dollars');
        FProgramInfo.ProgramCostOtherCode := GetNodeValue(Node, 'Program_Cost_Other_Code');
        FProgramInfo.ProgramCostOther := StrToIntDef(GetNodeValue(Node, 'Program_Cost_Other'), 0);

        // Use string properties that will handle conversion
        FProgramInfo.ProgramTypeAsString := GetNodeValue(Node, 'Program_Type');
        FProgramInfo.ProgramReleaseStatusAsString := GetNodeValue(Node, 'Program_Release_Status');
        FProgramInfo.ProgramInstallSupportAsString := GetNodeValue(Node, 'Program_Install_Support');

        // Load OS Support from string
        FProgramInfo.ProgramOSSupportAsString := GetNodeValue(Node, 'Program_OS_Support');

        // Load Language from string
        FProgramInfo.ProgramLanguageAsString := GetNodeValue(Node, 'Program_Language');

        FProgramInfo.ProgramChangeInfo := GetNodeValue(Node, 'Program_Change_Info');
        FProgramInfo.ProgramSpecificCategory := GetNodeValue(Node, 'Program_Specific_Category');
        FProgramInfo.ProgramCategoryClassAsString := GetNodeValue(Node, 'Program_Category_Class');
        FProgramInfo.ProgramSystemRequirements := GetNodeValue(Node, 'Program_System_Requirements');

        // Load File Info
        SubNode := Node.FindNode('File_Info');
        if Assigned(SubNode) then
        begin
          FProgramInfo.FileInfo.FileSizeBytes := StrToInt64Safe(GetNodeValue(SubNode, 'File_Size_Bytes'));
          FProgramInfo.FileInfo.FileSizeK := StrToInt64Safe(GetNodeValue(SubNode, 'File_Size_K'));
          FProgramInfo.FileInfo.FileSizeMB := StrToFloatSafe(GetNodeValue(SubNode, 'File_Size_MB'));
        end;

        // Load Expire Info
        SubNode := Node.FindNode('Expire_Info');
        if Assigned(SubNode) then
        begin
          FProgramInfo.ExpireInfo.HasExpireInfo := UpperCase(GetNodeValue(SubNode, 'Has_Expire_Info')) = 'Y';
          FProgramInfo.ExpireInfo.ExpireCount := StrToIntDef(GetNodeValue(SubNode, 'Expire_Count'), 0);
          FProgramInfo.ExpireInfo.ExpireBasedOnAsString := GetNodeValue(SubNode, 'Expire_Based_On');
          FProgramInfo.ExpireInfo.ExpireOtherInfo := GetNodeValue(SubNode, 'Expire_Other_Info');
          FProgramInfo.ExpireInfo.ExpireMonth := StrToIntDef(GetNodeValue(SubNode, 'Expire_Month'), 0);
          FProgramInfo.ExpireInfo.ExpireDay := StrToIntDef(GetNodeValue(SubNode, 'Expire_Day'), 0);
          FProgramInfo.ExpireInfo.ExpireYear := StrToIntDef(GetNodeValue(SubNode, 'Expire_Year'), 0);
        end;
      end;

      // Load Program Descriptions
      Node := RootNode.FindNode('Program_Descriptions');
      if Assigned(Node) then
      begin
        SubNode := Node.FindNode('English');
        if Assigned(SubNode) then
        begin
          FProgramDescriptions.English.Keywords := GetNodeValue(SubNode, 'Keywords');
          FProgramDescriptions.English.CharDesc45 := GetNodeValue(SubNode, 'Char_Desc_45');
          FProgramDescriptions.English.CharDesc80 := GetNodeValue(SubNode, 'Char_Desc_80');
          FProgramDescriptions.English.CharDesc250 := GetNodeValue(SubNode, 'Char_Desc_250');
          FProgramDescriptions.English.CharDesc450 := GetNodeValue(SubNode, 'Char_Desc_450');
          FProgramDescriptions.English.CharDesc2000 := GetNodeValue(SubNode, 'Char_Desc_2000');
        end;
      end;

      // Load Web Info
      Node := RootNode.FindNode('Web_Info');
      if Assigned(Node) then
      begin
        // Application URLs
        SubNode := Node.FindNode('Application_URLs');
        if Assigned(SubNode) then
        begin
          FWebInfo.ApplicationURLs.ApplicationInfoURL := GetNodeValue(SubNode, 'Application_Info_URL');
          FWebInfo.ApplicationURLs.ApplicationOrderURL := GetNodeValue(SubNode, 'Application_Order_URL');
          FWebInfo.ApplicationURLs.ApplicationScreenshotURL := GetNodeValue(SubNode, 'Application_Screenshot_URL');
          FWebInfo.ApplicationURLs.ApplicationIconURL := GetNodeValue(SubNode, 'Application_Icon_URL');
          FWebInfo.ApplicationURLs.ApplicationXMLFileURL := GetNodeValue(SubNode, 'Application_XML_File_URL');
        end;

        // Download URLs
        SubNode := Node.FindNode('Download_URLs');
        if Assigned(SubNode) then
        begin
          FWebInfo.DownloadURLs.PrimaryDownloadURL := GetNodeValue(SubNode, 'Primary_Download_URL');
          FWebInfo.DownloadURLs.SecondaryDownloadURL := GetNodeValue(SubNode, 'Secondary_Download_URL');
          FWebInfo.DownloadURLs.AdditionalDownloadURL1 := GetNodeValue(SubNode, 'Additional_Download_URL_1');
          FWebInfo.DownloadURLs.AdditionalDownloadURL2 := GetNodeValue(SubNode, 'Additional_Download_URL_2');
        end;
      end;

      // Load Permissions
      Node := RootNode.FindNode('Permissions');
      if Assigned(Node) then
      begin
        FPermissions.DistributionPermissions := GetNodeValue(Node, 'Distribution_Permissions');
        FPermissions.EULA := GetNodeValue(Node, 'EULA');
      end;

      // Load Affiliates
      Node := RootNode.FindNode('Affiliates');
      if Assigned(Node) then
      begin
        FAffiliates.Affiliates_FORM := UpperCase(GetNodeValue(Node, 'Affiliates_FORM')) = 'Y';

        // Set version based on master version
        if MasterPadVersionInfo.Version < 3.10 then
          FAffiliates.Affiliates_VERSION := '1.2'
        else
          FAffiliates.Affiliates_VERSION := '1.4';

        // But if version is specified in XML, use it
        if GetNodeValue(Node, 'Affiliates_VERSION') <> '' then
          FAffiliates.Affiliates_VERSION := GetNodeValue(Node, 'Affiliates_VERSION');

        FAffiliates.Affiliates_URL := GetNodeValue(Node, 'Affiliates_URL');
        FAffiliates.Affiliates_Information_Page := GetNodeValue(Node, 'Affiliates_Information_Page');

        // Check if we should load full section
        if Trim(FAffiliates.Affiliates_Information_Page) = '' then
        begin
          // Load individual affiliate companies (full section)
          LoadAffiliateCompany(FAffiliates.Avangate,
            'Affiliates_Avangate_Order_Page',
            'Affiliates_Avangate_Vendor_ID',
            'Affiliates_Avangate_Product_ID',
            'Affiliates_Avangate_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.BMTMicro,
            'Affiliates_BMTMicro_Order_Page',
            'Affiliates_BMTMicro_Vendor_ID',
            'Affiliates_BMTMicro_Product_ID',
            'Affiliates_BMTMicro_Maximum_Commission_Rate');

          // Load Cleverbridge only for version 1.4+
          if FAffiliates.Affiliates_VERSION >= '1.4' then
          begin
            LoadAffiliateCompany(FAffiliates.Cleverbridge,
              'Affiliates_Cleverbridge_Order_Page',
              'Affiliates_Cleverbridge_Vendor_ID',
              'Affiliates_Cleverbridge_Product_ID',
              'Affiliates_Cleverbridge_Maximum_Commission_Rate');
          end;

          LoadAffiliateCompany(FAffiliates.ClixGalore,
            'Affiliates_clixGalore_Order_Page',
            'Affiliates_clixGalore_Vendor_ID',
            'Affiliates_clixGalore_Product_ID',
            'Affiliates_clixGalore_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.CommissionJunction,
            'Affiliates_CommissionJunction_Order_Page',
            'Affiliates_CommissionJunction_Vendor_ID',
            'Affiliates_CommissionJunction_Product_ID',
            'Affiliates_CommissionJunction_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.DigiBuy,
            'Affiliates_DigiBuy_Order_Page',
            'Affiliates_DigiBuy_Vendor_ID',
            'Affiliates_DigiBuy_Product_ID',
            'Affiliates_DigiBuy_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.DigitalCandle,
            'Affiliates_DigitalCandle_Order_Page',
            'Affiliates_DigitalCandle_Vendor_ID',
            'Affiliates_DigitalCandle_Product_ID',
            'Affiliates_DigitalCandle_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Emetrix,
            'Affiliates_Emetrix_Order_Page',
            'Affiliates_Emetrix_Vendor_ID',
            'Affiliates_Emetrix_Product_ID',
            'Affiliates_Emetrix_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.eSellerate,
            'Affiliates_eSellerate_Order_Page',
            'Affiliates_eSellerate_Vendor_ID',
            'Affiliates_eSellerate_Product_ID',
            'Affiliates_eSellerate_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Kagi,
            'Affiliates_Kagi_Order_Page',
            'Affiliates_Kagi_Vendor_ID',
            'Affiliates_Kagi_Product_ID',
            'Affiliates_Kagi_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.LinkShare,
            'Affiliates_LinkShare_Order_Page',
            'Affiliates_LinkShare_Vendor_ID',
            'Affiliates_LinkShare_Product_ID',
            'Affiliates_LinkShare_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.NorthStarSol,
            'Affiliates_NorthStarSol_Order_Page',
            'Affiliates_NorthStarSol_Vendor_ID',
            'Affiliates_NorthStarSol_Product_ID',
            'Affiliates_NorthStarSol_Maximum_Commission_Rate');

          // Load OneNetworkDirect only for version 1.4+
          if FAffiliates.Affiliates_VERSION >= '1.4' then
          begin
            LoadAffiliateCompany(FAffiliates.OneNetworkDirect,
              'Affiliates_OneNetworkDirect_Order_Page',
              'Affiliates_OneNetworkDirect_Vendor_ID',
              'Affiliates_OneNetworkDirect_Product_ID',
              'Affiliates_OneNetworkDirect_Maximum_Commission_Rate');
          end;

          LoadAffiliateCompany(FAffiliates.Order1,
            'Affiliates_Order1_Order_Page',
            'Affiliates_Order1_Vendor_ID',
            'Affiliates_Order1_Product_ID',
            'Affiliates_Order1_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Osolis,
            'Affiliates_Osolis_Order_Page',
            'Affiliates_Osolis_Vendor_ID',
            'Affiliates_Osolis_Product_ID',
            'Affiliates_Osolis_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Plimus,
            'Affiliates_Plimus_Order_Page',
            'Affiliates_Plimus_Vendor_ID',
            'Affiliates_Plimus_Product_ID',
            'Affiliates_Plimus_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Regnet,
            'Affiliates_Regnet_Order_Page',
            'Affiliates_Regnet_Vendor_ID',
            'Affiliates_Regnet_Product_ID',
            'Affiliates_Regnet_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Regnow,
            'Affiliates_Regnow_Order_Page',
            'Affiliates_Regnow_Vendor_ID',
            'Affiliates_Regnow_Product_ID',
            'Affiliates_Regnow_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Regsoft,
            'Affiliates_Regsoft_Order_Page',
            'Affiliates_Regsoft_Vendor_ID',
            'Affiliates_Regsoft_Product_ID',
            'Affiliates_Regsoft_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.ShareIt,
            'Affiliates_ShareIt_Order_Page',
            'Affiliates_ShareIt_Vendor_ID',
            'Affiliates_ShareIt_Product_ID',
            'Affiliates_ShareIt_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Shareasale,
            'Affiliates_Shareasale_Order_Page',
            'Affiliates_Shareasale_Vendor_ID',
            'Affiliates_Shareasale_Product_ID',
            'Affiliates_Shareasale_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.SWReg,
            'Affiliates_SWReg_Order_Page',
            'Affiliates_SWReg_Vendor_ID',
            'Affiliates_SWReg_Product_ID',
            'Affiliates_SWReg_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.VShare,
            'Affiliates_V-Share_Order_Page',
            'Affiliates_V-Share_Vendor_ID',
            'Affiliates_V-Share_Product_ID',
            'Affiliates_V-Share_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.VFree,
            'Affiliates_VFree_Order_Page',
            'Affiliates_VFree_Vendor_ID',
            'Affiliates_VFree_Product_ID',
            'Affiliates_VFree_Maximum_Commission_Rate');

          LoadAffiliateCompany(FAffiliates.Yaskifo,
            'Affiliates_Yaskifo_Order_Page',
            'Affiliates_Yaskifo_Vendor_ID',
            'Affiliates_Yaskifo_Product_ID',
            'Affiliates_Yaskifo_Maximum_Commission_Rate');
        end;
      end;

      // Load ASP
      Node := RootNode.FindNode('ASP');
      if Assigned(Node) then
      begin
        FASP.ASPForm := UpperCase(GetNodeValue(Node, 'ASP_FORM')) = 'Y';
        FASP.ASPMember := UpperCase(GetNodeValue(Node, 'ASP_Member')) = 'Y';
        FASP.ASPMemberNumber := StrToIntDef(GetNodeValue(Node, 'ASP_Member_Number'), 0);
      end;

      FProgramDescriptions.English.SyncStringsToStrings;
      FNewsFeed.SyncStringsToStrings;
      FPermissions.SyncStringsToStrings;
    finally
      Doc.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TPadFormat.SaveToXML: string;
var
  Doc: TXMLDocument;
  RootNode, Node, SubNode: TDOMNode;
  Stream: TStringStream;
  FS: TFormatSettings;
  SaveFullSection: boolean;
begin
  Doc := TXMLDocument.Create;
  try
    FProgramDescriptions.English.SyncStringToStrings;
    FNewsFeed.SyncStringToStrings;
    FPermissions.SyncStringToStrings;

    // Create root element
    RootNode := Doc.CreateElement('XML_DIZ_INFO');
    Doc.AppendChild(RootNode);

    // Master Pad Version Info
    Node := AddChildNode(RootNode, 'MASTER_PAD_VERSION_INFO');
    SetNodeText(Doc, Node, 'MASTER_PAD_VERSION',
      FMasterPadVersionInfo.MasterPadVersion);
    SetNodeText(Doc, Node, 'MASTER_PAD_EDITOR',
      FMasterPadVersionInfo.MasterPadEditor);
    if Length(FMasterPadVersionInfo.MasterPadEditorUrl) > 0 then
      SetNodeText(Doc, Node, 'MASTER_PAD_EDITOR_URL',
        FMasterPadVersionInfo.MasterPadEditorUrl);
    SetNodeText(Doc, Node, 'MASTER_PAD_INFO',
      FMasterPadVersionInfo.MasterPadInfo);

    // Company Info
    Node := AddChildNode(RootNode, 'Company_Info');
    SetNodeText(Doc, Node, 'Company_Name', FCompanyInfo.CompanyName);
    SetNodeText(Doc, Node, 'Address_1', FCompanyInfo.Address1);
    SetNodeText(Doc, Node, 'Address_2', FCompanyInfo.Address2);
    SetNodeText(Doc, Node, 'City_Town', FCompanyInfo.CityTown);
    SetNodeText(Doc, Node, 'State_Province', FCompanyInfo.StateProvince);
    SetNodeText(Doc, Node, 'Zip_Postal_Code', FCompanyInfo.ZipPostalCode);
    SetNodeText(Doc, Node, 'Country', FCompanyInfo.Country);
    SetNodeText(Doc, Node, 'Company_WebSite_URL', FCompanyInfo.CompanyWebsiteURL);

    // Contact Info
    SubNode := AddChildNode(Node, 'Contact_Info');
    SetNodeText(Doc, SubNode, 'Author_First_Name',
      FCompanyInfo.ContactInfo.AuthorFirstName);
    SetNodeText(Doc, SubNode, 'Author_Last_Name',
      FCompanyInfo.ContactInfo.AuthorLastName);
    SetNodeText(Doc, SubNode, 'Author_Email',
      FCompanyInfo.ContactInfo.AuthorEmail);
    SetNodeText(Doc, SubNode, 'Contact_First_Name',
      FCompanyInfo.ContactInfo.ContactFirstName);
    SetNodeText(Doc, SubNode, 'Contact_Last_Name',
      FCompanyInfo.ContactInfo.ContactLastName);
    SetNodeText(Doc, SubNode, 'Contact_Email',
      FCompanyInfo.ContactInfo.ContactEmail);

    // Support Info
    SubNode := AddChildNode(Node, 'Support_Info');
    SetNodeText(Doc, SubNode, 'Sales_Email',
      FCompanyInfo.SupportInfo.SalesEmail);
    SetNodeText(Doc, SubNode, 'Support_Email',
      FCompanyInfo.SupportInfo.SupportEmail);
    SetNodeText(Doc, SubNode, 'General_Email',
      FCompanyInfo.SupportInfo.GeneralEmail);
    SetNodeText(Doc, SubNode, 'Sales_Phone',
      FCompanyInfo.SupportInfo.SalesPhone);
    SetNodeText(Doc, SubNode, 'Support_Phone',
      FCompanyInfo.SupportInfo.SupportPhone);
    SetNodeText(Doc, SubNode, 'General_Phone',
      FCompanyInfo.SupportInfo.GeneralPhone);
    SetNodeText(Doc, SubNode, 'Fax_Phone',
      FCompanyInfo.SupportInfo.FaxPhone);

    if (MasterPadVersionInfo.Version >= 4) then
    begin
      // Social media pages
      SetNodeText(Doc, Node, 'GooglePlusPage', FCompanyInfo.GooglePlusPage);
      SetNodeText(Doc, Node, 'LinkedinPage', FCompanyInfo.LinkedinPage);
      SetNodeText(Doc, Node, 'TwitterCompanyPage', FCompanyInfo.TwitterCompanyPage);
      SetNodeText(Doc, Node, 'FacebookCompanyPage', FCompanyInfo.FacebookCompanyPage);
      SetNodeText(Doc, Node, 'CompanyStorePage', FCompanyInfo.CompanyStorePage);

      // Save News Feed (updated with new fields)
      if FNewsFeed.NewsFeed_FORM then
      begin
        Node := AddChildNode(RootNode, 'NewsFeed');
        SetNodeText(Doc, Node, 'NewsFeed_FORM', BoolToStr(FNewsFeed.NewsFeed_FORM, 'Y', 'N'));
        SetNodeText(Doc, Node, 'NewsFeed_VERSION', FNewsFeed.NewsFeed_VERSION);
        SetNodeText(Doc, Node, 'NewsFeed_URL', FNewsFeed.NewsFeed_URL);
        SetNodeText(Doc, Node, 'NewsFeed_Type', FNewsFeed.NewsFeed_TypeAsString);
        SetNodeText(Doc, Node, 'NewsFeed_Language', FNewsFeed.NewsFeed_Language);
        SetNodeText(Doc, Node, 'NewsFeed_Purpose', FNewsFeed.NewsFeed_Purpose);
        SetNodeText(Doc, Node, 'NewsFeed_Author_Email', FNewsFeed.NewsFeed_Author_Email);
        SetNodeText(Doc, Node, 'NewsFeed_Author_First_Name', FNewsFeed.NewsFeed_Author_First_Name);
        SetNodeText(Doc, Node, 'NewsFeed_Author_Last_Name', FNewsFeed.NewsFeed_Author_Last_Name);
        SetNodeText(Doc, Node, 'NewsFeed_DESCRIPTION', FNewsFeed.NewsFeed_DESCRIPTION);
        SetNodeText(Doc, Node, 'NewsFeed_Feed_URL', FNewsFeed.NewsFeed_Feed_URL);
        SetNodeText(Doc, Node, 'NewsFeed_Site_Name', FNewsFeed.NewsFeed_Site_Name);
        SetNodeText(Doc, Node, 'NewsFeed_Site_URL', FNewsFeed.NewsFeed_Site_URL);
        SetNodeText(Doc, Node, 'NewsFeed_Title', FNewsFeed.NewsFeed_Title);
        SetNodeText(Doc, Node, 'NewsFeed_Keywords', FNewsFeed.NewsFeed_Keywords);
        SetNodeText(Doc, Node, 'NewsFeed_Description_70', FNewsFeed.NewsFeed_Description_70);
        SetNodeText(Doc, Node, 'NewsFeed_Description_250', FNewsFeed.NewsFeed_Description_250);
      end;
    end;

    // Program Info
    Node := AddChildNode(RootNode, 'Program_Info');
    SetNodeText(Doc, Node, 'Program_Name', FProgramInfo.ProgramName);
    SetNodeText(Doc, Node, 'Program_Version', FProgramInfo.ProgramVersion);
    SetNodeText(Doc, Node, 'Program_Release_Month',
      IfThen(FProgramInfo.ProgramReleaseMonth = 0, '', Format('%.2d', [FProgramInfo.ProgramReleaseMonth])));
    SetNodeText(Doc, Node, 'Program_Release_Day',
      IfThen(FProgramInfo.ProgramReleaseDay = 0, '', Format('%.2d', [FProgramInfo.ProgramReleaseDay])));
    SetNodeText(Doc, Node, 'Program_Release_Year',
      IntToStr(FProgramInfo.ProgramReleaseYear));
    SetNodeText(Doc, Node, 'Program_Cost_Dollars', FProgramInfo.ProgramCostDollars);
    SetNodeText(Doc, Node, 'Program_Cost_Other_Code',
      FProgramInfo.ProgramCostOtherCode);
    SetNodeText(Doc, Node, 'Program_Cost_Other',
      IfThen(FProgramInfo.ProgramCostOther = 0, '', IntToStr(FProgramInfo.ProgramCostOther)));
    SetNodeText(Doc, Node, 'Program_Type',
      FProgramInfo.ProgramTypeAsString);
    SetNodeText(Doc, Node, 'Program_Release_Status',
      FProgramInfo.ProgramReleaseStatusAsString);
    SetNodeText(Doc, Node, 'Program_Install_Support',
      FProgramInfo.ProgramInstallSupportAsString);
    SetNodeText(Doc, Node, 'Program_OS_Support',
      FProgramInfo.ProgramOSSupportAsString);
    SetNodeText(Doc, Node, 'Program_Language',
      FProgramInfo.ProgramLanguageAsString);
    SetNodeText(Doc, Node, 'Program_Change_Info',
      FProgramInfo.ProgramChangeInfo);
    SetNodeText(Doc, Node, 'Program_Specific_Category',
      FProgramInfo.ProgramSpecificCategory);
    SetNodeText(Doc, Node, 'Program_Category_Class',
      FProgramInfo.ProgramCategoryClassAsString);
    SetNodeText(Doc, Node, 'Program_System_Requirements',
      FProgramInfo.ProgramSystemRequirements);

    // File Info
    SubNode := AddChildNode(Node, 'File_Info');
    SetNodeText(Doc, SubNode, 'File_Size_Bytes',
      IntToStr(FProgramInfo.FileInfo.FileSizeBytes));
    SetNodeText(Doc, SubNode, 'File_Size_K',
      IntToStr(FProgramInfo.FileInfo.FileSizeK));
    FS.DecimalSeparator := '.';
    SetNodeText(Doc, SubNode, 'File_Size_MB',
      FloatToStr(FProgramInfo.FileInfo.FileSizeMB, FS));

    // Expire Info
    SubNode := AddChildNode(Node, 'Expire_Info');
    SetNodeText(Doc, SubNode, 'Has_Expire_Info',
      BoolToStr(FProgramInfo.ExpireInfo.HasExpireInfo, 'Y', 'N'));
    SetNodeText(Doc, SubNode, 'Expire_Count',
      IfThen(FProgramInfo.ExpireInfo.ExpireCount = 0, '', IntToStr(FProgramInfo.ExpireInfo.ExpireCount)));
    SetNodeText(Doc, SubNode, 'Expire_Based_On',
      FProgramInfo.ExpireInfo.ExpireBasedOnAsString);
    SetNodeText(Doc, SubNode, 'Expire_Other_Info',
      FProgramInfo.ExpireInfo.ExpireOtherInfo);
    SetNodeText(Doc, SubNode, 'Expire_Month',
      IfThen(FProgramInfo.ExpireInfo.ExpireMonth = 0, '', Format('%.2d', [FProgramInfo.ExpireInfo.ExpireMonth])));
    SetNodeText(Doc, SubNode, 'Expire_Day',
      IfThen(FProgramInfo.ExpireInfo.ExpireDay = 0, '', Format('%.2d', [FProgramInfo.ExpireInfo.ExpireDay])));
    SetNodeText(Doc, SubNode, 'Expire_Year',
      IfThen(FProgramInfo.ExpireInfo.ExpireYear = 0, '', IntToStr(FProgramInfo.ExpireInfo.ExpireYear)));

    // Program Descriptions
    Node := AddChildNode(RootNode, 'Program_Descriptions');
    SubNode := AddChildNode(Node, 'English');
    SetNodeText(Doc, SubNode, 'Keywords',
      FProgramDescriptions.English.Keywords);
    SetNodeText(Doc, SubNode, 'Char_Desc_45',
      FProgramDescriptions.English.CharDesc45);
    SetNodeText(Doc, SubNode, 'Char_Desc_80',
      FProgramDescriptions.English.CharDesc80);
    SetNodeText(Doc, SubNode, 'Char_Desc_250',
      FProgramDescriptions.English.CharDesc250);
    SetNodeText(Doc, SubNode, 'Char_Desc_450',
      FProgramDescriptions.English.CharDesc450);
    SetNodeText(Doc, SubNode, 'Char_Desc_2000',
      FProgramDescriptions.English.CharDesc2000);

    // Web Info
    Node := AddChildNode(RootNode, 'Web_Info');
    SubNode := AddChildNode(Node, 'Application_URLs');
    SetNodeText(Doc, SubNode, 'Application_Info_URL',
      FWebInfo.ApplicationURLs.ApplicationInfoURL);
    SetNodeText(Doc, SubNode, 'Application_Order_URL',
      FWebInfo.ApplicationURLs.ApplicationOrderURL);
    SetNodeText(Doc, SubNode, 'Application_Screenshot_URL',
      FWebInfo.ApplicationURLs.ApplicationScreenshotURL);
    SetNodeText(Doc, SubNode, 'Application_Icon_URL',
      FWebInfo.ApplicationURLs.ApplicationIconURL);
    SetNodeText(Doc, SubNode, 'Application_XML_File_URL',
      FWebInfo.ApplicationURLs.ApplicationXMLFileURL);

    SubNode := AddChildNode(Node, 'Download_URLs');
    SetNodeText(Doc, SubNode, 'Primary_Download_URL',
      FWebInfo.DownloadURLs.PrimaryDownloadURL);
    SetNodeText(Doc, SubNode, 'Secondary_Download_URL',
      FWebInfo.DownloadURLs.SecondaryDownloadURL);
    SetNodeText(Doc, SubNode, 'Additional_Download_URL_1',
      FWebInfo.DownloadURLs.AdditionalDownloadURL1);
    SetNodeText(Doc, SubNode, 'Additional_Download_URL_2',
      FWebInfo.DownloadURLs.AdditionalDownloadURL2);

    // Permissions
    Node := AddChildNode(RootNode, 'Permissions');
    SetNodeText(Doc, Node, 'Distribution_Permissions', FPermissions.DistributionPermissions);
    SetNodeText(Doc, Node, 'EULA', FPermissions.EULA);

    // Save Affiliates
    // Determine Affiliates version based on master version
    if MasterPadVersionInfo.Version < 3.10 then
      FAffiliates.Affiliates_VERSION := '1.2'
    else
      FAffiliates.Affiliates_VERSION := '1.4';

    // Check if we should save full section
    SaveFullSection := FAffiliates.ShouldSaveFullSection;

    if (FAffiliates.Affiliates_FORM) then
    begin
      Node := AddChildNode(RootNode, 'Affiliates');
      SetNodeText(Doc, Node, 'Affiliates_FORM', BoolToStr(FAffiliates.Affiliates_FORM, 'Y', 'N'));
      SetNodeText(Doc, Node, 'Affiliates_VERSION', FAffiliates.Affiliates_VERSION);
      SetNodeText(Doc, Node, 'Affiliates_URL', FAffiliates.Affiliates_URL);
      SetNodeText(Doc, Node, 'Affiliates_Information_Page', FAffiliates.Affiliates_Information_Page);

      // Save individual affiliate companies only if full section is needed
      if SaveFullSection then
      begin
        SetNodeText(Doc, Node, 'Affiliates_Avangate_Order_Page', FAffiliates.Avangate.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Avangate_Vendor_ID', FAffiliates.Avangate.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Avangate_Product_ID', FAffiliates.Avangate.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Avangate_Maximum_Commission_Rate', FAffiliates.Avangate.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_BMTMicro_Order_Page', FAffiliates.BMTMicro.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_BMTMicro_Vendor_ID', FAffiliates.BMTMicro.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_BMTMicro_Product_ID', FAffiliates.BMTMicro.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_BMTMicro_Maximum_Commission_Rate', FAffiliates.BMTMicro.MaximumCommissionRate);

        // Save Cleverbridge only for version 1.4+
        if FAffiliates.Affiliates_VERSION >= '1.4' then
        begin
          SetNodeText(Doc, Node, 'Affiliates_Cleverbridge_Order_Page', FAffiliates.Cleverbridge.OrderPage);
          SetNodeText(Doc, Node, 'Affiliates_Cleverbridge_Vendor_ID', FAffiliates.Cleverbridge.VendorID);
          SetNodeText(Doc, Node, 'Affiliates_Cleverbridge_Product_ID', FAffiliates.Cleverbridge.ProductID);
          SetNodeText(Doc, Node, 'Affiliates_Cleverbridge_Maximum_Commission_Rate', FAffiliates.Cleverbridge.MaximumCommissionRate);
        end;

        SetNodeText(Doc, Node, 'Affiliates_clixGalore_Order_Page', FAffiliates.ClixGalore.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_clixGalore_Vendor_ID', FAffiliates.ClixGalore.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_clixGalore_Product_ID', FAffiliates.ClixGalore.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_clixGalore_Maximum_Commission_Rate', FAffiliates.ClixGalore.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_CommissionJunction_Order_Page', FAffiliates.CommissionJunction.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_CommissionJunction_Vendor_ID', FAffiliates.CommissionJunction.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_CommissionJunction_Product_ID', FAffiliates.CommissionJunction.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_CommissionJunction_Maximum_Commission_Rate',
          FAffiliates.CommissionJunction.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_DigiBuy_Order_Page', FAffiliates.DigiBuy.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_DigiBuy_Vendor_ID', FAffiliates.DigiBuy.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_DigiBuy_Product_ID', FAffiliates.DigiBuy.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_DigiBuy_Maximum_Commission_Rate', FAffiliates.DigiBuy.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_DigitalCandle_Order_Page', FAffiliates.DigitalCandle.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_DigitalCandle_Vendor_ID', FAffiliates.DigitalCandle.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_DigitalCandle_Product_ID', FAffiliates.DigitalCandle.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_DigitalCandle_Maximum_Commission_Rate', FAffiliates.DigitalCandle.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Emetrix_Order_Page', FAffiliates.Emetrix.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Emetrix_Vendor_ID', FAffiliates.Emetrix.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Emetrix_Product_ID', FAffiliates.Emetrix.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Emetrix_Maximum_Commission_Rate', FAffiliates.Emetrix.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_eSellerate_Order_Page', FAffiliates.eSellerate.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_eSellerate_Vendor_ID', FAffiliates.eSellerate.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_eSellerate_Product_ID', FAffiliates.eSellerate.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_eSellerate_Maximum_Commission_Rate', FAffiliates.eSellerate.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Kagi_Order_Page', FAffiliates.Kagi.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Kagi_Vendor_ID', FAffiliates.Kagi.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Kagi_Product_ID', FAffiliates.Kagi.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Kagi_Maximum_Commission_Rate', FAffiliates.Kagi.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_LinkShare_Order_Page', FAffiliates.LinkShare.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_LinkShare_Vendor_ID', FAffiliates.LinkShare.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_LinkShare_Product_ID', FAffiliates.LinkShare.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_LinkShare_Maximum_Commission_Rate', FAffiliates.LinkShare.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_NorthStarSol_Order_Page', FAffiliates.NorthStarSol.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_NorthStarSol_Vendor_ID', FAffiliates.NorthStarSol.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_NorthStarSol_Product_ID', FAffiliates.NorthStarSol.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_NorthStarSol_Maximum_Commission_Rate', FAffiliates.NorthStarSol.MaximumCommissionRate);

        // Save OneNetworkDirect only for version 1.4+
        if FAffiliates.Affiliates_VERSION >= '1.4' then
        begin
          SetNodeText(Doc, Node, 'Affiliates_OneNetworkDirect_Order_Page', FAffiliates.OneNetworkDirect.OrderPage);
          SetNodeText(Doc, Node, 'Affiliates_OneNetworkDirect_Vendor_ID', FAffiliates.OneNetworkDirect.VendorID);
          SetNodeText(Doc, Node, 'Affiliates_OneNetworkDirect_Product_ID', FAffiliates.OneNetworkDirect.ProductID);
          SetNodeText(Doc, Node, 'Affiliates_OneNetworkDirect_Maximum_Commission_Rate',
            FAffiliates.OneNetworkDirect.MaximumCommissionRate);
        end;

        SetNodeText(Doc, Node, 'Affiliates_Order1_Order_Page', FAffiliates.Order1.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Order1_Vendor_ID', FAffiliates.Order1.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Order1_Product_ID', FAffiliates.Order1.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Order1_Maximum_Commission_Rate', FAffiliates.Order1.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Osolis_Order_Page', FAffiliates.Osolis.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Osolis_Vendor_ID', FAffiliates.Osolis.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Osolis_Product_ID', FAffiliates.Osolis.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Osolis_Maximum_Commission_Rate', FAffiliates.Osolis.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Plimus_Order_Page', FAffiliates.Plimus.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Plimus_Vendor_ID', FAffiliates.Plimus.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Plimus_Product_ID', FAffiliates.Plimus.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Plimus_Maximum_Commission_Rate', FAffiliates.Plimus.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Regnet_Order_Page', FAffiliates.Regnet.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Regnet_Vendor_ID', FAffiliates.Regnet.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Regnet_Product_ID', FAffiliates.Regnet.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Regnet_Maximum_Commission_Rate', FAffiliates.Regnet.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Regnow_Order_Page', FAffiliates.Regnow.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Regnow_Vendor_ID', FAffiliates.Regnow.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Regnow_Product_ID', FAffiliates.Regnow.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Regnow_Maximum_Commission_Rate', FAffiliates.Regnow.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Regsoft_Order_Page', FAffiliates.Regsoft.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Regsoft_Vendor_ID', FAffiliates.Regsoft.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Regsoft_Product_ID', FAffiliates.Regsoft.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Regsoft_Maximum_Commission_Rate', FAffiliates.Regsoft.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_ShareIt_Order_Page', FAffiliates.ShareIt.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_ShareIt_Vendor_ID', FAffiliates.ShareIt.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_ShareIt_Product_ID', FAffiliates.ShareIt.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_ShareIt_Maximum_Commission_Rate', FAffiliates.ShareIt.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Shareasale_Order_Page', FAffiliates.Shareasale.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Shareasale_Vendor_ID', FAffiliates.Shareasale.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Shareasale_Product_ID', FAffiliates.Shareasale.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Shareasale_Maximum_Commission_Rate', FAffiliates.Shareasale.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_SWReg_Order_Page', FAffiliates.SWReg.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_SWReg_Vendor_ID', FAffiliates.SWReg.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_SWReg_Product_ID', FAffiliates.SWReg.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_SWReg_Maximum_Commission_Rate', FAffiliates.SWReg.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_V-Share_Order_Page', FAffiliates.VShare.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_V-Share_Vendor_ID', FAffiliates.VShare.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_V-Share_Product_ID', FAffiliates.VShare.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_V-Share_Maximum_Commission_Rate', FAffiliates.VShare.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_VFree_Order_Page', FAffiliates.VFree.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_VFree_Vendor_ID', FAffiliates.VFree.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_VFree_Product_ID', FAffiliates.VFree.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_VFree_Maximum_Commission_Rate', FAffiliates.VFree.MaximumCommissionRate);

        SetNodeText(Doc, Node, 'Affiliates_Yaskifo_Order_Page', FAffiliates.Yaskifo.OrderPage);
        SetNodeText(Doc, Node, 'Affiliates_Yaskifo_Vendor_ID', FAffiliates.Yaskifo.VendorID);
        SetNodeText(Doc, Node, 'Affiliates_Yaskifo_Product_ID', FAffiliates.Yaskifo.ProductID);
        SetNodeText(Doc, Node, 'Affiliates_Yaskifo_Maximum_Commission_Rate', FAffiliates.Yaskifo.MaximumCommissionRate);
      end;
    end;

    // ASP
    if FASP.ASPForm then
    begin
      Node := AddChildNode(RootNode, 'ASP');
      SetNodeText(Doc, Node, 'ASP_FORM', BoolToStr(FASP.ASPForm, 'Y', 'N'));
      SetNodeText(Doc, Node, 'ASP_Member', BoolToStr(FASP.ASPMember, 'Y', 'N'));
      SetNodeText(Doc, Node, 'ASP_Member_Number',
        IfThen(FASP.ASPMemberNumber = 0, '', IntToStr(FASP.ASPMemberNumber)));
    end;

    // Save to string
    Stream := TStringStream.Create('');
    try
      WriteXML(Doc, Stream);
      Result := Stream.DataString;
    finally
      Stream.Free;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TPadFormat.Clear;
begin
  // Clear all properties to default values
  FMasterPadVersionInfo.MasterPadVersion := '4.0';
  FMasterPadVersionInfo.MasterPadEditor := '';
  FMasterPadVersionInfo.MasterPadEditorUrl := '';
  FMasterPadVersionInfo.MasterPadInfo :=
    'Portable Application Description, or PAD for short, is a data set that is used by shareware authors to disseminate information to anyone interested in their software products. To find out more go to http://www.asp-shareware.org/pad';

  // Clear Company Info
  FCompanyInfo.CompanyName := '';
  FCompanyInfo.Address1 := '';
  FCompanyInfo.Address2 := '';
  FCompanyInfo.CityTown := '';
  FCompanyInfo.StateProvince := '';
  FCompanyInfo.ZipPostalCode := '';
  FCompanyInfo.Country := '';
  FCompanyInfo.CompanyWebsiteURL := '';

  // Clear Contact Info
  FCompanyInfo.ContactInfo.AuthorFirstName := '';
  FCompanyInfo.ContactInfo.AuthorLastName := '';
  FCompanyInfo.ContactInfo.AuthorEmail := '';
  FCompanyInfo.ContactInfo.ContactFirstName := '';
  FCompanyInfo.ContactInfo.ContactLastName := '';
  FCompanyInfo.ContactInfo.ContactEmail := '';

  // Clear Support Info
  FCompanyInfo.SupportInfo.SalesEmail := '';
  FCompanyInfo.SupportInfo.SupportEmail := '';
  FCompanyInfo.SupportInfo.GeneralEmail := '';
  FCompanyInfo.SupportInfo.SalesPhone := '';
  FCompanyInfo.SupportInfo.SupportPhone := '';
  FCompanyInfo.SupportInfo.GeneralPhone := '';
  FCompanyInfo.SupportInfo.FaxPhone := '';

  // Clear social media
  FCompanyInfo.GooglePlusPage := '';
  FCompanyInfo.LinkedinPage := '';
  FCompanyInfo.TwitterCompanyPage := '';
  FCompanyInfo.FacebookCompanyPage := '';
  FCompanyInfo.CompanyStorePage := '';

  // Clear News Feed
  FNewsFeed.NewsFeed_FORM := False;
  FNewsFeed.NewsFeed_VERSION := '1.0';
  FNewsFeed.NewsFeed_URL := 'http://Submit-Everywhere.com/extensions/NewsFeed.htm';
  FNewsFeed.NewsFeed_Type := pnftRSS090;
  FNewsFeed.NewsFeed_Language := '';
  FNewsFeed.NewsFeed_Purpose := '';
  FNewsFeed.NewsFeed_Author_Email := '';
  FNewsFeed.NewsFeed_Author_First_Name := '';
  FNewsFeed.NewsFeed_Author_Last_Name := '';
  FNewsFeed.NewsFeed_DESCRIPTION :=
    'This PAD extension allows you to add your RSS and Atom news feeds info into your PAD file. This info can be used by RSS feed submission software or by feed directories themselves.';
  FNewsFeed.NewsFeed_Feed_URL := '';
  FNewsFeed.NewsFeed_Site_Name := '';
  FNewsFeed.NewsFeed_Site_URL := '';
  FNewsFeed.NewsFeed_Title := '';
  FNewsFeed.NewsFeed_Keywords := '';
  FNewsFeed.NewsFeed_Description_70 := '';
  FNewsFeed.NewsFeed_Description_250 := '';

  // Clear Program Info
  FProgramInfo.ProgramName := '';
  FProgramInfo.ProgramVersion := '';
  FProgramInfo.ProgramReleaseMonth := 0;
  FProgramInfo.ProgramReleaseDay := 0;
  FProgramInfo.ProgramReleaseYear := 0;
  FProgramInfo.ProgramCostDollars := '0';
  FProgramInfo.ProgramCostOtherCode := '';
  FProgramInfo.ProgramCostOther := 0;
  FProgramInfo.ProgramType := pptShareware;
  FProgramInfo.ProgramReleaseStatus := prsNewRelease;
  FProgramInfo.ProgramInstallSupport := pisInstallAndUninstall;

  // Clear OS Support sets
  FProgramInfo.ProgramOSSupportWindows := [];
  FProgramInfo.ProgramOSSupportUnixLinux := [];
  FProgramInfo.ProgramOSSupportOther := [];

  // Clear Language sets
  FProgramInfo.ProgramLanguageEuropean := [];
  FProgramInfo.ProgramLanguageAsian := [];
  FProgramInfo.ProgramLanguageOtherMajor := [];
  FProgramInfo.ProgramLanguageWorld := [];

  FProgramInfo.ProgramChangeInfo := '';
  FProgramInfo.ProgramSpecificCategory := '';
  FProgramInfo.ProgramCategoryClass := pccNone;
  FProgramInfo.ProgramSystemRequirements := '';

  // Clear File Info
  FProgramInfo.FileInfo.FileSizeBytes := 0;
  FProgramInfo.FileInfo.FileSizeK := 0;
  FProgramInfo.FileInfo.FileSizeMB := 0;

  // Clear Expire Info
  FProgramInfo.ExpireInfo.HasExpireInfo := False;
  FProgramInfo.ExpireInfo.ExpireCount := 0;
  FProgramInfo.ExpireInfo.ExpireBasedOn := pebDays;
  FProgramInfo.ExpireInfo.ExpireOtherInfo := '';
  FProgramInfo.ExpireInfo.ExpireMonth := 0;
  FProgramInfo.ExpireInfo.ExpireDay := 0;
  FProgramInfo.ExpireInfo.ExpireYear := 0;

  // Clear Program Descriptions
  FProgramDescriptions.English.Keywords := '';
  FProgramDescriptions.English.CharDesc45 := '';
  FProgramDescriptions.English.CharDesc80 := '';
  FProgramDescriptions.English.CharDesc250 := '';
  FProgramDescriptions.English.CharDesc450 := '';
  FProgramDescriptions.English.CharDesc2000 := '';

  // Clear TStrings objects
  FProgramDescriptions.English.CharDesc250Strings.Clear;
  FProgramDescriptions.English.CharDesc450Strings.Clear;
  FProgramDescriptions.English.CharDesc2000Strings.Clear;

  // Clear Web Info
  FWebInfo.ApplicationURLs.ApplicationInfoURL := '';
  FWebInfo.ApplicationURLs.ApplicationOrderURL := '';
  FWebInfo.ApplicationURLs.ApplicationScreenshotURL := '';
  FWebInfo.ApplicationURLs.ApplicationIconURL := '';
  FWebInfo.ApplicationURLs.ApplicationXMLFileURL := '';

  FWebInfo.DownloadURLs.PrimaryDownloadURL := '';
  FWebInfo.DownloadURLs.SecondaryDownloadURL := '';
  FWebInfo.DownloadURLs.AdditionalDownloadURL1 := '';
  FWebInfo.DownloadURLs.AdditionalDownloadURL2 := '';

  // Clear Permissions
  FPermissions.DistributionPermissions := '';
  FPermissions.EULA := '';
  FPermissions.DistributionPermissionsStrings.Clear;
  FPermissions.EULAStrings.Clear;

  // Clear Affiliates
  FAffiliates.Affiliates_FORM := False;
  FAffiliates.Affiliates_VERSION := '1.4'; // Default, will be updated on save based on master version
  FAffiliates.Affiliates_URL := 'http://www.asp-shareware.org/pad/extensions/Affiliates.htm';
  FAffiliates.Affiliates_Information_Page := '';

  // Clear all affiliate companies
  FAffiliates.Avangate.OrderPage := '';
  FAffiliates.Avangate.VendorID := '';
  FAffiliates.Avangate.ProductID := '';
  FAffiliates.Avangate.MaximumCommissionRate := '';

  FAffiliates.BMTMicro.OrderPage := '';
  FAffiliates.BMTMicro.VendorID := '';
  FAffiliates.BMTMicro.ProductID := '';
  FAffiliates.BMTMicro.MaximumCommissionRate := '';

  FAffiliates.Cleverbridge.OrderPage := '';
  FAffiliates.Cleverbridge.VendorID := '';
  FAffiliates.Cleverbridge.ProductID := '';
  FAffiliates.Cleverbridge.MaximumCommissionRate := '';

  FAffiliates.ClixGalore.OrderPage := '';
  FAffiliates.ClixGalore.VendorID := '';
  FAffiliates.ClixGalore.ProductID := '';
  FAffiliates.ClixGalore.MaximumCommissionRate := '';

  FAffiliates.CommissionJunction.OrderPage := '';
  FAffiliates.CommissionJunction.VendorID := '';
  FAffiliates.CommissionJunction.ProductID := '';
  FAffiliates.CommissionJunction.MaximumCommissionRate := '';

  FAffiliates.DigiBuy.OrderPage := '';
  FAffiliates.DigiBuy.VendorID := '';
  FAffiliates.DigiBuy.ProductID := '';
  FAffiliates.DigiBuy.MaximumCommissionRate := '';

  FAffiliates.DigitalCandle.OrderPage := '';
  FAffiliates.DigitalCandle.VendorID := '';
  FAffiliates.DigitalCandle.ProductID := '';
  FAffiliates.DigitalCandle.MaximumCommissionRate := '';

  FAffiliates.Emetrix.OrderPage := '';
  FAffiliates.Emetrix.VendorID := '';
  FAffiliates.Emetrix.ProductID := '';
  FAffiliates.Emetrix.MaximumCommissionRate := '';

  FAffiliates.eSellerate.OrderPage := '';
  FAffiliates.eSellerate.VendorID := '';
  FAffiliates.eSellerate.ProductID := '';
  FAffiliates.eSellerate.MaximumCommissionRate := '';

  FAffiliates.Kagi.OrderPage := '';
  FAffiliates.Kagi.VendorID := '';
  FAffiliates.Kagi.ProductID := '';
  FAffiliates.Kagi.MaximumCommissionRate := '';

  FAffiliates.LinkShare.OrderPage := '';
  FAffiliates.LinkShare.VendorID := '';
  FAffiliates.LinkShare.ProductID := '';
  FAffiliates.LinkShare.MaximumCommissionRate := '';

  FAffiliates.NorthStarSol.OrderPage := '';
  FAffiliates.NorthStarSol.VendorID := '';
  FAffiliates.NorthStarSol.ProductID := '';
  FAffiliates.NorthStarSol.MaximumCommissionRate := '';

  FAffiliates.OneNetworkDirect.OrderPage := '';
  FAffiliates.OneNetworkDirect.VendorID := '';
  FAffiliates.OneNetworkDirect.ProductID := '';
  FAffiliates.OneNetworkDirect.MaximumCommissionRate := '';

  FAffiliates.Order1.OrderPage := '';
  FAffiliates.Order1.VendorID := '';
  FAffiliates.Order1.ProductID := '';
  FAffiliates.Order1.MaximumCommissionRate := '';

  FAffiliates.Osolis.OrderPage := '';
  FAffiliates.Osolis.VendorID := '';
  FAffiliates.Osolis.ProductID := '';
  FAffiliates.Osolis.MaximumCommissionRate := '';

  FAffiliates.Plimus.OrderPage := '';
  FAffiliates.Plimus.VendorID := '';
  FAffiliates.Plimus.ProductID := '';
  FAffiliates.Plimus.MaximumCommissionRate := '';

  FAffiliates.Regnet.OrderPage := '';
  FAffiliates.Regnet.VendorID := '';
  FAffiliates.Regnet.ProductID := '';
  FAffiliates.Regnet.MaximumCommissionRate := '';

  FAffiliates.Regnow.OrderPage := '';
  FAffiliates.Regnow.VendorID := '';
  FAffiliates.Regnow.ProductID := '';
  FAffiliates.Regnow.MaximumCommissionRate := '';

  FAffiliates.Regsoft.OrderPage := '';
  FAffiliates.Regsoft.VendorID := '';
  FAffiliates.Regsoft.ProductID := '';
  FAffiliates.Regsoft.MaximumCommissionRate := '';

  FAffiliates.ShareIt.OrderPage := '';
  FAffiliates.ShareIt.VendorID := '';
  FAffiliates.ShareIt.ProductID := '';
  FAffiliates.ShareIt.MaximumCommissionRate := '';

  FAffiliates.Shareasale.OrderPage := '';
  FAffiliates.Shareasale.VendorID := '';
  FAffiliates.Shareasale.ProductID := '';
  FAffiliates.Shareasale.MaximumCommissionRate := '';

  FAffiliates.SWReg.OrderPage := '';
  FAffiliates.SWReg.VendorID := '';
  FAffiliates.SWReg.ProductID := '';
  FAffiliates.SWReg.MaximumCommissionRate := '';

  FAffiliates.VShare.OrderPage := '';
  FAffiliates.VShare.VendorID := '';
  FAffiliates.VShare.ProductID := '';
  FAffiliates.VShare.MaximumCommissionRate := '';

  FAffiliates.VFree.OrderPage := '';
  FAffiliates.VFree.VendorID := '';
  FAffiliates.VFree.ProductID := '';
  FAffiliates.VFree.MaximumCommissionRate := '';

  FAffiliates.Yaskifo.OrderPage := '';
  FAffiliates.Yaskifo.VendorID := '';
  FAffiliates.Yaskifo.ProductID := '';
  FAffiliates.Yaskifo.MaximumCommissionRate := '';

  // Clear ASP
  FASP.ASPForm := True;
  FASP.ASPMember := False;
  FASP.ASPMemberNumber := 0;
end;

function TPadFormat.SetNodeText(Doc: TXMLDocument; ParentNode: TDOMNode; NodeName, NodeValue: string): TDOMNode;
begin
  Result := Doc.CreateElement(DOMString(NodeName));
  TDOMElement(Result).TextContent := DOMString(NodeValue);
  ParentNode.AppendChild(Result);
end;

function TPadFormat.AddChildNode(ParentNode: TDOMNode; NodeName: string): TDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(DOMString(NodeName));
  ParentNode.AppendChild(Result);
end;

procedure TPadFormat.SetNodeTextValue(Node: TDOMNode; Value: string);
begin
  if Assigned(Node) then
    TDOMElement(Node).TextContent := DOMString(Value);
end;

// Helper functions implementation

function GetNodeText(Node: TDOMNode): string;
begin
  if Assigned(Node) and Assigned(Node.FirstChild) then
    Result := UTF8Encode(Node.FirstChild.NodeValue)
  else
    Result := '';
end;

function StrToInt64Safe(const S: string): int64;
begin
  // Convert string to Int64 safely, default to 0 if conversion fails
  Result := StrToInt64Def(Trim(S), 0);
end;

function StrToFloatSafe(const S: string): double;
var
  Temp: string;
  FS: TFormatSettings;
begin
  Temp := Trim(S);
  if Temp = '' then
    Exit(0);

  // Try with dot
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Result := StrToFloatDef(Temp, 0, FS);
  if Result <> 0 then
    Exit;

  // Try with comma
  FS.DecimalSeparator := ',';
  Result := StrToFloatDef(Temp, 0, FS);
end;

function PadInstallSupportToString(Value: TPadInstallSupport): string;
begin
  case Value of
    pisInstallAndUninstall: Result := 'Install and Uninstall';
    pisInstallOnly: Result := 'Install Only';
    pisNoInstallSupport: Result := 'No Install Support';
    pisUninstallOnly: Result := 'Uninstall Only';
    else
      Result := '';
  end;
end;

function StringToPadInstallSupport(const Value: string): TPadInstallSupport;
begin
  if Value = 'Install and Uninstall' then
    Result := pisInstallAndUninstall
  else if Value = 'Install Only' then
    Result := pisInstallOnly
  else if Value = 'No Install Support' then
    Result := pisNoInstallSupport
  else if Value = 'Uninstall Only' then
    Result := pisUninstallOnly
  else
    Result := pisInstallAndUninstall; // Default
end;

function PadProgramTypeToString(Value: TPadProgramType): string;
begin
  case Value of
    pptNone: Result := '';
    pptShareware: Result := 'Shareware';
    pptFreeware: Result := 'Freeware';
    pptAdware: Result := 'Adware';
    pptDemo: Result := 'Demo';
    pptCommercial: Result := 'Commercial';
    pptDataOnly: Result := 'Data Only';
    else
      Result := '';
  end;
end;

function StringToPadProgramType(const Value: string): TPadProgramType;
begin
  if Value = 'Shareware' then
    Result := pptShareware
  else if Value = 'Freeware' then
    Result := pptFreeware
  else if Value = 'Adware' then
    Result := pptAdware
  else if Value = 'Demo' then
    Result := pptDemo
  else if Value = 'Commercial' then
    Result := pptCommercial
  else if Value = 'Data Only' then
    Result := pptDataOnly
  else
    Result := pptShareware; // Default
end;

function PadReleaseStatusToString(Value: TPadReleaseStatus): string;
begin
  case Value of
    prsMajorUpdate: Result := 'Major Update';
    prsMinorUpdate: Result := 'Minor Update';
    prsNewRelease: Result := 'New Release';
    prsBeta: Result := 'Beta';
    prsAlpha: Result := 'Alpha';
    prsMediaOnly: Result := 'Media Only';
    else
      Result := '';
  end;
end;

function StringToPadReleaseStatus(const Value: string): TPadReleaseStatus;
begin
  if Value = 'Major Update' then
    Result := prsMajorUpdate
  else if Value = 'Minor Update' then
    Result := prsMinorUpdate
  else if Value = 'New Release' then
    Result := prsNewRelease
  else if Value = 'Beta' then
    Result := prsBeta
  else if Value = 'Alpha' then
    Result := prsAlpha
  else if Value = 'Media Only' then
    Result := prsMediaOnly
  else
    Result := prsNewRelease; // Default
end;

function PadExpireBasedOnToString(Value: TPadExpireBasedOn): string;
begin
  case Value of
    pebNone: Result := '';
    pebDays: Result := 'Days';
    pebUses: Result := 'Uses';
    pebEitherOr: Result := 'Either/Or';
    else
      Result := '';
  end;
end;

function StringToPadExpireBasedOn(const Value: string): TPadExpireBasedOn;
begin
  if Value = 'Days' then
    Result := pebDays
  else if Value = 'Uses' then
    Result := pebUses
  else if Value = 'EitherOr' then
    Result := pebEitherOr
  else
    Result := pebNone; // Default
end;

function PadProgramCategoryClassToString(Value: TPadProgramCategoryClass): string;
begin
  if (Value >= Low(TPadProgramCategoryClass)) and (Value <= High(TPadProgramCategoryClass)) then
    Result := PadProgramCategoryClassStrings[Value]
  else
    Result := '';
end;

function StringToPadProgramCategoryClass(const Value: string): TPadProgramCategoryClass;
var
  Category: TPadProgramCategoryClass;
begin
  // Simple linear search through array
  for Category := Low(TPadProgramCategoryClass) to High(TPadProgramCategoryClass) do
  begin
    if SameText(PadProgramCategoryClassStrings[Category], Value) then
    begin
      Result := Category;
      Exit;
    end;
  end;

  // If not found, return first value as default
  Result := Low(TPadProgramCategoryClass);
end;

function IsValidPadProgramCategoryClassString(const Value: string): boolean;
var
  Category: TPadProgramCategoryClass;
begin
  Result := False;
  for Category := Low(TPadProgramCategoryClass) to High(TPadProgramCategoryClass) do
  begin
    if SameText(PadProgramCategoryClassStrings[Category], Value) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function GetPadProgramCategoryClassDisplayName(Value: TPadProgramCategoryClass): string;
var
  FullString: string;
  PosSeparator: integer;
begin
  FullString := PadProgramCategoryClassToString(Value);
  PosSeparator := Pos('::', FullString);
  if PosSeparator > 0 then
    Result := Copy(FullString, PosSeparator + 2, Length(FullString))
  else
    Result := FullString;
end;

function GetPadProgramCategoryClassMainCategory(Value: TPadProgramCategoryClass): string;
var
  FullString: string;
  PosSeparator: integer;
begin
  FullString := PadProgramCategoryClassToString(Value);
  PosSeparator := Pos('::', FullString);
  if PosSeparator > 0 then
    Result := Copy(FullString, 1, PosSeparator - 1)
  else
    Result := FullString;
end;

function GetPadProgramCategoryClassSubCategory(Value: TPadProgramCategoryClass): string;
begin
  Result := GetPadProgramCategoryClassDisplayName(Value);
end;

// OS Support conversion functions
function PadOSWindowsToString(Value: TPadOSWindows): string;
begin
  if (Value >= Low(TPadOSWindows)) and (Value <= High(TPadOSWindows)) then
    Result := PadOSWindowsStrings[Value]
  else
    Result := '';
end;

function StringToPadOSWindows(const Value: string): TPadOSWindows;
var
  OS: TPadOSWindows;
begin
  for OS := Low(TPadOSWindows) to High(TPadOSWindows) do
  begin
    if SameText(PadOSWindowsStrings[OS], Value) then
    begin
      Result := OS;
      Exit;
    end;
  end;
  Result := Low(TPadOSWindows);
end;

function PadOSUnixLinuxToString(Value: TPadOSUnixLinux): string;
begin
  if (Value >= Low(TPadOSUnixLinux)) and (Value <= High(TPadOSUnixLinux)) then
    Result := PadOSUnixLinuxStrings[Value]
  else
    Result := '';
end;

function StringToPadOSUnixLinux(const Value: string): TPadOSUnixLinux;
var
  OS: TPadOSUnixLinux;
begin
  for OS := Low(TPadOSUnixLinux) to High(TPadOSUnixLinux) do
  begin
    if SameText(PadOSUnixLinuxStrings[OS], Value) then
    begin
      Result := OS;
      Exit;
    end;
  end;
  Result := Low(TPadOSUnixLinux);
end;

function PadOSOtherToString(Value: TPadOSOther): string;
begin
  if (Value >= Low(TPadOSOther)) and (Value <= High(TPadOSOther)) then
    Result := PadOSOtherStrings[Value]
  else
    Result := '';
end;

function StringToPadOSOther(const Value: string): TPadOSOther;
var
  OS: TPadOSOther;
begin
  for OS := Low(TPadOSOther) to High(TPadOSOther) do
  begin
    if SameText(PadOSOtherStrings[OS], Value) then
    begin
      Result := OS;
      Exit;
    end;
  end;
  Result := Low(TPadOSOther);
end;

// Combined Language conversion
// Combined OS Support conversion
function PadOSSupportToString(WindowsSet: TPadOSWindowsSet; UnixLinuxSet: TPadOSUnixLinuxSet; OtherSet: TPadOSOtherSet): string;
var
  List: TStringList;
  WinOS: TPadOSWindows;
  UnixOS: TPadOSUnixLinux;
  OtherOS: TPadOSOther;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ',';
    List.StrictDelimiter := True;

    // Add Unix/Linux OS
    for UnixOS := Low(TPadOSUnixLinux) to High(TPadOSUnixLinux) do
    begin
      if UnixOS in UnixLinuxSet then
        List.Add(PadOSUnixLinuxStrings[UnixOS]);
    end;

    // Add Windows OS
    for WinOS := Low(TPadOSWindows) to High(TPadOSWindows) do
    begin
      if WinOS in WindowsSet then
        List.Add(PadOSWindowsStrings[WinOS]);
    end;

    // Add Other OS
    for OtherOS := Low(TPadOSOther) to High(TPadOSOther) do
    begin
      if OtherOS in OtherSet then
        List.Add(PadOSOtherStrings[OtherOS]);
    end;

    Result := List.DelimitedText;
  finally
    List.Free;
  end;
end;

procedure StringToPadOSSupport(const Value: string; out WindowsSet: TPadOSWindowsSet; out UnixLinuxSet: TPadOSUnixLinuxSet;
  out OtherSet: TPadOSOtherSet);
var
  OSList: TStringList;
  i: integer;
  OSStr: string;
  Found: boolean;
  WinOS: TPadOSWindows;
  UnixOS: TPadOSUnixLinux;
  OtherOS: TPadOSOther;
begin
  WindowsSet := [];
  UnixLinuxSet := [];
  OtherSet := [];

  if Trim(Value) = '' then
    Exit;

  OSList := TStringList.Create;
  try
    OSList.Delimiter := ',';
    OSList.StrictDelimiter := True;
    OSList.DelimitedText := Value;

    for i := 0 to OSList.Count - 1 do
    begin
      OSStr := Trim(OSList[i]);
      Found := False;

      // Try to find in Windows set
      for WinOS := Low(TPadOSWindows) to High(TPadOSWindows) do
      begin
        if SameText(PadOSWindowsStrings[WinOS], OSStr) then
        begin
          Include(WindowsSet, WinOS);
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        // Try to find in Unix/Linux set
        for UnixOS := Low(TPadOSUnixLinux) to High(TPadOSUnixLinux) do
        begin
          if SameText(PadOSUnixLinuxStrings[UnixOS], OSStr) then
          begin
            Include(UnixLinuxSet, UnixOS);
            Found := True;
            Break;
          end;
        end;
      end;

      if not Found then
      begin
        // Try to find in Other set
        for OtherOS := Low(TPadOSOther) to High(TPadOSOther) do
        begin
          if SameText(PadOSOtherStrings[OtherOS], OSStr) then
          begin
            Include(OtherSet, OtherOS);
            Break;
          end;
        end;
      end;
    end;
  finally
    OSList.Free;
  end;
end;

// Language conversion functions
function PadLangEuropeanToString(Value: TPadLangEuropean): string;
begin
  if (Value >= Low(TPadLangEuropean)) and (Value <= High(TPadLangEuropean)) then
    Result := PadLangEuropeanStrings[Value]
  else
    Result := '';
end;

function StringToPadLangEuropean(const Value: string): TPadLangEuropean;
var
  Lang: TPadLangEuropean;
begin
  for Lang := Low(TPadLangEuropean) to High(TPadLangEuropean) do
  begin
    if SameText(PadLangEuropeanStrings[Lang], Value) then
    begin
      Result := Lang;
      Exit;
    end;
  end;
  Result := Low(TPadLangEuropean);
end;

function PadLangAsianToString(Value: TPadLangAsian): string;
begin
  if (Value >= Low(TPadLangAsian)) and (Value <= High(TPadLangAsian)) then
    Result := PadLangAsianStrings[Value]
  else
    Result := '';
end;

function StringToPadLangAsian(const Value: string): TPadLangAsian;
var
  Lang: TPadLangAsian;
begin
  for Lang := Low(TPadLangAsian) to High(TPadLangAsian) do
  begin
    if SameText(PadLangAsianStrings[Lang], Value) then
    begin
      Result := Lang;
      Exit;
    end;
  end;
  Result := Low(TPadLangAsian);
end;

function PadLangOtherMajorToString(Value: TPadLangOtherMajor): string;
begin
  if (Value >= Low(TPadLangOtherMajor)) and (Value <= High(TPadLangOtherMajor)) then
    Result := PadLangOtherMajorStrings[Value]
  else
    Result := '';
end;

function StringToPadLangOtherMajor(const Value: string): TPadLangOtherMajor;
var
  Lang: TPadLangOtherMajor;
begin
  for Lang := Low(TPadLangOtherMajor) to High(TPadLangOtherMajor) do
  begin
    if SameText(PadLangOtherMajorStrings[Lang], Value) then
    begin
      Result := Lang;
      Exit;
    end;
  end;
  Result := Low(TPadLangOtherMajor);
end;

function PadLangWorldToString(Value: TPadLangWorld): string;
begin
  if (Value >= Low(TPadLangWorld)) and (Value <= High(TPadLangWorld)) then
    Result := PadLangWorldStrings[Value]
  else
    Result := '';
end;

function StringToPadLangWorld(const Value: string): TPadLangWorld;
var
  Lang: TPadLangWorld;
begin
  for Lang := Low(TPadLangWorld) to High(TPadLangWorld) do
  begin
    if SameText(PadLangWorldStrings[Lang], Value) then
    begin
      Result := Lang;
      Exit;
    end;
  end;
  Result := Low(TPadLangWorld);
end;

// Combined Language conversion
function PadLanguagesToString(EuropeanSet: TPadLangEuropeanSet; AsianSet: TPadLangAsianSet;
  OtherMajorSet: TPadLangOtherMajorSet; WorldSet: TPadLangWorldSet): string;
var
  List: TStringList;
  TempList: TStringList;
  i, EnglishIndex: integer;
  EuroLang: TPadLangEuropean;
  AsianLang: TPadLangAsian;
  OtherLang: TPadLangOtherMajor;
  WorldLang: TPadLangWorld;
begin
  List := TStringList.Create;
  TempList := TStringList.Create;
  try
    // First pass: collect all selected languages
    for EuroLang := Low(TPadLangEuropean) to High(TPadLangEuropean) do
    begin
      if EuroLang in EuropeanSet then
        TempList.Add(PadLangEuropeanStrings[EuroLang]);
    end;

    for AsianLang := Low(TPadLangAsian) to High(TPadLangAsian) do
    begin
      if AsianLang in AsianSet then
        TempList.Add(PadLangAsianStrings[AsianLang]);
    end;

    for OtherLang := Low(TPadLangOtherMajor) to High(TPadLangOtherMajor) do
    begin
      if OtherLang in OtherMajorSet then
        TempList.Add(PadLangOtherMajorStrings[OtherLang]);
    end;

    for WorldLang := Low(TPadLangWorld) to High(TPadLangWorld) do
    begin
      if WorldLang in WorldSet then
        TempList.Add(PadLangWorldStrings[WorldLang]);
    end;

    // Sort alphabetically
    TempList.Sort;

    // Separate English from the list
    EnglishIndex := -1;
    for i := 0 to TempList.Count - 1 do
    begin
      if SameText(TempList[i], 'English') then
      begin
        EnglishIndex := i;
        Break;
      end;
    end;

    // Configure result list
    List.Delimiter := ',';
    List.StrictDelimiter := True;

    // Add English first if found
    if EnglishIndex >= 0 then
    begin
      List.Add(TempList[EnglishIndex]);
      // Remove English from temp list
      TempList.Delete(EnglishIndex);
    end;

    // Add all other languages (already sorted alphabetically)
    for i := 0 to TempList.Count - 1 do
    begin
      List.Add(TempList[i]);
    end;

    Result := List.DelimitedText;
  finally
    TempList.Free;
    List.Free;
  end;
end;

procedure StringToPadLanguages(const Value: string; out EuropeanSet: TPadLangEuropeanSet; out AsianSet: TPadLangAsianSet;
  out OtherMajorSet: TPadLangOtherMajorSet; out WorldSet: TPadLangWorldSet);
var
  LangList: TStringList;
  i: integer;
  LangStr: string;
  Found: boolean;
  EuroLang: TPadLangEuropean;
  AsianLang: TPadLangAsian;
  OtherLang: TPadLangOtherMajor;
  WorldLang: TPadLangWorld;
begin
  EuropeanSet := [];
  AsianSet := [];
  OtherMajorSet := [];
  WorldSet := [];

  if Trim(Value) = '' then
    Exit;

  LangList := TStringList.Create;
  try
    LangList.CommaText := Value;

    for i := 0 to LangList.Count - 1 do
    begin
      LangStr := Trim(LangList[i]);
      Found := False;

      // Try to find in European set
      for EuroLang := Low(TPadLangEuropean) to High(TPadLangEuropean) do
      begin
        if SameText(PadLangEuropeanStrings[EuroLang], LangStr) then
        begin
          Include(EuropeanSet, EuroLang);
          Found := True;
          Break;
        end;
      end;

      if not Found then
      begin
        // Try to find in Asian set
        for AsianLang := Low(TPadLangAsian) to High(TPadLangAsian) do
        begin
          if SameText(PadLangAsianStrings[AsianLang], LangStr) then
          begin
            Include(AsianSet, AsianLang);
            Found := True;
            Break;
          end;
        end;
      end;

      if not Found then
      begin
        // Try to find in Other Major set
        for OtherLang := Low(TPadLangOtherMajor) to High(TPadLangOtherMajor) do
        begin
          if SameText(PadLangOtherMajorStrings[OtherLang], LangStr) then
          begin
            Include(OtherMajorSet, OtherLang);
            Found := True;
            Break;
          end;
        end;
      end;

      if not Found then
      begin
        // Try to find in World set
        for WorldLang := Low(TPadLangWorld) to High(TPadLangWorld) do
        begin
          if SameText(PadLangWorldStrings[WorldLang], LangStr) then
          begin
            Include(WorldSet, WorldLang);
            Break;
          end;
        end;
      end;
    end;
  finally
    LangList.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('PAD', [TPadFormat]);
end;

end.
