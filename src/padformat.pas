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
  xmlutils,
  padconst;

type
  // Forward declarations
  TPadContactInfo = class;
  TPadSupportInfo = class;
  TPadFileInfo = class;
  TPadExpireInfo = class;
  TPadLanguageDescription = class;
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

  { TPadRoboSoft }
  // RoboSoft section for additional product identification
  TPadRoboSoft = class(TPersistent)
  private
    FCompany_UIN: string;
    FCompany_Description: string;
    FProduct_UIN: string;
    FSearch_String: string;
    FPress_Release_Search_String: string;
    FNewsFeed_Search_String: string;
    FSearch_Engine_Search_String: string;
    FWeb_Directories_Search_String: string;
    FSearch_String_Unique: string;
    FPublish_on_CD: string;
    FRSProductType: string;
    FComments_For_Reviewer: string;
    FBacklink: string;
  published
    property Company_UIN: string read FCompany_UIN write FCompany_UIN;
    property Company_Description: string read FCompany_Description write FCompany_Description;
    property Product_UIN: string read FProduct_UIN write FProduct_UIN;
    property Search_String: string read FSearch_String write FSearch_String;
    property Press_Release_Search_String: string read FPress_Release_Search_String write FPress_Release_Search_String;
    property NewsFeed_Search_String: string read FNewsFeed_Search_String write FNewsFeed_Search_String;
    property Search_Engine_Search_String: string read FSearch_Engine_Search_String write FSearch_Engine_Search_String;
    property Web_Directories_Search_String: string read FWeb_Directories_Search_String write FWeb_Directories_Search_String;
    property Search_String_Unique: string read FSearch_String_Unique write FSearch_String_Unique;
    property Publish_on_CD: string read FPublish_on_CD write FPublish_on_CD;
    property RSProductType: string read FRSProductType write FRSProductType;
    property Comments_For_Reviewer: string read FComments_For_Reviewer write FComments_For_Reviewer;
    property Backlink: string read FBacklink write FBacklink;
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
    FGooglePlusPageExists: boolean;
    FLinkedinPageExists: boolean;
    FTwitterCompanyPageExists: boolean;
    FFacebookCompanyPageExists: boolean;
    FCompanyStorePageExists: boolean;
    procedure SetGooglePlusPage(const Value: string);
    procedure SetLinkedinPage(const Value: string);
    procedure SetTwitterCompanyPage(const Value: string);
    procedure SetFacebookCompanyPage(const Value: string);
    procedure SetCompanyStorePage(const Value: string);
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
    property GooglePlusPage: string read FGooglePlusPage write SetGooglePlusPage;
    property LinkedinPage: string read FLinkedinPage write SetLinkedinPage;
    property TwitterCompanyPage: string read FTwitterCompanyPage write SetTwitterCompanyPage;
    property FacebookCompanyPage: string read FFacebookCompanyPage write SetFacebookCompanyPage;
    property CompanyStorePage: string read FCompanyStorePage write SetCompanyStorePage;
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

  { TPadSite }
  // Site section for website information submission
  TPadSite = class(TPersistent)
  private
    FSite_FORM: boolean;
    FSite_VERSION: string;
    FSite_URL: string;
    FSite_DESCRIPTION: string;
    FSite_Contact_Email: string;
    FSite_Contact_First_Name: string;
    FSite_Contact_Last_Name: string;
    FSite_Site_Title: string;
    FSite_Site_URL: string;
    FSite_Description_100: string;
    FSite_Description_250: string;
    FSite_Keywords: string;
    FSite_Description_450: string;
  published
    property Site_FORM: boolean read FSite_FORM write FSite_FORM;
    property Site_VERSION: string read FSite_VERSION write FSite_VERSION;
    property Site_URL: string read FSite_URL write FSite_URL;
    property Site_DESCRIPTION: string read FSite_DESCRIPTION write FSite_DESCRIPTION;
    property Site_Contact_Email: string read FSite_Contact_Email write FSite_Contact_Email;
    property Site_Contact_First_Name: string read FSite_Contact_First_Name write FSite_Contact_First_Name;
    property Site_Contact_Last_Name: string read FSite_Contact_Last_Name write FSite_Contact_Last_Name;
    property Site_Site_Title: string read FSite_Site_Title write FSite_Site_Title;
    property Site_Site_URL: string read FSite_Site_URL write FSite_Site_URL;
    property Site_Description_100: string read FSite_Description_100 write FSite_Description_100;
    property Site_Description_250: string read FSite_Description_250 write FSite_Description_250;
    property Site_Keywords: string read FSite_Keywords write FSite_Keywords;
    property Site_Description_450: string read FSite_Description_450 write FSite_Description_450;
  end;

  { TPadPADCertificationPromotion }
  // PAD Certification and Promotion section
  TPadPADCertificationPromotion = class(TPersistent)
  private
    FApply_For_Certification: string;
  published
    property Apply_For_Certification: string read FApply_For_Certification write FApply_For_Certification;
  end;

  { TPadDynamicPAD }
  // Dynamic PAD section for distributive information
  TPadDynamicPAD = class(TPersistent)
  private
    FDynamic_Distributive: string;
  published
    property Dynamic_Distributive: string read FDynamic_Distributive write FDynamic_Distributive;
  end;

  { TPadFileInfo }
  TPadFileInfo = class(TPersistent)
  private
    FFileSizeBytes: cardinal;
    FFileSizeK: double;
    FFileSizeMB: double;
    FFileNameVersioned: string;
    FFileNamePrevious: string;
    FFileNameGeneric: string;
    FFileNameLong: string;
    FFileNameVersionedExists: boolean;
    FFileNamePreviousExists: boolean;
    FFileNameGenericExists: boolean;
    FFileNameLongExists: boolean;
    procedure SetFileNameVersioned(const Value: string);
    procedure SetFileNamePrevious(const Value: string);
    procedure SetFileNameGeneric(const Value: string);
    procedure SetFileNameLong(const Value: string);
  published
    property FileSizeBytes: cardinal read FFileSizeBytes write FFileSizeBytes;
    property FileSizeK: double read FFileSizeK write FFileSizeK;
    property FileSizeMB: double read FFileSizeMB write FFileSizeMB;
    property FileNameVersioned: string read FFileNameVersioned write SetFileNameVersioned;
    property FileNamePrevious: string read FFileNamePrevious write SetFileNamePrevious;
    property FileNameGeneric: string read FFileNameGeneric write SetFileNameGeneric;
    property FileNameLong: string read FFileNameLong write SetFileNameLong;
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
    FProgramCostOther: string;
    FProgramType: TPadProgramType;
    FProgramReleaseStatus: TPadReleaseStatus;
    FProgramInstallSupport: TPadInstallSupport;
    // OS Support groups
    FProgramOSSupportWindows: TPadOSWindowsSet;
    FProgramOSSupportUnixLinux: TPadOSUnixLinuxSet;
    FProgramOSSupportOther: TPadOSOtherSet;
    FProgramOSSupportModern: TPadOSModernSet;
    // Language groups
    FProgramLanguageEuropean: TPadLangEuropeanSet;
    FProgramLanguageAsian: TPadLangAsianSet;
    FProgramLanguageOtherMajor: TPadLangOtherMajorSet;
    FProgramLanguageWorld: TPadLangWorldSet;

    FProgramChangeInfo: string;
    FProgramSpecificCategory: string;
    FProgramCategoryClass: TPadProgramCategoryClass;
    FProgramCategories: string;
    FProgramCategoriesExists: boolean;
    FProgramSystemRequirements: string;

    FIncludesJavaVm: boolean;
    FIncludesVbRuntime: boolean;
    FIncludesDirectX: boolean;
    FIncludesJavaVmExists: boolean;
    FIncludesVbRuntimeExists: boolean;
    FIncludesDirectXExists: boolean;

    FFileInfo: TPadFileInfo;
    FExpireInfo: TPadExpireInfo;

    procedure SetProgramCategories(const Value: string);
    procedure SetIncludesJavaVm(const Value: boolean);
    procedure SetIncludesVbRuntime(const Value: boolean);
    procedure SetIncludesDirectX(const Value: boolean);

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
    property ProgramCostOther: string read FProgramCostOther write FProgramCostOther;
    property ProgramType: TPadProgramType read FProgramType write FProgramType;
    property ProgramReleaseStatus: TPadReleaseStatus read FProgramReleaseStatus write FProgramReleaseStatus;
    property ProgramInstallSupport: TPadInstallSupport read FProgramInstallSupport write FProgramInstallSupport;

    // OS Support properties for designer
    property ProgramOSSupportWindows: TPadOSWindowsSet read FProgramOSSupportWindows write FProgramOSSupportWindows;
    property ProgramOSSupportUnixLinux: TPadOSUnixLinuxSet read FProgramOSSupportUnixLinux write FProgramOSSupportUnixLinux;
    property ProgramOSSupportOther: TPadOSOtherSet read FProgramOSSupportOther write FProgramOSSupportOther;
    property ProgramOSSupportModern: TPadOSModernSet read FProgramOSSupportModern write FProgramOSSupportModern;

    // Language properties for designer
    property ProgramLanguageEuropean: TPadLangEuropeanSet read FProgramLanguageEuropean write FProgramLanguageEuropean;
    property ProgramLanguageAsian: TPadLangAsianSet read FProgramLanguageAsian write FProgramLanguageAsian;
    property ProgramLanguageOtherMajor: TPadLangOtherMajorSet read FProgramLanguageOtherMajor write FProgramLanguageOtherMajor;
    property ProgramLanguageWorld: TPadLangWorldSet read FProgramLanguageWorld write FProgramLanguageWorld;

    property ProgramChangeInfo: string read FProgramChangeInfo write FProgramChangeInfo;
    property ProgramSpecificCategory: string read FProgramSpecificCategory write FProgramSpecificCategory;
    property ProgramCategoryClass: TPadProgramCategoryClass read FProgramCategoryClass write FProgramCategoryClass;
    property ProgramCategories: string read FProgramCategories write SetProgramCategories;
    property ProgramSystemRequirements: string read FProgramSystemRequirements write FProgramSystemRequirements;
    property IncludesJavaVm: boolean read FIncludesJavaVm write SetIncludesJavaVm default False;
    property IncludesVbRuntime: boolean read FIncludesVbRuntime write SetIncludesVbRuntime default False;
    property IncludesDirectX: boolean read FIncludesDirectX write SetIncludesDirectX default False;
    property FileInfo: TPadFileInfo read FFileInfo write FFileInfo;
    property ExpireInfo: TPadExpireInfo read FExpireInfo write FExpireInfo;
  end;

  { TPadLanguageDescription }
  TPadLanguageDescription = class(TPersistent)
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

    FCharDesc250LastLineEnding: boolean;
    FCharDesc450LastLineEnding: boolean;
    FCharDesc2000LastLineEnding: boolean;

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
    FLanguageName: string;
    FLanguage: TPadLanguageDescription;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property LanguageName: string read FLanguageName write FLanguageName;
    property Language: TPadLanguageDescription read FLanguage write FLanguage;
  end;

  { TPadApplicationURLs }
  TPadApplicationURLs = class(TPersistent)
  private
    FApplicationInfoURL: string;
    FApplicationOrderURL: string;
    FApplicationScreenshotURL: string;
    FApplicationIconURL: string;
    FApplicationXMLFileURL: string;
    FVideoLink1URLExists: boolean;
    FVideoLink1URL: string;
    FVideoLink2URLExists: boolean;
    FVideoLink2URL: string;

    procedure SetVideoLink1URL(const Value: string);
    procedure SetVideoLink2URL(const Value: string);
  published
    property ApplicationInfoURL: string read FApplicationInfoURL write FApplicationInfoURL;
    property ApplicationOrderURL: string read FApplicationOrderURL write FApplicationOrderURL;
    property ApplicationScreenshotURL: string read FApplicationScreenshotURL write FApplicationScreenshotURL;
    property ApplicationIconURL: string read FApplicationIconURL write FApplicationIconURL;
    property ApplicationXMLFileURL: string read FApplicationXMLFileURL write FApplicationXMLFileURL;
    property VideoLink1URL: string read FVideoLink1URL write SetVideoLink1URL;
    property VideoLink2URL: string read FVideoLink2URL write SetVideoLink2URL;
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
    FASPMemberNumber: string;
  published
    property ASPForm: boolean read FASPForm write FASPForm;
    property ASPMember: boolean read FASPMember write FASPMember;
    property ASPMemberNumber: string read FASPMemberNumber write FASPMemberNumber;
  end;

  { TXmlConfig }
  TPadXmlCofig = class(TPersistent)
  private
    FXMLEncoding: TPadEncoding;
    FXMLUseTabIndent: boolean;
    FXMLIndentSize: integer;
    FXMLEmptyTagType: TPadEmptyTagType;
    FXMLEndsWithLineBreak: boolean;
  public
    constructor Create;
  published
    property XMLEncoding: TPadEncoding read FXMLEncoding write FXMLEncoding default peUTF8;
    property XMLUseTabIndent: boolean read FXMLUseTabIndent write FXMLUseTabIndent default False;
    property XMLIndentSize: integer read FXMLIndentSize write FXMLIndentSize;
    property XMLEmptyTagType: TPadEmptyTagType read FXMLEmptyTagType write FXMLEmptyTagType default ettWithoutSpace;
    property XMLEndsWithLineBreak: boolean read FXMLEndsWithLineBreak write FXMLEndsWithLineBreak default False;
  end;

  { TPadFormat }
  TPadFormat = class(TComponent)
  private
    FXmlConfig: TPadXmlCofig;
    FMasterPadVersionInfo: TPadMasterVersionInfo;
    FRoboSoft: TPadRoboSoft;
    FCompanyInfo: TPadCompanyInfo;
    FNewsFeed: TPadNewsFeed;
    FSite: TPadSite;
    FPAD_Certification_Promotion: TPadPADCertificationPromotion;
    FDynamic_PAD: TPadDynamicPAD;
    FProgramInfo: TPadProgramInfo;
    FProgramDescriptions: TPadProgramDescriptions;
    FWebInfo: TPadWebInfo;
    FPermissions: TPadPermissions;
    FAffiliates: TPadAffiliates;
    FASP: TPadASP;
    function SetNodeText(Doc: TXMLDocument; ParentNode: TDOMNode; NodeName, NodeValue: string): TDOMNode;
    function AddChildNode(ParentNode: TDOMNode; NodeName: string): TDOMNode;
    procedure SetNodeTextValue(Node: TDOMNode; Value: string);
    // Helper functions for XML formatting
    procedure SetEncodingByString(const EncodingStr: string);
    function DetectEncodingFromString(const XMLContent: string): TPadEncoding;
    function DetectIndentationStyle(const XMLContent: string): boolean;
    function DetectIndentSize(const XMLContent: string): integer;
    function DetectEmptyTagType(const XMLContent: string): TPadEmptyTagType;
    function ConvertIndentation(const XMLString: string; UseTabs: boolean; IndentSize: integer): string;
    function ConvertEmptyTags(const XMLString: string; EmptyTagType: TPadEmptyTagType): string;
    function SetXMLDeclaration(XMLString: string; XMLVersion: string; Encoding: TPadEncoding): string;
    function RemoveXMLDeclaration(const XMLString: string): string;
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
    property XmlConfig: TPadXmlCofig read FXmlConfig write FXmlConfig;
    property MasterPadVersionInfo: TPadMasterVersionInfo read FMasterPadVersionInfo write FMasterPadVersionInfo;
    property RoboSoft: TPadRoboSoft read FRoboSoft write FRoboSoft;
    property CompanyInfo: TPadCompanyInfo read FCompanyInfo write FCompanyInfo;
    property NewsFeed: TPadNewsFeed read FNewsFeed write FNewsFeed;
    property Site: TPadSite read FSite write FSite;
    property PAD_Certification_Promotion: TPadPADCertificationPromotion read FPAD_Certification_Promotion
      write FPAD_Certification_Promotion;
    property Dynamic_PAD: TPadDynamicPAD read FDynamic_PAD write FDynamic_PAD;
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
function PadOSModernToString(Value: TPadOSModern): string;
function StringToPadOSModern(const Value: string): TPadOSModern;

// Combined OS Support conversion
function PadOSSupportToString(WindowsSet: TPadOSWindowsSet; UnixLinuxSet: TPadOSUnixLinuxSet; OtherSet: TPadOSOtherSet;
  ModernSet: TPadOSModernSet): string;
procedure StringToPadOSSupport(const Value: string; out WindowsSet: TPadOSWindowsSet; out UnixLinuxSet: TPadOSUnixLinuxSet;
  out OtherSet: TPadOSOtherSet; out ModernSet: TPadOSModernSet);

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

{ TPadFileInfo }

procedure TPadFileInfo.SetFileNameVersioned(const Value: string);
begin
  if FFileNameVersioned <> Value then
  begin
    FFileNameVersioned := Value;
    FFileNameVersionedExists := True;
  end;
end;

procedure TPadFileInfo.SetFileNamePrevious(const Value: string);
begin
  if FFileNamePrevious <> Value then
  begin
    FFileNamePrevious := Value;
    FFileNamePreviousExists := True;
  end;
end;

procedure TPadFileInfo.SetFileNameGeneric(const Value: string);
begin
  if FFileNameGeneric <> Value then
  begin
    FFileNameGeneric := Value;
    FFileNameGenericExists := True;
  end;
end;

procedure TPadFileInfo.SetFileNameLong(const Value: string);
begin
  if FFileNameLong <> Value then
  begin
    FFileNameLong := Value;
    FFileNameLongExists := True;
  end;
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
  Result := PadOSSupportToString(FProgramOSSupportWindows, FProgramOSSupportUnixLinux, FProgramOSSupportOther, FProgramOSSupportModern);
end;

procedure TPadProgramInfo.SetProgramOSSupportAsString(const Value: string);
begin
  StringToPadOSSupport(Value, FProgramOSSupportWindows, FProgramOSSupportUnixLinux, FProgramOSSupportOther, FProgramOSSupportModern);
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

procedure TPadProgramInfo.SetProgramCategories(const Value: string);
begin
  if FProgramCategories <> Value then
  begin
    FProgramCategories := Value;
    // Set Exists flag to True when value is assigned
    FProgramCategoriesExists := True;
  end;
end;

procedure TPadProgramInfo.SetIncludesJavaVm(const Value: boolean);
begin
  if FIncludesJavaVm <> Value then
  begin
    FIncludesJavaVm := Value;
    // Set Exists flag to True when value is assigned
    FIncludesJavaVmExists := True;
  end;
end;

procedure TPadProgramInfo.SetIncludesVbRuntime(const Value: boolean);
begin
  if FIncludesVbRuntime <> Value then
  begin
    FIncludesVbRuntime := Value;
    // Set Exists flag to True when value is assigned
    FIncludesVbRuntimeExists := True;
  end;
end;

procedure TPadProgramInfo.SetIncludesDirectX(const Value: boolean);
begin
  if FIncludesDirectX <> Value then
  begin
    FIncludesDirectX := Value;
    // Set Exists flag to True when value is assigned
    FIncludesDirectXExists := True;
  end;
end;

{ TPadCompanyInfo }

constructor TPadCompanyInfo.Create;
begin
  inherited Create;
  FContactInfo := TPadContactInfo.Create;
  FSupportInfo := TPadSupportInfo.Create;

  FGooglePlusPageExists := False;
  FLinkedinPageExists := False;
  FTwitterCompanyPageExists := False;
  FFacebookCompanyPageExists := False;
  FCompanyStorePageExists := False;
end;

destructor TPadCompanyInfo.Destroy;
begin
  FContactInfo.Free;
  FSupportInfo.Free;
  inherited Destroy;
end;

procedure TPadCompanyInfo.SetGooglePlusPage(const Value: string);
begin
  if FGooglePlusPage <> Value then
  begin
    FGooglePlusPage := Value;
    FGooglePlusPageExists := True;
  end;
end;

procedure TPadCompanyInfo.SetLinkedinPage(const Value: string);
begin
  if FLinkedinPage <> Value then
  begin
    FLinkedinPage := Value;
    FLinkedinPageExists := True;
  end;
end;

procedure TPadCompanyInfo.SetTwitterCompanyPage(const Value: string);
begin
  if FTwitterCompanyPage <> Value then
  begin
    FTwitterCompanyPage := Value;
    FTwitterCompanyPageExists := True;
  end;
end;

procedure TPadCompanyInfo.SetFacebookCompanyPage(const Value: string);
begin
  if FFacebookCompanyPage <> Value then
  begin
    FFacebookCompanyPage := Value;
    FFacebookCompanyPageExists := True;
  end;
end;

procedure TPadCompanyInfo.SetCompanyStorePage(const Value: string);
begin
  if FCompanyStorePage <> Value then
  begin
    FCompanyStorePage := Value;
    FCompanyStorePageExists := True;
  end;
end;

{ TPadLanguageDescription }

constructor TPadLanguageDescription.Create;
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

destructor TPadLanguageDescription.Destroy;
begin
  FCharDesc250Strings.Free;
  FCharDesc450Strings.Free;
  FCharDesc2000Strings.Free;
  inherited Destroy;
end;

function TPadLanguageDescription.GetCharDesc250Strings: TStrings;
begin
  // Always return the TStrings object
  Result := FCharDesc250Strings;
end;

procedure TPadLanguageDescription.SetCharDesc250Strings(Value: TStrings);
begin
  if Assigned(Value) then
    FCharDesc250Strings.Assign(Value)
  else
    FCharDesc250Strings.Clear;
end;

function TPadLanguageDescription.GetCharDesc450Strings: TStrings;
begin
  Result := FCharDesc450Strings;
end;

procedure TPadLanguageDescription.SetCharDesc450Strings(Value: TStrings);
begin
  if Assigned(Value) then
    FCharDesc450Strings.Assign(Value)
  else
    FCharDesc450Strings.Clear;
end;

function TPadLanguageDescription.GetCharDesc2000Strings: TStrings;
begin
  Result := FCharDesc2000Strings;
end;

procedure TPadLanguageDescription.SetCharDesc2000Strings(Value: TStrings);
begin
  if Assigned(Value) then
    FCharDesc2000Strings.Assign(Value)
  else
    FCharDesc2000Strings.Clear;
end;

// Call this after loading from XML to populate TStrings from string fields
procedure TPadLanguageDescription.SyncStringsToStrings;
begin
  FCharDesc250LastLineEnding := EndsStr(#10, FCharDesc250) or EndsStr(#13, FCharDesc250);
  FCharDesc450LastLineEnding := EndsStr(#10, FCharDesc450) or EndsStr(#13, FCharDesc450);
  FCharDesc2000LastLineEnding := EndsStr(#10, FCharDesc2000) or EndsStr(#13, FCharDesc2000);
  FCharDesc250Strings.Text := FCharDesc250;
  FCharDesc450Strings.Text := FCharDesc450;
  FCharDesc2000Strings.Text := FCharDesc2000;
end;

// Call this before saving to XML to update string fields from TStrings
procedure TPadLanguageDescription.SyncStringToStrings;
begin
  FCharDesc250Strings.TrailingLineBreak := FCharDesc250LastLineEnding;
  FCharDesc450Strings.TrailingLineBreak := FCharDesc450LastLineEnding;
  FCharDesc2000Strings.TrailingLineBreak := FCharDesc2000LastLineEnding;
  FCharDesc250 := FCharDesc250Strings.Text;
  FCharDesc450 := FCharDesc450Strings.Text;
  FCharDesc2000 := FCharDesc2000Strings.Text;
end;

{ TPadProgramDescriptions }

constructor TPadProgramDescriptions.Create;
begin
  inherited Create;
  FLanguage := TPadLanguageDescription.Create;
  FLanguageName := 'English';
end;

destructor TPadProgramDescriptions.Destroy;
begin
  FLanguage.Free;
  inherited Destroy;
end;

{ TPadApplicationURLs }

procedure TPadApplicationURLs.SetVideoLink1URL(const Value: string);
begin
  if FVideoLink1URL <> Value then
  begin
    FVideoLink1URL := Value;
    FVideoLink1URLExists := True;
  end;
end;

procedure TPadApplicationURLs.SetVideoLink2URL(const Value: string);
begin
  if FVideoLink2URL <> Value then
  begin
    FVideoLink2URL := Value;
    FVideoLink2URLExists := True;
  end;
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

{ TXmlConfig }
constructor TPadXmlCofig.Create;
begin
  // Initialize XML formatting options with default values
  FXMLEncoding := peUTF8;           // Default XML encoding
  FXMLUseTabIndent := False;         // Use spaces by default
  FXMLIndentSize := 2;               // 2 spaces indentation
  FXMLEmptyTagType := ettWithoutSpace; // Default empty tag format <Empty/>
  FXMLEndsWithLineBreak := False;
end;

{ TPadFormat }

constructor TPadFormat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXmlConfig := TPadXmlCofig.Create;
  FMasterPadVersionInfo := TPadMasterVersionInfo.Create;
  FRoboSoft := TPadRoboSoft.Create;
  FCompanyInfo := TPadCompanyInfo.Create;
  FNewsFeed := TPadNewsFeed.Create;
  FSite := TPadSite.Create;
  FPAD_Certification_Promotion := TPadPADCertificationPromotion.Create;
  FDynamic_PAD := TPadDynamicPAD.Create;
  FProgramInfo := TPadProgramInfo.Create;
  FProgramDescriptions := TPadProgramDescriptions.Create;
  FWebInfo := TPadWebInfo.Create;
  FPermissions := TPadPermissions.Create;
  FAffiliates := TPadAffiliates.Create;
  FASP := TPadASP.Create;
end;

destructor TPadFormat.Destroy;
begin
  FXmlConfig.Free;
  FMasterPadVersionInfo.Free;
  FRoboSoft.Free;
  FCompanyInfo.Free;
  FNewsFeed.Free;
  FSite.Free;
  FPAD_Certification_Promotion.Free;
  FDynamic_PAD.Free;
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

  // Detect XML formatting options from content
  FXmlConfig.XMLEncoding := DetectEncodingFromString(XMLContent);
  FXmlConfig.XMLUseTabIndent := DetectIndentationStyle(XMLContent);
  FXmlConfig.XMLIndentSize := DetectIndentSize(XMLContent);
  FXmlConfig.XMLEmptyTagType := DetectEmptyTagType(XMLContent);
  FXmlConfig.XMLEndsWithLineBreak := EndsStr(LineEnding, XMLContent);

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

      // Load RoboSoft section
      Node := RootNode.FindNode('RoboSoft');
      if Assigned(Node) then
      begin
        FRoboSoft.Company_UIN := GetNodeValue(Node, 'Company_UIN');
        FRoboSoft.Company_Description := GetNodeValue(Node, 'Company_Description');
        FRoboSoft.Product_UIN := GetNodeValue(Node, 'Product_UIN');
        FRoboSoft.Search_String := GetNodeValue(Node, 'Search_String');
        FRoboSoft.Press_Release_Search_String := GetNodeValue(Node, 'Press_Release_Search_String');
        FRoboSoft.NewsFeed_Search_String := GetNodeValue(Node, 'NewsFeed_Search_String');
        FRoboSoft.Search_Engine_Search_String := GetNodeValue(Node, 'Search_Engine_Search_String');
        FRoboSoft.Web_Directories_Search_String := GetNodeValue(Node, 'Web_Directories_Search_String');
        FRoboSoft.Search_String_Unique := GetNodeValue(Node, 'Search_String_Unique');
        FRoboSoft.Publish_on_CD := GetNodeValue(Node, 'Publish_on_CD');
        FRoboSoft.RSProductType := GetNodeValue(Node, 'RSProductType');
        FRoboSoft.Comments_For_Reviewer := GetNodeValue(Node, 'Comments_For_Reviewer');
        FRoboSoft.Backlink := GetNodeValue(Node, 'Backlink');
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
        FCompanyInfo.FGooglePlusPageExists := Assigned(Node.FindNode('GooglePlusPage'));
        FCompanyInfo.GooglePlusPage := GetNodeValue(Node, 'GooglePlusPage');
        FCompanyInfo.FLinkedinPageExists := Assigned(Node.FindNode('LinkedinPage'));
        FCompanyInfo.LinkedinPage := GetNodeValue(Node, 'LinkedinPage');
        FCompanyInfo.FTwitterCompanyPageExists := Assigned(Node.FindNode('TwitterCompanyPage'));
        FCompanyInfo.TwitterCompanyPage := GetNodeValue(Node, 'TwitterCompanyPage');
        FCompanyInfo.FFacebookCompanyPageExists := Assigned(Node.FindNode('FacebookCompanyPage'));
        FCompanyInfo.FacebookCompanyPage := GetNodeValue(Node, 'FacebookCompanyPage');
        FCompanyInfo.FCompanyStorePageExists := Assigned(Node.FindNode('CompanyStorePage'));
        FCompanyInfo.CompanyStorePage := GetNodeValue(Node, 'CompanyStorePage');
      end;

      // Load News Feed (updated with new fields)
      Node := RootNode.FindNode('NewsFeed');
      if Assigned(Node) then
      begin
        FNewsFeed.NewsFeed_FORM := True;
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
      end
      else
        FNewsFeed.NewsFeed_FORM := False;

      // Load Site section
      Node := RootNode.FindNode('Site');
      if Assigned(Node) then
      begin
        FSite.Site_FORM := True;
        FSite.Site_VERSION := GetNodeValue(Node, 'Site_VERSION');
        FSite.Site_URL := GetNodeValue(Node, 'Site_URL');
        FSite.Site_DESCRIPTION := GetNodeValue(Node, 'Site_DESCRIPTION');
        FSite.Site_Contact_Email := GetNodeValue(Node, 'Site_Contact_Email');
        FSite.Site_Contact_First_Name := GetNodeValue(Node, 'Site_Contact_First_Name');
        FSite.Site_Contact_Last_Name := GetNodeValue(Node, 'Site_Contact_Last_Name');
        FSite.Site_Site_Title := GetNodeValue(Node, 'Site_Site_Title');
        FSite.Site_Site_URL := GetNodeValue(Node, 'Site_Site_URL');
        FSite.Site_Description_100 := GetNodeValue(Node, 'Site_Description_100');
        FSite.Site_Description_250 := GetNodeValue(Node, 'Site_Description_250');
        FSite.Site_Keywords := GetNodeValue(Node, 'Site_Keywords');
        FSite.Site_Description_450 := GetNodeValue(Node, 'Site_Description_450');
      end
      else
        FSite.Site_FORM := False;

      // Load PAD Certification Promotion
      Node := RootNode.FindNode('PAD_Certification_Promotion');
      if Assigned(Node) then
        FPAD_Certification_Promotion.Apply_For_Certification := GetNodeValue(Node, 'Apply_For_Certification');

      // Load Dynamic PAD
      Node := RootNode.FindNode('Dynamic_PAD');
      if Assigned(Node) then
        FDynamic_PAD.Dynamic_Distributive := GetNodeValue(Node, 'Dynamic_Distributive');

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
        FProgramInfo.ProgramCostOther := GetNodeValue(Node, 'Program_Cost_Other');

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
        FProgramInfo.FProgramCategoriesExists := Assigned(Node.FindNode('Program_Categories'));
        FProgramInfo.ProgramCategories := GetNodeValue(Node, 'Program_Categories');
        FProgramInfo.ProgramSystemRequirements := GetNodeValue(Node, 'Program_System_Requirements');

        FProgramInfo.FIncludesJavaVmExists := Assigned(Node.FindNode('Includes_JAVA_VM'));
        FProgramInfo.IncludesJavaVm := UpperCase(GetNodeValue(Node, 'Includes_JAVA_VM')) = 'Y';
        FProgramInfo.FIncludesVbRuntimeExists := Assigned(Node.FindNode('Includes_VB_Runtime'));
        FProgramInfo.IncludesVbRuntime := UpperCase(GetNodeValue(Node, 'Includes_VB_Runtime')) = 'Y';
        FProgramInfo.FIncludesDirectXExists := Assigned(Node.FindNode('Includes_DirectX'));
        FProgramInfo.IncludesDirectX := UpperCase(GetNodeValue(Node, 'Includes_DirectX')) = 'Y';

        // Load File Info
        SubNode := Node.FindNode('File_Info');
        if Assigned(SubNode) then
        begin
          FProgramInfo.FileInfo.FileSizeBytes := StrToInt64Safe(GetNodeValue(SubNode, 'File_Size_Bytes'));
          FProgramInfo.FileInfo.FileSizeK := StrToFloatSafe(GetNodeValue(SubNode, 'File_Size_K'));
          FProgramInfo.FileInfo.FileSizeMB := StrToFloatSafe(GetNodeValue(SubNode, 'File_Size_MB'));
          FProgramInfo.FileInfo.FFileNameVersionedExists := Assigned(SubNode.FindNode('Filename_Versioned'));
          FProgramInfo.FileInfo.FileNameVersioned := GetNodeValue(SubNode, 'Filename_Versioned');
          FProgramInfo.FileInfo.FFileNamePreviousExists := Assigned(SubNode.FindNode('Filename_Previous'));
          FProgramInfo.FileInfo.FileNamePrevious := GetNodeValue(SubNode, 'Filename_Previous');
          FProgramInfo.FileInfo.FFileNameGenericExists := Assigned(SubNode.FindNode('Filename_Generic'));
          FProgramInfo.FileInfo.FileNameGeneric := GetNodeValue(SubNode, 'Filename_Generic');
          FProgramInfo.FileInfo.FFileNameLongExists := Assigned(SubNode.FindNode('Filename_Long'));
          FProgramInfo.FileInfo.FileNameLong := GetNodeValue(SubNode, 'Filename_Long');
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
        SubNode := Node.FirstChild;
        FProgramDescriptions.FLanguageName := utf8string(SubNode.NodeName);
        if Assigned(SubNode) then
        begin
          FProgramDescriptions.FLanguage.Keywords := GetNodeValue(SubNode, 'Keywords');
          FProgramDescriptions.FLanguage.CharDesc45 := GetNodeValue(SubNode, 'Char_Desc_45');
          FProgramDescriptions.FLanguage.CharDesc80 := GetNodeValue(SubNode, 'Char_Desc_80');
          FProgramDescriptions.FLanguage.CharDesc250 := GetNodeValue(SubNode, 'Char_Desc_250');
          FProgramDescriptions.FLanguage.CharDesc450 := GetNodeValue(SubNode, 'Char_Desc_450');
          FProgramDescriptions.FLanguage.CharDesc2000 := GetNodeValue(SubNode, 'Char_Desc_2000');
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
          FWebInfo.ApplicationURLs.FVideoLink1URLExists := Assigned(SubNode.FindNode('Video_Link_1_URL'));
          FWebInfo.ApplicationURLs.VideoLink1URL := GetNodeValue(SubNode, 'Video_Link_1_URL');
          FWebInfo.ApplicationURLs.FVideoLink2URLExists := Assigned(SubNode.FindNode('Video_Link_2_URL'));
          FWebInfo.ApplicationURLs.VideoLink2URL := GetNodeValue(SubNode, 'Video_Link_2_URL');
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
        FASP.ASPForm := True;
        FASP.ASPMember := UpperCase(GetNodeValue(Node, 'ASP_Member')) = 'Y';
        FASP.ASPMemberNumber := GetNodeValue(Node, 'ASP_Member_Number');
      end
      else
        FASP.ASPForm := False;

      FProgramDescriptions.Language.SyncStringsToStrings;
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
  XMLContent: string;
begin
  Doc := TXMLDocument.Create;
  try
    FProgramDescriptions.Language.SyncStringToStrings;
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

    // RoboSoft section
    if (MasterPadVersionInfo.Version >= 4) then
    begin
      if (FRoboSoft.Company_UIN <> '') or (FRoboSoft.Company_Description <> '') or (FRoboSoft.Product_UIN <> '') or
        (FRoboSoft.Search_String <> '') then
      begin
        Node := AddChildNode(RootNode, 'RoboSoft');
        SetNodeText(Doc, Node, 'Company_UIN', FRoboSoft.Company_UIN);
        SetNodeText(Doc, Node, 'Company_Description', FRoboSoft.Company_Description);
        SetNodeText(Doc, Node, 'Product_UIN', FRoboSoft.Product_UIN);
        SetNodeText(Doc, Node, 'Search_String', FRoboSoft.Search_String);
        SetNodeText(Doc, Node, 'Press_Release_Search_String', FRoboSoft.Press_Release_Search_String);
        SetNodeText(Doc, Node, 'NewsFeed_Search_String', FRoboSoft.NewsFeed_Search_String);
        SetNodeText(Doc, Node, 'Search_Engine_Search_String', FRoboSoft.Search_Engine_Search_String);
        SetNodeText(Doc, Node, 'Web_Directories_Search_String', FRoboSoft.Web_Directories_Search_String);
        SetNodeText(Doc, Node, 'Search_String_Unique', FRoboSoft.Search_String_Unique);
        SetNodeText(Doc, Node, 'Publish_on_CD', FRoboSoft.Publish_on_CD);
        SetNodeText(Doc, Node, 'RSProductType', FRoboSoft.RSProductType);
        SetNodeText(Doc, Node, 'Comments_For_Reviewer', FRoboSoft.Comments_For_Reviewer);
        SetNodeText(Doc, Node, 'Backlink', FRoboSoft.Backlink);
      end;
    end;

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
      if FCompanyInfo.FGooglePlusPageExists then
        SetNodeText(Doc, Node, 'GooglePlusPage', FCompanyInfo.GooglePlusPage);
      if FCompanyInfo.FLinkedinPageExists then
        SetNodeText(Doc, Node, 'LinkedinPage', FCompanyInfo.LinkedinPage);
      if FCompanyInfo.FTwitterCompanyPageExists then
        SetNodeText(Doc, Node, 'TwitterCompanyPage', FCompanyInfo.TwitterCompanyPage);
      if FCompanyInfo.FFacebookCompanyPageExists then
        SetNodeText(Doc, Node, 'FacebookCompanyPage', FCompanyInfo.FacebookCompanyPage);
      if FCompanyInfo.FCompanyStorePageExists then
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

      // Save Site section
      if FSite.Site_FORM then
      begin
        Node := AddChildNode(RootNode, 'Site');
        SetNodeText(Doc, Node, 'Site_FORM', BoolToStr(FSite.Site_FORM, 'Y', 'N'));
        SetNodeText(Doc, Node, 'Site_VERSION', FSite.Site_VERSION);
        SetNodeText(Doc, Node, 'Site_URL', FSite.Site_URL);
        SetNodeText(Doc, Node, 'Site_DESCRIPTION', FSite.Site_DESCRIPTION);
        SetNodeText(Doc, Node, 'Site_Contact_Email', FSite.Site_Contact_Email);
        SetNodeText(Doc, Node, 'Site_Contact_First_Name', FSite.Site_Contact_First_Name);
        SetNodeText(Doc, Node, 'Site_Contact_Last_Name', FSite.Site_Contact_Last_Name);
        SetNodeText(Doc, Node, 'Site_Site_Title', FSite.Site_Site_Title);
        SetNodeText(Doc, Node, 'Site_Site_URL', FSite.Site_Site_URL);
        SetNodeText(Doc, Node, 'Site_Description_100', FSite.Site_Description_100);
        SetNodeText(Doc, Node, 'Site_Description_250', FSite.Site_Description_250);
        SetNodeText(Doc, Node, 'Site_Keywords', FSite.Site_Keywords);
        SetNodeText(Doc, Node, 'Site_Description_450', FSite.Site_Description_450);
      end;

      // Save PAD Certification Promotion
      if (FPAD_Certification_Promotion.Apply_For_Certification <> '') then
      begin
        Node := AddChildNode(RootNode, 'PAD_Certification_Promotion');
        SetNodeText(Doc, Node, 'Apply_For_Certification', FPAD_Certification_Promotion.Apply_For_Certification);
      end;

      // Save Dynamic PAD
      if (FDynamic_PAD.Dynamic_Distributive <> '') then
      begin
        Node := AddChildNode(RootNode, 'Dynamic_PAD');
        SetNodeText(Doc, Node, 'Dynamic_Distributive', FDynamic_PAD.Dynamic_Distributive);
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
      FProgramInfo.ProgramCostOther);
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
    if FProgramInfo.FProgramCategoriesExists then
      SetNodeText(Doc, Node, 'Program_Categories',
        FProgramInfo.ProgramCategories);
    SetNodeText(Doc, Node, 'Program_System_Requirements',
      FProgramInfo.ProgramSystemRequirements);

    if FProgramInfo.FIncludesJavaVmExists then
      SetNodeText(Doc, Node, 'Includes_JAVA_VM',
        BoolToStr(FProgramInfo.IncludesJavaVm, 'Y', 'N'));
    if FProgramInfo.FIncludesVbRuntimeExists then
      SetNodeText(Doc, Node, 'Includes_VB_Runtime',
        BoolToStr(FProgramInfo.FIncludesVbRuntime, 'Y', 'N'));
    if FProgramInfo.FIncludesDirectXExists then
      SetNodeText(Doc, Node, 'Includes_DirectX',
        BoolToStr(FProgramInfo.FIncludesDirectX, 'Y', 'N'));

    // File Info
    SubNode := AddChildNode(Node, 'File_Info');

    if FProgramInfo.FileInfo.FFileNameVersionedExists then
      SetNodeText(Doc, SubNode, 'Filename_Versioned', FProgramInfo.FileInfo.FFileNameVersioned);
    if FProgramInfo.FileInfo.FFileNamePreviousExists then
      SetNodeText(Doc, SubNode, 'Filename_Previous', FProgramInfo.FileInfo.FFileNamePrevious);
    if FProgramInfo.FileInfo.FFileNameGenericExists then
      SetNodeText(Doc, SubNode, 'Filename_Generic', FProgramInfo.FileInfo.FFileNameGeneric);
    if FProgramInfo.FileInfo.FFileNameLongExists then
      SetNodeText(Doc, SubNode, 'Filename_Long', FProgramInfo.FileInfo.FFileNameLong);

    SetNodeText(Doc, SubNode, 'File_Size_Bytes',
      IntToStr(FProgramInfo.FileInfo.FileSizeBytes));
    FS.DecimalSeparator := '.';
    if Frac(FProgramInfo.FileInfo.FileSizeK) = 0 then
      SetNodeText(Doc, SubNode, 'File_Size_K',
        FormatFloat('0.####', FProgramInfo.FileInfo.FileSizeK, FS))
    else
      SetNodeText(Doc, SubNode, 'File_Size_K',
        FormatFloat('0.00##', FProgramInfo.FileInfo.FileSizeK, FS));
    if Frac(FProgramInfo.FileInfo.FileSizeMB) = 0 then
      SetNodeText(Doc, SubNode, 'File_Size_MB',
        FormatFloat('0.####', FProgramInfo.FileInfo.FileSizeMB, FS))
    else
      SetNodeText(Doc, SubNode, 'File_Size_MB',
        FormatFloat('0.00##', FProgramInfo.FileInfo.FileSizeMB, FS));

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
    SubNode := AddChildNode(Node, FProgramDescriptions.LanguageName);
    SetNodeText(Doc, SubNode, 'Keywords',
      FProgramDescriptions.Language.Keywords);
    SetNodeText(Doc, SubNode, 'Char_Desc_45',
      FProgramDescriptions.Language.CharDesc45);
    SetNodeText(Doc, SubNode, 'Char_Desc_80',
      FProgramDescriptions.Language.CharDesc80);
    SetNodeText(Doc, SubNode, 'Char_Desc_250',
      FProgramDescriptions.Language.CharDesc250);
    SetNodeText(Doc, SubNode, 'Char_Desc_450',
      FProgramDescriptions.Language.CharDesc450);
    SetNodeText(Doc, SubNode, 'Char_Desc_2000',
      FProgramDescriptions.Language.CharDesc2000);

    // Web Info
    Node := AddChildNode(RootNode, 'Web_Info');
    SubNode := AddChildNode(Node, 'Application_URLs');
    if MasterPadVersionInfo.Version >= 4 then
    begin
      if FWebInfo.ApplicationURLs.FVideoLink1URLExists then
        SetNodeText(Doc, SubNode, 'Video_Link_1_URL',
          FWebInfo.ApplicationURLs.VideoLink1URL);
      if FWebInfo.ApplicationURLs.FVideoLink2URLExists then
        SetNodeText(Doc, SubNode, 'Video_Link_2_URL',
          FWebInfo.ApplicationURLs.VideoLink2URL);
    end;
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

    // Check if we should save full section
    SaveFullSection := FAffiliates.ShouldSaveFullSection;

    // Save Affiliates
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
      SetNodeText(Doc, Node, 'ASP_Member_Number', FASP.ASPMemberNumber);
    end;

    // Save to string
    Stream := TStringStream.Create('');
    try
      WriteXML(Doc, Stream);
      XMLContent := Stream.DataString;
    finally
      Stream.Free;
    end;

    // Declaration Replace
    XMLContent := RemoveXMLDeclaration(XMLContent);
    XMLContent := SetXMLDeclaration(XmlContent, UTF8Encode(Doc.XMLVersion), FXmlConfig.XMLEncoding);

    // Apply empty tag formatting if needed
    if FXmlConfig.XMLEmptyTagType <> ettWithoutSpace then
      XMLContent := ConvertEmptyTags(XMLContent, FXmlConfig.XMLEmptyTagType);

    if (FXmlConfig.XMLUseTabIndent) or (FXmlConfig.XMLIndentSize <> 2) then
      // Only convert if using tabs or custom space count
      Result := ConvertIndentation(XMLContent, FXmlConfig.XMLUseTabIndent, FXmlConfig.XMLIndentSize)
    else
      // Keep original 2-space indentation
      Result := XMLContent;
    if FXmlConfig.XMLEndsWithLineBreak then
      Result += LineEnding;
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

  // Clear RoboSoft
  FRoboSoft.Company_UIN := '';
  FRoboSoft.Company_Description := '';
  FRoboSoft.Product_UIN := '';
  FRoboSoft.Search_String := '';
  FRoboSoft.Press_Release_Search_String := '';
  FRoboSoft.NewsFeed_Search_String := '';
  FRoboSoft.Search_Engine_Search_String := '';
  FRoboSoft.Web_Directories_Search_String := '';
  FRoboSoft.Search_String_Unique := '';
  FRoboSoft.Publish_on_CD := '';
  FRoboSoft.RSProductType := '';
  FRoboSoft.Comments_For_Reviewer := '';
  FRoboSoft.Backlink := '';

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
  FCompanyInfo.FGooglePlusPageExists := False;
  FCompanyInfo.FLinkedinPageExists := False;
  FCompanyInfo.FTwitterCompanyPageExists := False;
  FCompanyInfo.FFacebookCompanyPageExists := False;
  FCompanyInfo.FCompanyStorePageExists := False;

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

  // Clear Site section
  FSite.Site_FORM := False;
  FSite.Site_VERSION := '1.0';
  FSite.Site_URL := 'http://Submit-Everywhere.com/extensions/Site.htm';
  FSite.Site_DESCRIPTION :=
    'This PAD extension allows you to add your site info into your PAD file. This info can be used by site submission software or by web directories themselves.';
  FSite.Site_Contact_Email := '';
  FSite.Site_Contact_First_Name := '';
  FSite.Site_Contact_Last_Name := '';
  FSite.Site_Site_Title := '';
  FSite.Site_Site_URL := '';
  FSite.Site_Description_100 := '';
  FSite.Site_Description_250 := '';
  FSite.Site_Keywords := '';
  FSite.Site_Description_450 := '';

  // Clear PAD Certification Promotion
  FPAD_Certification_Promotion.Apply_For_Certification := '';

  // Clear Dynamic PAD
  FDynamic_PAD.Dynamic_Distributive := '';

  // Clear Program Info
  FProgramInfo.ProgramName := '';
  FProgramInfo.ProgramVersion := '';
  FProgramInfo.ProgramReleaseMonth := 0;
  FProgramInfo.ProgramReleaseDay := 0;
  FProgramInfo.ProgramReleaseYear := 0;
  FProgramInfo.ProgramCostDollars := '0';
  FProgramInfo.ProgramCostOtherCode := '';
  FProgramInfo.ProgramCostOther := '';
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
  FProgramInfo.ProgramCategories := '';
  FProgramInfo.ProgramSystemRequirements := '';
  FProgramInfo.IncludesJavaVm := False;
  FProgramInfo.IncludesVbRuntime := False;
  FProgramInfo.IncludesDirectX := False;
  FProgramInfo.FIncludesJavaVmExists := False;
  FProgramInfo.FIncludesVbRuntimeExists := False;
  FProgramInfo.FIncludesDirectXExists := False;

  // Clear File Info
  FProgramInfo.FileInfo.FileSizeBytes := 0;
  FProgramInfo.FileInfo.FileSizeK := 0;
  FProgramInfo.FileInfo.FileSizeMB := 0;
  FProgramInfo.FileInfo.FFileNameVersioned := '';
  FProgramInfo.FileInfo.FFileNameVersionedExists := False;
  FProgramInfo.FileInfo.FFileNamePrevious := '';
  FProgramInfo.FileInfo.FFileNamePreviousExists := False;
  FProgramInfo.FileInfo.FFileNameGeneric := '';
  FProgramInfo.FileInfo.FFileNameGenericExists := False;
  FProgramInfo.FileInfo.FFileNameLong := '';
  FProgramInfo.FileInfo.FFileNameLongExists := False;

  // Clear Expire Info
  FProgramInfo.ExpireInfo.HasExpireInfo := False;
  FProgramInfo.ExpireInfo.ExpireCount := 0;
  FProgramInfo.ExpireInfo.ExpireBasedOn := pebDays;
  FProgramInfo.ExpireInfo.ExpireOtherInfo := '';
  FProgramInfo.ExpireInfo.ExpireMonth := 0;
  FProgramInfo.ExpireInfo.ExpireDay := 0;
  FProgramInfo.ExpireInfo.ExpireYear := 0;

  // Clear Program Descriptions
  FProgramDescriptions.FLanguageName := 'English';
  FProgramDescriptions.Language.Keywords := '';
  FProgramDescriptions.Language.CharDesc45 := '';
  FProgramDescriptions.Language.CharDesc80 := '';
  FProgramDescriptions.Language.CharDesc250 := '';
  FProgramDescriptions.Language.CharDesc450 := '';
  FProgramDescriptions.Language.CharDesc2000 := '';

  // Clear TStrings objects
  FProgramDescriptions.Language.CharDesc250Strings.Clear;
  FProgramDescriptions.Language.CharDesc450Strings.Clear;
  FProgramDescriptions.Language.CharDesc2000Strings.Clear;

  // Clear Web Info
  FWebInfo.ApplicationURLs.ApplicationInfoURL := '';
  FWebInfo.ApplicationURLs.ApplicationOrderURL := '';
  FWebInfo.ApplicationURLs.ApplicationScreenshotURL := '';
  FWebInfo.ApplicationURLs.ApplicationIconURL := '';
  FWebInfo.ApplicationURLs.ApplicationXMLFileURL := '';
  FWebInfo.ApplicationURLs.VideoLink1URL := '';
  FWebInfo.ApplicationURLs.VideoLink2URL := '';
  FWebInfo.ApplicationURLs.FVideoLink1URLExists := False;
  FWebInfo.ApplicationURLs.FVideoLink2URLExists := False;

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
  FASP.ASPMemberNumber := '';

  // Clear XML formatting options
  FXmlConfig.XMLEncoding := peUTF8;
  FXmlConfig.XMLUseTabIndent := False;
  FXmlConfig.XMLIndentSize := 2;
  FXmlConfig.XMLEmptyTagType := ettWithoutSpace;
  FXmlConfig.XMLEndsWithLineBreak := False;
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

procedure TPadFormat.SetEncodingByString(const EncodingStr: string);
var
  i: TPadEncoding;
begin
  // Try to find matching encoding
  for i := Low(TPadEncoding) to High(TPadEncoding) do
  begin
    if SameText(PadEncodingStrings[i], EncodingStr) then
    begin
      FXmlConfig.XmlEncoding := i;
      Exit;
    end;
  end;

  // If not found, default to UTF-8
  FXmlConfig.XmlEncoding := peUTF8;
end;

function TPadFormat.DetectEncodingFromString(const XMLContent: string): TPadEncoding;
var
  Lines: TStringList;
  i, pStart, pEnd: integer;
  Line, EncodingStr: string;
  QuoteChar: char;
begin
  Result := peNone; // Default encoding

  Lines := TStringList.Create;
  try
    Lines.Text := XMLContent;

    // Look for XML declaration
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);

      // Check for XML declaration with encoding
      if Pos('<?xml', Line) = 1 then
      begin
        // Look for encoding attribute
        pStart := Pos('encoding=', Line);
        if pStart > 0 then
        begin
          // Skip 'encoding='
          pStart := pStart + 9;

          // Find the opening quote (skip any whitespace)
          while (pStart <= Length(Line)) and (Line[pStart] in [' ', #9]) do
            Inc(pStart);

          if (pStart <= Length(Line)) and (Line[pStart] in ['"', '''']) then
          begin
            QuoteChar := Line[pStart];

            // Find the closing quote of the same type
            pEnd := pStart + 1;
            while (pEnd <= Length(Line)) and (Line[pEnd] <> QuoteChar) do
              Inc(pEnd);

            if pEnd <= Length(Line) then
            begin
              // Extract encoding value
              EncodingStr := Copy(Line, pStart + 1, pEnd - pStart - 1);

              // Convert to TPadEncoding
              SetEncodingByString(EncodingStr);
              Result := FXmlConfig.XmlEncoding;
              Exit;
            end;
          end;
        end
        else
        begin
          // XML declaration without encoding specified
          Result := peNone;
          Exit;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TPadFormat.DetectIndentationStyle(const XMLContent: string): boolean;
var
  Lines: TStringList;
  i: integer;
  Line: string;
begin
  Result := False; // Default to spaces

  Lines := TStringList.Create;
  try
    Lines.Text := XMLContent;

    // Find first non-empty line after XML declaration
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);

      // Skip XML declaration and comments
      if (Line = '') or (Pos('<?xml', Line) = 1) or (Pos('<!--', Line) = 1) then
        Continue;

      // Check if line starts with tab
      if (Length(Lines[i]) > 0) and (Lines[i][1] = #9) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TPadFormat.DetectIndentSize(const XMLContent: string): integer;
var
  Lines: TStringList;
  i, j: integer;
  Line: string;
  FirstChar: char;
begin
  Result := 2; // Default indent size

  Lines := TStringList.Create;
  try
    Lines.Text := XMLContent;

    // Check each line
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];

      // Skip empty lines and XML declaration
      if (Trim(Line) = '') or (Pos('<?xml', Line) = 1) then
        Continue;

      // Skip comment lines
      if (Pos('<!--', Trim(Line)) = 1) then
        Continue;

      // Check if line has indentation
      if (Length(Line) > 0) then
      begin
        FirstChar := Line[1];

        // Count tabs
        if (FirstChar = #9) then
        begin
          j := 1;
          while (j <= Length(Line)) and (Line[j] = #9) do
            Inc(j);
          Result := j - 1;
          Break;
        end
        // Count spaces
        else if (FirstChar = ' ') then
        begin
          j := 1;
          while (j <= Length(Line)) and (Line[j] = ' ') do
            Inc(j);
          Result := j - 1;

          // Don't accept 0 or 1 spaces as valid indentation
          // (could be just alignment, not indentation)
          if (Result >= 2) and (Result <= 8) then
            Break
          else
            Result := 2; // Reset to default
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TPadFormat.DetectEmptyTagType(const XMLContent: string): TPadEmptyTagType;
var
  Lines: TStringList;
  i, j, StartPos, EndPos: integer;
  Line, TempLine: string;
  InComment, InCDATA, InQuote: boolean;
  QuoteChar: char;
  WithoutSpaceCount, WithSpaceCount, DoubleSidedCount: integer;

  function IsWhitespaceOnly(const S: string): boolean;
  var
    k: integer;
  begin
    Result := True;
    for k := 1 to Length(S) do
      if not (S[k] in [#9, #10, #13, #32]) then
      begin
        Result := False;
        Break;
      end;
  end;

begin
  Result := ettWithoutSpace; // Default value
  WithoutSpaceCount := 0;
  WithSpaceCount := 0;
  DoubleSidedCount := 0;

  Lines := TStringList.Create;
  try
    Lines.Text := XMLContent;

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      InComment := False;
      InCDATA := False;
      InQuote := False;
      QuoteChar := #0;

      j := 1;
      while j <= Length(Line) do
      begin
        // Check for comments <!-- ... -->
        if not InCDATA and not InQuote and (j <= Length(Line) - 3) and (Line[j] = '<') and (Line[j + 1] = '!') and
          (Line[j + 2] = '-') and (Line[j + 3] = '-') then
        begin
          InComment := True;
          Inc(j, 4);
          Continue;
        end;

        if InComment then
        begin
          if (j <= Length(Line) - 2) and (Line[j] = '-') and (Line[j + 1] = '-') and (Line[j + 2] = '>') then
          begin
            InComment := False;
            Inc(j, 3);
            Continue;
          end;
          Inc(j);
          Continue;
        end;

        // Check for CDATA <![CDATA[ ... ]]>
        if not InQuote and (j <= Length(Line) - 8) and (Line[j] = '<') and (Line[j + 1] = '!') and
          (Line[j + 2] = '[') and (Line[j + 3] = 'C') and (Line[j + 4] = 'D') and (Line[j + 5] = 'A') and
          (Line[j + 6] = 'T') and (Line[j + 7] = 'A') and (Line[j + 8] = '[') then
        begin
          InCDATA := True;
          Inc(j, 9);
          Continue;
        end;

        if InCDATA then
        begin
          if (j <= Length(Line) - 2) and (Line[j] = ']') and (Line[j + 1] = ']') and (Line[j + 2] = '>') then
          begin
            InCDATA := False;
            Inc(j, 3);
            Continue;
          end;
          Inc(j);
          Continue;
        end;

        // Check for quote characters
        if not InComment and not InCDATA then
        begin
          if (Line[j] = '"') or (Line[j] = '''') then
          begin
            if not InQuote then
            begin
              InQuote := True;
              QuoteChar := Line[j];
            end
            else if Line[j] = QuoteChar then
            begin
              InQuote := False;
            end;
          end;
        end;

        // Look for empty tags when not in comments, CDATA or quotes
        if not InComment and not InCDATA and not InQuote then
        begin
          // Check for self-closing tag: <tag/>
          if (j <= Length(Line) - 1) and (Line[j] = '<') then
          begin
            // Find the end of the tag
            StartPos := j;
            EndPos := StartPos;
            while (EndPos <= Length(Line)) and (Line[EndPos] <> '>') do
              Inc(EndPos);

            if (EndPos <= Length(Line)) and (Line[EndPos] = '>') then
            begin
              // Check if it's a self-closing tag (ends with />)
              if (EndPos > StartPos + 1) and (Line[EndPos - 1] = '/') then
              begin
                // Check if there's a space before the slash
                if (EndPos > StartPos + 2) and (Line[EndPos - 2] = ' ') then
                  Inc(WithSpaceCount)   // <tag />
                else
                  Inc(WithoutSpaceCount); // <tag/>
              end
              else
              begin
                // It's an opening tag, check if it's followed by closing tag with no content
                // Extract tag name
                TempLine := Copy(Line, StartPos + 1, EndPos - StartPos - 1);

                // Find closing tag for this opening tag
                // This is simplified and assumes closing tag is on the same line
                // For multi-line cases, we'd need more complex parsing
                if EndPos < Length(Line) then
                begin
                  // Look for </tagname> after this position
                  // First extract just the tag name without attributes
                  if Pos(' ', TempLine) > 0 then
                    TempLine := Copy(TempLine, 1, Pos(' ', TempLine) - 1);

                  // Search for closing tag
                  if Pos('</' + TempLine + '>', Line, EndPos + 1) > 0 then
                  begin
                    // Check if there's only whitespace between tags
                    TempLine := Copy(Line, EndPos + 1, Pos('</' + TempLine + '>', Line, EndPos + 1) - EndPos - 1);
                    if IsWhitespaceOnly(TempLine) then
                      Inc(DoubleSidedCount);
                  end;
                end;
              end;

              j := EndPos; // Skip to the end of the tag
            end;
          end;
        end;

        Inc(j);
      end;
    end;

    // Determine the most common empty tag type
    if (DoubleSidedCount > WithoutSpaceCount) and (DoubleSidedCount > WithSpaceCount) then
      Result := ettDoubleSided
    else if WithSpaceCount > WithoutSpaceCount then
      Result := ettWithSpace
    else
      Result := ettWithoutSpace;

  finally
    Lines.Free;
  end;
end;

function TPadFormat.ConvertIndentation(const XMLString: string; UseTabs: boolean; IndentSize: integer): string;
var
  Lines: TStringList;
  i, SpaceCount, Level: integer;
  Line, TrimmedLine: string;
  InsideTextNode: boolean;
begin
  if (IndentSize <= 0) then
    IndentSize := 2;

  Lines := TStringList.Create;
  try
    Lines.TrailingLineBreak := False;
    Lines.Text := XMLString;

    InsideTextNode := False;

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      TrimmedLine := Trim(Line);

      // Skip empty lines
      if TrimmedLine = '' then
        Continue;

      // Check if we're entering or leaving a text node
      if not InsideTextNode then
      begin
        // If line doesn't start with '<', it's the start of text content
        if (TrimmedLine[1] <> '<') then
        begin
          InsideTextNode := True;

          // Inside text node: replace every 2 spaces with Tab if UseTabs=True
          // Process text content line (including indentation)
          if UseTabs then
          begin
            // Count leading spaces
            SpaceCount := 0;
            while (SpaceCount < Length(Line)) and (Line[SpaceCount + 1] = ' ') do
              Inc(SpaceCount);

            // Replace pairs of spaces with tabs
            Level := SpaceCount div 2;
            if Level > 0 then
              Line := StringOfChar(#9, Level) + Copy(Line, SpaceCount + 1, MaxInt);
          end;

          Lines[i] := Line;
          Continue;
        end;
      end
      else
      begin
        // We're inside a text node - process ALL lines including closing tag
        // Replace every 2 spaces with Tab if UseTabs=True, ignore IndentSize

        if UseTabs then
        begin
          // Count leading spaces
          SpaceCount := 0;
          while (SpaceCount < Length(Line)) and (Line[SpaceCount + 1] = ' ') do
            Inc(SpaceCount);

          // Replace pairs of spaces with tabs
          Level := SpaceCount div 2;
          if Level > 0 then
            Line := StringOfChar(#9, Level) + Copy(Line, SpaceCount + 1, MaxInt);
        end;

        Lines[i] := Line;

        // Check if this line ends the text node (is a closing tag)
        if (TrimmedLine[1] = '<') then
        begin
          // Check if it's a closing tag
          if (Length(TrimmedLine) > 1) and (TrimmedLine[2] = '/') then
          begin
            // This is a closing tag after text - we've already processed it
            InsideTextNode := False;
          end;
        end;

        Continue;
      end;

      // If we get here, we're processing a structural tag (not inside text node)
      // Check if line starts with space (indented structural tag)
      if (Length(Line) > 0) and (Line[1] = ' ') then
      begin
        // Count leading spaces
        SpaceCount := 0;
        while (SpaceCount < Length(Line)) and (Line[SpaceCount + 1] = ' ') do
          Inc(SpaceCount);

        // Check if after spaces there's a '<' (should be for WriteXML structural tags)
        if (SpaceCount < Length(Line)) and (Line[SpaceCount + 1] = '<') then
        begin
          // Calculate level (WriteXML uses 2 spaces per level)
          Level := SpaceCount div 2;

          // Create new indentation
          if UseTabs then
            Line := StringOfChar(#9, Level) + Copy(Line, SpaceCount + 1, MaxInt)
          else
            Line := StringOfChar(' ', Level * IndentSize) + Copy(Line, SpaceCount + 1, MaxInt);

          Lines[i] := Line;
        end;
      end
      else if (Length(Line) > 0) and (Line[1] = '<') then
      begin
        // This is a structural tag at level 0 (no leading spaces)
        // Trim any accidental leading/trailing spaces
        Lines[i] := TrimmedLine;
      end;
      // All other cases are already handled or left as-is
    end;

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TPadFormat.ConvertEmptyTags(const XMLString: string; EmptyTagType: TPadEmptyTagType): string;
var
  Lines: TStringList;
  i, StartPos, EndPos: integer;
  Line, TagName, NewTag: string;
  InComment, InCDATA, InQuote: boolean;
  QuoteChar: char;
begin
  if EmptyTagType = ettWithoutSpace then
  begin
    Result := XMLString;
    Exit; // Default behavior, no conversion needed
  end;

  Lines := TStringList.Create;
  try
    Lines.TrailingLineBreak := False;
    Lines.Text := XMLString;

    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      InComment := False;
      InCDATA := False;
      InQuote := False;
      QuoteChar := #0;

      StartPos := 1;
      while StartPos <= Length(Line) do
      begin
        // Check for comments <!-- ... -->
        if not InCDATA and not InQuote and (StartPos <= Length(Line) - 3) and (Line[StartPos] = '<') and
          (Line[StartPos + 1] = '!') and (Line[StartPos + 2] = '-') and (Line[StartPos + 3] = '-') then
        begin
          InComment := True;
        end;

        // Check for CDATA <![CDATA[ ... ]]>
        if not InComment and not InQuote and (StartPos <= Length(Line) - 8) and (Line[StartPos] = '<') and
          (Line[StartPos + 1] = '!') and (Line[StartPos + 2] = '[') and (Line[StartPos + 3] = 'C') and
          (Line[StartPos + 4] = 'D') and (Line[StartPos + 5] = 'A') and (Line[StartPos + 6] = 'T') and
          (Line[StartPos + 7] = 'A') and (Line[StartPos + 8] = '[') then
        begin
          InCDATA := True;
        end;

        // Check for quote characters
        if not InComment and not InCDATA then
        begin
          if (Line[StartPos] = '"') or (Line[StartPos] = '''') then
          begin
            if not InQuote then
            begin
              InQuote := True;
              QuoteChar := Line[StartPos];
            end
            else if Line[StartPos] = QuoteChar then
            begin
              InQuote := False;
            end;
          end;
        end;

        // Look for empty tags when not in comments, CDATA or quotes
        if not InComment and not InCDATA and not InQuote and (StartPos <= Length(Line) - 2) and (Line[StartPos] = '<') then
        begin
          // Find the end of the tag
          EndPos := StartPos;
          while (EndPos <= Length(Line)) and (Line[EndPos] <> '>') do
            Inc(EndPos);

          if (EndPos <= Length(Line)) and (Line[EndPos] = '>') then
          begin
            // Check if it's an empty tag (ends with />)
            if (EndPos > StartPos + 1) and (Line[EndPos - 1] = '/') then
            begin
              // Extract the tag content (without < and >)
              TagName := Copy(Line, StartPos + 1, EndPos - StartPos - 2);

              // Remove trailing slash for ettWithSpace and ettDoubleSided
              if (TagName[Length(TagName)] = '/') then
                Delete(TagName, Length(TagName), 1);

              // Remove trailing space if present
              if (Length(TagName) > 0) and (TagName[Length(TagName)] = ' ') then
                Delete(TagName, Length(TagName), 1);

              case EmptyTagType of
                ettWithSpace:
                begin
                  // Format: <tag />
                  NewTag := '<' + TagName + ' />';
                end;
                ettDoubleSided:
                begin
                  // Format: <tag></tag>
                  // Extract just the tag name without attributes
                  if Pos(' ', TagName) > 0 then
                    NewTag := '<' + TagName + '></' + Copy(TagName, 1, Pos(' ', TagName) - 1) + '>'
                  else
                    NewTag := '<' + TagName + '></' + TagName + '>';
                end;
                else
                  NewTag := '<' + TagName + '/>';
              end;

              // Replace the tag in the line
              Delete(Line, StartPos, EndPos - StartPos + 1);
              Insert(NewTag, Line, StartPos);

              // Adjust positions for the next search
              StartPos := StartPos + Length(NewTag);
              Continue;
            end;
          end;
        end;

        Inc(StartPos);
      end;

      Lines[i] := Line;
    end;

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TPadFormat.SetXMLDeclaration(XMLString: string; XMLVersion: string; Encoding: TPadEncoding): string;
var
  EncodingStr: string;
  DeclarationStr: string;
begin
  // Get encoding string from TPadEncoding enumeration
  if Encoding = peNone then
    EncodingStr := ''
  else
    EncodingStr := PadEncodingStrings[Encoding];

  // Set XML header data
  DeclarationStr := '<?xml version="' + XMLVersion + '"';
  if (EncodingStr <> '') then  DeclarationStr += ' encoding="' + EncodingStr + '"';
  if FXmlConfig.XMLEmptyTagType <> ettWithoutSpace then
    DeclarationStr += ' ';
  DeclarationStr += '?>';
  Result := DeclarationStr + LineEnding + XMLString;
end;

function TPadFormat.RemoveXMLDeclaration(const XMLString: string): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.TrailingLineBreak := False;
    Lines.Text := XMLString;

    // Check if first line is an XML declaration
    if (Lines.Count > 0) and (Pos('<?xml', Lines[0]) = 1) then
      Lines.Delete(0);

    // Remove any empty lines at the beginning
    while (Lines.Count > 0) and (Trim(Lines[0]) = '') do
      Lines.Delete(0);

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
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
    Result := pisNone; // Default
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
    prsUpdate: Result := 'Update';
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
  if Value = 'Update' then
    Result := prsUpdate
  else if Value = 'Major Update' then
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

function PadOSModernToString(Value: TPadOSModern): string;
begin
  if (Value >= Low(TPadOSModern)) and (Value <= High(TPadOSModern)) then
    Result := PadOSModernStrings[Value]
  else
    Result := '';
end;

function StringToPadOSModern(const Value: string): TPadOSModern;
var
  OS: TPadOSModern;
begin
  for OS := Low(TPadOSModern) to High(TPadOSModern) do
  begin
    if SameText(PadOSModernStrings[OS], Value) then
    begin
      Result := OS;
      Exit;
    end;
  end;
  Result := Low(TPadOSModern);
end;

// Combined Language conversion
// Combined OS Support conversion
function PadOSSupportToString(WindowsSet: TPadOSWindowsSet; UnixLinuxSet: TPadOSUnixLinuxSet; OtherSet: TPadOSOtherSet;
  ModernSet: TPadOSModernSet): string;
var
  List: TStringList;
  WinOS: TPadOSWindows;
  UnixOS: TPadOSUnixLinux;
  OtherOS: TPadOSOther;
  ModernOS: TPadOSModern;
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

    // Add Modern OS
    for ModernOS := Low(TPadOSModern) to High(TPadOSModern) do
    begin
      if ModernOS in ModernSet then
        List.Add(PadOSModernStrings[ModernOS]);
    end;

    Result := List.DelimitedText;
  finally
    List.Free;
  end;
end;

procedure StringToPadOSSupport(const Value: string; out WindowsSet: TPadOSWindowsSet; out UnixLinuxSet: TPadOSUnixLinuxSet;
  out OtherSet: TPadOSOtherSet; out ModernSet: TPadOSModernSet);
var
  OSList: TStringList;
  i: integer;
  OSStr: string;
  Found: boolean;
  WinOS: TPadOSWindows;
  UnixOS: TPadOSUnixLinux;
  OtherOS: TPadOSOther;
  ModernOS: TPadOSModern;
begin
  WindowsSet := [];
  UnixLinuxSet := [];
  OtherSet := [];
  ModernSet := [];

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

      if not Found then
      begin
        // Try to find in Modern set
        for ModernOS := Low(TPadOSModern) to High(TPadOSModern) do
        begin
          if SameText(PadOSModernStrings[ModernOS], OSStr) then
          begin
            Include(ModernSet, ModernOS);
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

end.
