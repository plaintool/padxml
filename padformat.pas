unit padformat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, TypInfo, StrUtils;

type
  // Enumerations for PAD standard values
  TPadInstallSupport = (
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
    prsMajorUpdate,
    prsMinorUpdate,
    prsNewRelease,
    prsBeta,
    prsAlpha,
    prsMediaOnly
    );

  TPadExpireBasedOn = (
    pebDays,
    pebRuns,
    pebDate,
    pebOther
    );

  // Language support - this will be a set
  TPadLanguage = (
    plEnglish,
    plFrench,
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
    plJapanese,
    plChinese,
    plKorean,
    plArabic,
    plHebrew,
    plGreek,
    plTurkish,
    plPolish,
    plCzech,
    plHungarian,
    plRomanian,
    plBulgarian
    );

  TPadLanguages = set of TPadLanguage;

  // OS support - this will be a set
  TPadOS = (
    posWindows95,
    posWindows98,
    posWindowsME,
    posWindowsNT,
    posWindows2000,
    posWindowsXP,
    posWindowsVista,
    posWindows7,
    posWindows8,
    posWindows10,
    posWindows11,
    posMacOS,
    posLinux,
    posUnix,
    posDOS,
    posOS2,
    posOther
    );

  TPadOSSupport = set of TPadOS;

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
    FMasterPadVersion: string;
    FMasterPadEditor: string;
    FMasterPadInfo: string;
  published
    property MasterPadVersion: string read FMasterPadVersion write FMasterPadVersion;
    property MasterPadEditor: string read FMasterPadEditor write FMasterPadEditor;
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
    FNewsFeedFeedURL: string;
    FNewsFeedType: string;
    FNewsFeedTitle: string;
    FNewsFeedKeywords: string;
    FNewsFeedDescription70: string;
    FNewsFeedDescription250: string;
  published
    property NewsFeedFeedURL: string read FNewsFeedFeedURL write FNewsFeedFeedURL;
    property NewsFeedType: string read FNewsFeedType write FNewsFeedType;
    property NewsFeedTitle: string read FNewsFeedTitle write FNewsFeedTitle;
    property NewsFeedKeywords: string read FNewsFeedKeywords write FNewsFeedKeywords;
    property NewsFeedDescription70: string read FNewsFeedDescription70 write FNewsFeedDescription70;
    property NewsFeedDescription250: string read FNewsFeedDescription250 write FNewsFeedDescription250;
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
  published
    property HasExpireInfo: boolean read FHasExpireInfo write FHasExpireInfo;
    property ExpireCount: integer read FExpireCount write FExpireCount;
    property ExpireBasedOn: TPadExpireBasedOn read FExpireBasedOn write FExpireBasedOn;
    property ExpireBasedOnAsString: string read GetExpireBasedOnAsString write SetExpireBasedOnAsString;
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
    FProgramCostDollars: word;
    FProgramCostOtherCode: string;
    FProgramCostOther: word;
    FProgramType: TPadProgramType;
    FProgramReleaseStatus: TPadReleaseStatus;
    FProgramInstallSupport: TPadInstallSupport;
    FProgramOSSupport: TPadOSSupport;
    FProgramLanguage: TPadLanguages;
    FProgramChangeInfo: string;
    FProgramSpecificCategory: string;
    FProgramCategoryClass: string;
    FProgramSystemRequirements: string;
    FFileInfo: TPadFileInfo;
    FExpireInfo: TPadExpireInfo;
    function GetProgramTypeAsString: string;
    procedure SetProgramTypeAsString(const Value: string);
    function GetProgramReleaseStatusAsString: string;
    procedure SetProgramReleaseStatusAsString(const Value: string);
    function GetProgramInstallSupportAsString: string;
    procedure SetProgramInstallSupportAsString(const Value: string);
    function GetProgramOSSupportAsString: string;
    procedure SetProgramOSSupportAsString(const Value: string);
    function GetProgramLanguageAsString: string;
    procedure SetProgramLanguageAsString(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ProgramName: string read FProgramName write FProgramName;
    property ProgramVersion: string read FProgramVersion write FProgramVersion;
    property ProgramReleaseMonth: byte read FProgramReleaseMonth write FProgramReleaseMonth;
    property ProgramReleaseDay: byte read FProgramReleaseDay write FProgramReleaseDay;
    property ProgramReleaseYear: word read FProgramReleaseYear write FProgramReleaseYear;
    property ProgramCostDollars: word read FProgramCostDollars write FProgramCostDollars;
    property ProgramCostOtherCode: string read FProgramCostOtherCode write FProgramCostOtherCode;
    property ProgramCostOther: word read FProgramCostOther write FProgramCostOther;
    property ProgramType: TPadProgramType read FProgramType write FProgramType;
    property ProgramTypeAsString: string read GetProgramTypeAsString write SetProgramTypeAsString;
    property ProgramReleaseStatus: TPadReleaseStatus read FProgramReleaseStatus write FProgramReleaseStatus;
    property ProgramReleaseStatusAsString: string read GetProgramReleaseStatusAsString write SetProgramReleaseStatusAsString;
    property ProgramInstallSupport: TPadInstallSupport read FProgramInstallSupport write FProgramInstallSupport;
    property ProgramInstallSupportAsString: string read GetProgramInstallSupportAsString write SetProgramInstallSupportAsString;
    property ProgramOSSupport: TPadOSSupport read FProgramOSSupport write FProgramOSSupport;
    property ProgramOSSupportAsString: string read GetProgramOSSupportAsString write SetProgramOSSupportAsString;
    property ProgramLanguage: TPadLanguages read FProgramLanguage write FProgramLanguage;
    property ProgramLanguageAsString: string read GetProgramLanguageAsString write SetProgramLanguageAsString;
    property ProgramChangeInfo: string read FProgramChangeInfo write FProgramChangeInfo;
    property ProgramSpecificCategory: string read FProgramSpecificCategory write FProgramSpecificCategory;
    property ProgramCategoryClass: string read FProgramCategoryClass write FProgramCategoryClass;
    property ProgramSystemRequirements: string read FProgramSystemRequirements write FProgramSystemRequirements;
    property FileInfo: TPadFileInfo read FFileInfo write FFileInfo;
    property ExpireInfo: TPadExpireInfo read FExpireInfo write FExpireInfo;
  end;

  { TPadEnglishDescription }
  TPadEnglishDescription = class(TPersistent)
  private
    FKeywords: string;
    FCharDesc45: string;
    FCharDesc80: string;
    FCharDesc250: string;
    FCharDesc450: string;
    FCharDesc2000: string;
  published
    property Keywords: string read FKeywords write FKeywords;
    property CharDesc45: string read FCharDesc45 write FCharDesc45;
    property CharDesc80: string read FCharDesc80 write FCharDesc80;
    property CharDesc250: string read FCharDesc250 write FCharDesc250;
    property CharDesc450: string read FCharDesc450 write FCharDesc450;
    property CharDesc2000: string read FCharDesc2000 write FCharDesc2000;
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
    FDistributionPermissions: string;
    FEULA: string;
  published
    property DistributionPermissions: string read FDistributionPermissions write FDistributionPermissions;
    property DistributionPermissionsAsString: string read FDistributionPermissions write FDistributionPermissions;
    property EULA: string read FEULA write FEULA;
  end;

  { TPadASP }
  TPadASP = class(TPersistent)
  private
    FASPMember: boolean;
    FASPMemberNumber: word;
  published
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
    property ASP: TPadASP read FASP write FASP;
  end;

// Helper functions for conversions
function GetNodeText(Node: TDOMNode): string;

function PadInstallSupportToString(Value: TPadInstallSupport): string;
function StringToPadInstallSupport(const Value: string): TPadInstallSupport;

function PadProgramTypeToString(Value: TPadProgramType): string;
function StringToPadProgramType(const Value: string): TPadProgramType;

function PadReleaseStatusToString(Value: TPadReleaseStatus): string;
function StringToPadReleaseStatus(const Value: string): TPadReleaseStatus;

function PadExpireBasedOnToString(Value: TPadExpireBasedOn): string;
function StringToPadExpireBasedOn(const Value: string): TPadExpireBasedOn;

function PadOSSupportToString(Value: TPadOSSupport): string;
function StringToPadOSSupport(const Value: string): TPadOSSupport;

function PadLanguagesToString(Value: TPadLanguages): string;
function StringToPadLanguages(const Value: string): TPadLanguages;

procedure Register;

implementation

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

function TPadProgramInfo.GetProgramOSSupportAsString: string;
begin
  Result := PadOSSupportToString(FProgramOSSupport);
end;

procedure TPadProgramInfo.SetProgramOSSupportAsString(const Value: string);
begin
  FProgramOSSupport := StringToPadOSSupport(Value);
end;

function TPadProgramInfo.GetProgramLanguageAsString: string;
begin
  Result := PadLanguagesToString(FProgramLanguage);
end;

procedure TPadProgramInfo.SetProgramLanguageAsString(const Value: string);
begin
  FProgramLanguage := StringToPadLanguages(Value);
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
  FASP.Free;
  inherited Destroy;
end;

{ TPadFormat }

procedure TPadFormat.LoadFromXML(const XMLContent: string);
var
  Doc: TXMLDocument;
  Stream: TStringStream;
  RootNode, Node, SubNode: TDOMNode;

  function GetNodeValue(Parent: TDOMNode; const NodeName: string): string;
  var
    TempNode: TDOMNode;
  begin
    TempNode := Parent.FindNode(NodeName);
    if Assigned(TempNode) then
      Result := GetNodeText(TempNode)
    else
      Result := '';
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

      // News Feed
      Node := RootNode.FindNode('NewsFeed');
      if Assigned(Node) then
      begin
        FNewsFeed.NewsFeedFeedURL := GetNodeValue(Node, 'NewsFeed_Feed_URL');
        FNewsFeed.NewsFeedType := GetNodeValue(Node, 'NewsFeed_Type');
        FNewsFeed.NewsFeedTitle := GetNodeValue(Node, 'NewsFeed_Title');
        FNewsFeed.NewsFeedKeywords := GetNodeValue(Node, 'NewsFeed_Keywords');
        FNewsFeed.NewsFeedDescription70 := GetNodeValue(Node, 'NewsFeed_Description_70');
        FNewsFeed.NewsFeedDescription250 := GetNodeValue(Node, 'NewsFeed_Description_250');
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
        FProgramInfo.ProgramCostDollars := StrToIntDef(GetNodeValue(Node, 'Program_Cost_Dollars'), 0);
        FProgramInfo.ProgramCostOtherCode := GetNodeValue(Node, 'Program_Cost_Other_Code');
        FProgramInfo.ProgramCostOther := StrToIntDef(GetNodeValue(Node, 'Program_Cost_Other'), 0);

        // Use string properties that will handle conversion
        FProgramInfo.ProgramTypeAsString := GetNodeValue(Node, 'Program_Type');
        FProgramInfo.ProgramReleaseStatusAsString := GetNodeValue(Node, 'Program_Release_Status');
        FProgramInfo.ProgramInstallSupportAsString := GetNodeValue(Node, 'Program_Install_Support');
        FProgramInfo.ProgramOSSupportAsString := GetNodeValue(Node, 'Program_OS_Support');
        FProgramInfo.ProgramLanguageAsString := GetNodeValue(Node, 'Program_Language');

        FProgramInfo.ProgramChangeInfo := GetNodeValue(Node, 'Program_Change_Info');
        FProgramInfo.ProgramSpecificCategory := GetNodeValue(Node, 'Program_Specific_Category');
        FProgramInfo.ProgramCategoryClass := GetNodeValue(Node, 'Program_Category_Class');
        FProgramInfo.ProgramSystemRequirements := GetNodeValue(Node, 'Program_System_Requirements');

        // Load File Info
        SubNode := Node.FindNode('File_Info');
        if Assigned(SubNode) then
        begin
          FProgramInfo.FileInfo.FileSizeBytes := StrToInt64Def(GetNodeValue(SubNode, 'File_Size_Bytes'), 0);
          FProgramInfo.FileInfo.FileSizeK := StrToInt64Def(GetNodeValue(SubNode, 'File_Size_K'), 0);
          FProgramInfo.FileInfo.FileSizeMB := StrToFloatDef(GetNodeValue(SubNode, 'File_Size_MB'), 0);
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
        FPermissions.DistributionPermissionsAsString := GetNodeValue(Node, 'Distribution_Permissions');
        FPermissions.EULA := GetNodeValue(Node, 'EULA');
      end;

      // Load ASP
      Node := RootNode.FindNode('ASP');
      if Assigned(Node) then
      begin
        FASP.ASPMember := UpperCase(GetNodeValue(Node, 'ASP_Member')) = 'Y';
        FASP.ASPMemberNumber := StrToIntDef(GetNodeValue(Node, 'ASP_Member_Number'), 0);
      end;

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
begin
  Doc := TXMLDocument.Create;
  try
    // Create root element
    RootNode := Doc.CreateElement('XML_DIZ_INFO');
    Doc.AppendChild(RootNode);

    // Master Pad Version Info
    Node := AddChildNode(RootNode, 'MASTER_PAD_VERSION_INFO');
    SetNodeText(Doc, Node, 'MASTER_PAD_VERSION',
      FMasterPadVersionInfo.MasterPadVersion);
    SetNodeText(Doc, Node, 'MASTER_PAD_EDITOR',
      FMasterPadVersionInfo.MasterPadEditor);
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

    // Social media pages
    SetNodeText(Doc, Node, 'GooglePlusPage', FCompanyInfo.GooglePlusPage);
    SetNodeText(Doc, Node, 'LinkedinPage', FCompanyInfo.LinkedinPage);
    SetNodeText(Doc, Node, 'TwitterCompanyPage', FCompanyInfo.TwitterCompanyPage);
    SetNodeText(Doc, Node, 'FacebookCompanyPage', FCompanyInfo.FacebookCompanyPage);
    SetNodeText(Doc, Node, 'CompanyStorePage', FCompanyInfo.CompanyStorePage);

    // News Feed
    Node := AddChildNode(RootNode, 'NewsFeed');
    SetNodeText(Doc, Node, 'NewsFeed_Feed_URL', FNewsFeed.NewsFeedFeedURL);
    SetNodeText(Doc, Node, 'NewsFeed_Type', FNewsFeed.NewsFeedType);
    SetNodeText(Doc, Node, 'NewsFeed_Title', FNewsFeed.NewsFeedTitle);
    SetNodeText(Doc, Node, 'NewsFeed_Keywords', FNewsFeed.NewsFeedKeywords);
    SetNodeText(Doc, Node, 'NewsFeed_Description_70', FNewsFeed.NewsFeedDescription70);
    SetNodeText(Doc, Node, 'NewsFeed_Description_250', FNewsFeed.NewsFeedDescription250);

    // Program Info
    Node := AddChildNode(RootNode, 'Program_Info');
    SetNodeText(Doc, Node, 'Program_Name', FProgramInfo.ProgramName);
    SetNodeText(Doc, Node, 'Program_Version', FProgramInfo.ProgramVersion);
    SetNodeText(Doc, Node, 'Program_Release_Month',
      IntToStr(FProgramInfo.ProgramReleaseMonth));
    SetNodeText(Doc, Node, 'Program_Release_Day',
      IntToStr(FProgramInfo.ProgramReleaseDay));
    SetNodeText(Doc, Node, 'Program_Release_Year',
      IntToStr(FProgramInfo.ProgramReleaseYear));
    SetNodeText(Doc, Node, 'Program_Cost_Dollars',
      IntToStr(FProgramInfo.ProgramCostDollars));
    SetNodeText(Doc, Node, 'Program_Cost_Other_Code',
      FProgramInfo.ProgramCostOtherCode);
    SetNodeText(Doc, Node, 'Program_Cost_Other',
      IntToStr(FProgramInfo.ProgramCostOther));
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
      FProgramInfo.ProgramCategoryClass);
    SetNodeText(Doc, Node, 'Program_System_Requirements',
      FProgramInfo.ProgramSystemRequirements);

    // File Info
    SubNode := AddChildNode(Node, 'File_Info');
    SetNodeText(Doc, SubNode, 'File_Size_Bytes',
      IntToStr(FProgramInfo.FileInfo.FileSizeBytes));
    SetNodeText(Doc, SubNode, 'File_Size_K',
      IntToStr(FProgramInfo.FileInfo.FileSizeK));
    SetNodeText(Doc, SubNode, 'File_Size_MB',
      FloatToStr(FProgramInfo.FileInfo.FileSizeMB));

    // Expire Info
    SubNode := AddChildNode(Node, 'Expire_Info');
    SetNodeText(Doc, SubNode, 'Has_Expire_Info',
      BoolToStr(FProgramInfo.ExpireInfo.HasExpireInfo, 'Y', ''));
    SetNodeText(Doc, SubNode, 'Expire_Count',
      IntToStr(FProgramInfo.ExpireInfo.ExpireCount));
    SetNodeText(Doc, SubNode, 'Expire_Based_On',
      FProgramInfo.ExpireInfo.ExpireBasedOnAsString);
    SetNodeText(Doc, SubNode, 'Expire_Other_Info',
      FProgramInfo.ExpireInfo.ExpireOtherInfo);
    SetNodeText(Doc, SubNode, 'Expire_Month',
      IfThen(FProgramInfo.ExpireInfo.ExpireMonth = 0, '', IntToStr(FProgramInfo.ExpireInfo.ExpireMonth)));
    SetNodeText(Doc, SubNode, 'Expire_Day',
      IfThen(FProgramInfo.ExpireInfo.ExpireDay = 0, '', IntToStr(FProgramInfo.ExpireInfo.ExpireDay)));
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
    SetNodeText(Doc, Node, 'Distribution_Permissions',
      FPermissions.DistributionPermissionsAsString);
    SetNodeText(Doc, Node, 'EULA', FPermissions.EULA);

    // ASP
    Node := AddChildNode(RootNode, 'ASP');
    SetNodeText(Doc, Node, 'ASP_Member', BoolToStr(FASP.ASPMember, 'Y', ''));
    SetNodeText(Doc, Node, 'ASP_Member_Number',
      IntToStr(FASP.ASPMemberNumber));

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
  FNewsFeed.NewsFeedFeedURL := '';
  FNewsFeed.NewsFeedType := '';
  FNewsFeed.NewsFeedTitle := '';
  FNewsFeed.NewsFeedKeywords := '';
  FNewsFeed.NewsFeedDescription70 := '';
  FNewsFeed.NewsFeedDescription250 := '';

  // Clear Program Info
  FProgramInfo.ProgramName := '';
  FProgramInfo.ProgramVersion := '';
  FProgramInfo.ProgramReleaseMonth := 0;
  FProgramInfo.ProgramReleaseDay := 0;
  FProgramInfo.ProgramReleaseYear := 0;
  FProgramInfo.ProgramCostDollars := 0;
  FProgramInfo.ProgramCostOtherCode := '';
  FProgramInfo.ProgramCostOther := 0;
  FProgramInfo.ProgramType := pptShareware;
  FProgramInfo.ProgramReleaseStatus := prsNewRelease;
  FProgramInfo.ProgramInstallSupport := pisInstallAndUninstall;
  FProgramInfo.ProgramOSSupport := [];
  FProgramInfo.ProgramLanguage := [];
  FProgramInfo.ProgramChangeInfo := '';
  FProgramInfo.ProgramSpecificCategory := '';
  FProgramInfo.ProgramCategoryClass := '';
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

  // Clear ASP
  FASP.ASPMember := False;
  FASP.ASPMemberNumber := 0;
end;

function TPadFormat.SetNodeText(Doc: TXMLDocument; ParentNode: TDOMNode; NodeName, NodeValue: string): TDOMNode;
begin
  Result := Doc.CreateElement(NodeName);
  TDOMElement(Result).TextContent := NodeValue;
  ParentNode.AppendChild(Result);
end;

function TPadFormat.AddChildNode(ParentNode: TDOMNode; NodeName: string): TDOMNode;
begin
  Result := ParentNode.OwnerDocument.CreateElement(NodeName);
  ParentNode.AppendChild(Result);
end;

procedure TPadFormat.SetNodeTextValue(Node: TDOMNode; Value: string);
begin
  if Assigned(Node) then
    TDOMElement(Node).TextContent := Value;
end;

// Helper functions implementation

function GetNodeText(Node: TDOMNode): string;
begin
  if Assigned(Node) and Assigned(Node.FirstChild) then
    Result := Node.FirstChild.NodeValue
  else
    Result := '';
end;

function PadInstallSupportToString(Value: TPadInstallSupport): string;
begin
  case Value of
    pisInstallAndUninstall: Result := 'Install and Uninstall';
    pisInstallOnly: Result := 'Install Only';
    pisNoInstallSupport: Result := 'No Install Support';
    pisUninstallOnly: Result := 'Uninstall Only';
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
    pebDays: Result := 'Days';
    pebRuns: Result := 'Runs';
    pebDate: Result := 'Date';
    pebOther: Result := 'Other';
  end;
end;

function StringToPadExpireBasedOn(const Value: string): TPadExpireBasedOn;
begin
  if Value = 'Days' then
    Result := pebDays
  else if Value = 'Runs' then
    Result := pebRuns
  else if Value = 'Date' then
    Result := pebDate
  else if Value = 'Other' then
    Result := pebOther
  else
    Result := pebDays; // Default
end;

function StringToPadDistributionPermission(const Value: string): string;
begin
  Result := Value;
end;

function PadOSSupportToString(Value: TPadOSSupport): string;
var
  List: TStringList;
  OS: TPadOS;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ',';
    List.StrictDelimiter := True;

    for OS := Low(TPadOS) to High(TPadOS) do
    begin
      if OS in Value then
      begin
        case OS of
          posWindows95: List.Add('Windows 95');
          posWindows98: List.Add('Windows 98');
          posWindowsME: List.Add('Windows ME');
          posWindowsNT: List.Add('Windows NT');
          posWindows2000: List.Add('Windows 2000');
          posWindowsXP: List.Add('Windows XP');
          posWindowsVista: List.Add('Windows Vista');
          posWindows7: List.Add('Windows 7');
          posWindows8: List.Add('Windows 8');
          posWindows10: List.Add('Windows 10');
          posWindows11: List.Add('Windows 11');
          posMacOS: List.Add('Mac OS');
          posLinux: List.Add('Linux');
          posUnix: List.Add('Unix');
          posDOS: List.Add('DOS');
          posOS2: List.Add('OS/2');
          posOther: List.Add('Other');
        end;
      end;
    end;

    Result := List.DelimitedText;
  finally
    List.Free;
  end;
end;

function StringToPadOSSupport(const Value: string): TPadOSSupport;
var
  OSList: TStringList;
  i: integer;
  OSStr: string;
begin
  Result := [];
  OSList := TStringList.Create;
  try
    // Handle the value - it might be a single OS or comma-separated list
    if Pos(',', Value) > 0 then
    begin
      // Comma-separated list
      OSList.CommaText := Value;
    end
    else
    begin
      // Single value
      OSList.Add(Trim(Value));
    end;

    for i := 0 to OSList.Count - 1 do
    begin
      OSStr := Trim(OSList[i]);

      // Check for Windows versions (case insensitive)
      if SameText(OSStr, 'Windows 10') or SameText(OSStr, 'Win10') or SameText(OSStr, 'Windows10') then
        Include(Result, posWindows10)
      else if SameText(OSStr, 'Windows 11') or SameText(OSStr, 'Win11') or SameText(OSStr, 'Windows11') then
        Include(Result, posWindows11)
      else if SameText(OSStr, 'Windows 8') or SameText(OSStr, 'Win8') or SameText(OSStr, 'Windows8') then
        Include(Result, posWindows8)
      else if SameText(OSStr, 'Windows 7') or SameText(OSStr, 'Win7') or SameText(OSStr, 'Windows7') then
        Include(Result, posWindows7)
      else if SameText(OSStr, 'Windows Vista') or SameText(OSStr, 'WinVista') or SameText(OSStr, 'WindowsVista') then
        Include(Result, posWindowsVista)
      else if SameText(OSStr, 'Windows XP') or SameText(OSStr, 'WinXP') or SameText(OSStr, 'WindowsXP') then
        Include(Result, posWindowsXP)
      else if SameText(OSStr, 'Windows 2000') or SameText(OSStr, 'Win2000') or SameText(OSStr, 'Win2K') or
        SameText(OSStr, 'Windows2000') then
        Include(Result, posWindows2000)
      else if SameText(OSStr, 'Windows NT') or SameText(OSStr, 'WinNT') or SameText(OSStr, 'WindowsNT') then
        Include(Result, posWindowsNT)
      else if SameText(OSStr, 'Windows ME') or SameText(OSStr, 'WinME') or SameText(OSStr, 'WindowsME') then
        Include(Result, posWindowsME)
      else if SameText(OSStr, 'Windows 98') or SameText(OSStr, 'Win98') or SameText(OSStr, 'Windows98') then
        Include(Result, posWindows98)
      else if SameText(OSStr, 'Windows 95') or SameText(OSStr, 'Win95') or SameText(OSStr, 'Windows95') then
        Include(Result, posWindows95)
      else if SameText(OSStr, 'Mac OS') or SameText(OSStr, 'MacOS') or SameText(OSStr, 'Mac') then
        Include(Result, posMacOS)
      else if SameText(OSStr, 'Linux') then
        Include(Result, posLinux)
      else if SameText(OSStr, 'Unix') then
        Include(Result, posUnix)
      else if SameText(OSStr, 'DOS') then
        Include(Result, posDOS)
      else if SameText(OSStr, 'OS/2') or SameText(OSStr, 'OS2') then
        Include(Result, posOS2)
      else if SameText(OSStr, 'Other') then
        Include(Result, posOther);
    end;
  finally
    OSList.Free;
  end;
end;

function PadLanguagesToString(Value: TPadLanguages): string;
var
  Lang: TPadLanguage;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    for Lang := Low(TPadLanguage) to High(TPadLanguage) do
    begin
      if Lang in Value then
      begin
        case Lang of
          plEnglish: List.Add('English');
          plFrench: List.Add('French');
          plGerman: List.Add('German');
          plSpanish: List.Add('Spanish');
          plItalian: List.Add('Italian');
          plDutch: List.Add('Dutch');
          plPortuguese: List.Add('Portuguese');
          plSwedish: List.Add('Swedish');
          plDanish: List.Add('Danish');
          plNorwegian: List.Add('Norwegian');
          plFinnish: List.Add('Finnish');
          plRussian: List.Add('Russian');
          plJapanese: List.Add('Japanese');
          plChinese: List.Add('Chinese');
          plKorean: List.Add('Korean');
          plArabic: List.Add('Arabic');
          plHebrew: List.Add('Hebrew');
          plGreek: List.Add('Greek');
          plTurkish: List.Add('Turkish');
          plPolish: List.Add('Polish');
          plCzech: List.Add('Czech');
          plHungarian: List.Add('Hungarian');
          plRomanian: List.Add('Romanian');
          plBulgarian: List.Add('Bulgarian');
        end;
      end;
    end;
    Result := List.CommaText;
  finally
    List.Free;
  end;
end;

function StringToPadLanguages(const Value: string): TPadLanguages;
var
  List: TStringList;
  i: integer;
begin
  Result := [];
  List := TStringList.Create;
  try
    List.CommaText := Value;
    for i := 0 to List.Count - 1 do
    begin
      if List[i] = 'English' then
        Include(Result, plEnglish)
      else if List[i] = 'French' then
        Include(Result, plFrench)
      else if List[i] = 'German' then
        Include(Result, plGerman)
      else if List[i] = 'Spanish' then
        Include(Result, plSpanish)
      else if List[i] = 'Italian' then
        Include(Result, plItalian)
      else if List[i] = 'Dutch' then
        Include(Result, plDutch)
      else if List[i] = 'Portuguese' then
        Include(Result, plPortuguese)
      else if List[i] = 'Swedish' then
        Include(Result, plSwedish)
      else if List[i] = 'Danish' then
        Include(Result, plDanish)
      else if List[i] = 'Norwegian' then
        Include(Result, plNorwegian)
      else if List[i] = 'Finnish' then
        Include(Result, plFinnish)
      else if List[i] = 'Russian' then
        Include(Result, plRussian)
      else if List[i] = 'Japanese' then
        Include(Result, plJapanese)
      else if List[i] = 'Chinese' then
        Include(Result, plChinese)
      else if List[i] = 'Korean' then
        Include(Result, plKorean)
      else if List[i] = 'Arabic' then
        Include(Result, plArabic)
      else if List[i] = 'Hebrew' then
        Include(Result, plHebrew)
      else if List[i] = 'Greek' then
        Include(Result, plGreek)
      else if List[i] = 'Turkish' then
        Include(Result, plTurkish)
      else if List[i] = 'Polish' then
        Include(Result, plPolish)
      else if List[i] = 'Czech' then
        Include(Result, plCzech)
      else if List[i] = 'Hungarian' then
        Include(Result, plHungarian)
      else if List[i] = 'Romanian' then
        Include(Result, plRomanian)
      else if List[i] = 'Bulgarian' then
        Include(Result, plBulgarian);
    end;
  finally
    List.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('PAD', [TPadFormat]);
end;

end.
