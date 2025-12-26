//-----------------------------------------------------------------------------------
//  PadXml © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/padxml
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  Menus,
  ComCtrls,
  PropEdits,
  ObjectInspector,
  RTTIGrids,
  LCLType, StdCtrls, ExtCtrls, Buttons,
  padformat;

type

  { TformPadXml }

  TformPadXml = class(TForm)
    filter: TEdit;
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuFileOpen: TMenuItem;
    menuFileSave: TMenuItem;
    menuFileExit: TMenuItem;
    menuFileSaveAs: TMenuItem;
    menuFileNew: TMenuItem;
    dialogOpen: TOpenDialog;
    menuHelp: TMenuItem;
    menuBuyMeACoffee: TMenuItem;
    menuCheckForUpdates: TMenuItem;
    menuAbout: TMenuItem;
    menuDeuPAD: TMenuItem;
    menuDynamicPAD: TMenuItem;
    menuAffiliates: TMenuItem;
    menuArticleContents: TMenuItem;
    menuAllmyapps: TMenuItem;
    menuAppStore: TMenuItem;
    menuIssues: TMenuItem;
    menuTPA: TMenuItem;
    menuMSN: TMenuItem;
    menuSimtel: TMenuItem;
    menuPADRING: TMenuItem;
    menuPressRelease: TMenuItem;
    menuPADCertificationPromotion: TMenuItem;
    menuOnlineShops: TMenuItem;
    menuPADmap: TMenuItem;
    menuSite: TMenuItem;
    menuRoboSoft: TMenuItem;
    menuNewsFeed: TMenuItem;
    menuView: TMenuItem;
    PanelFilter: TPanel;
    propertyPad: TTIPropertyGrid;
    dialogSave: TSaveDialog;
    menuFileSeparator1: TMenuItem;
    ButtonFilterClear: TSpeedButton;
    StatusBar: TStatusBar;
    procedure filterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure menuAboutClick(Sender: TObject);
    procedure menuSectionClick(Sender: TObject);
    procedure menuBuyMeACoffeeClick(Sender: TObject);
    procedure menuCheckForUpdatesClick(Sender: TObject);
    procedure menuFileExitClick(Sender: TObject);
    procedure menuFileNewClick(Sender: TObject);
    procedure menuFileOpenClick(Sender: TObject);
    procedure menuFileSaveAsClick(Sender: TObject);
    procedure menuFileSaveClick(Sender: TObject);
    procedure propertyPadEditorFilter(Sender: TObject; aEditor: TPropertyEditor; var aShow: boolean);
    procedure propertyPadModified(Sender: TObject);
    function EndsWithLineBreak(const Buffer: TBytes): boolean;
    function EndsWithLineBreak(const FileName: string): boolean;
    function LoadFileAsBytes(const FileName: string): TBytes;
    procedure ButtonFilterClearClick(Sender: TObject);
  private
    PadFormat: TPadFormat;
    FFileName: string;
    FChanged: boolean;
    FInitialized: boolean;
    FCommandLineFile: string;
    FFound: boolean;
    FFilterText: string;
    procedure EnumChildEditor(AEditor: TPropertyEditor);
    function EditorHasVisibleChild(AEditor: TPropertyEditor; const AFilter: string): boolean;
    function SaveFile(AFileName: string): boolean;
    function LoadFromFile(AFileName: string): boolean;
    procedure SetView;
    function IsCanClose: boolean;
    function PromptSaveChanges: TModalResult;
    procedure ClearEditor;
    procedure UpdateCaption;
    procedure OpenFile(const AFileName: string);
    procedure OpenFileFromCommandLine;
    procedure SetFileFilterForDialog;
    procedure HandleCommandLineParameters;
    function ValidateFileForOpen(const AFileName: string): boolean;
  public
  end;

var
  formPadXml: TformPadXml;

implementation

uses formabout, formdonate, systemtool, settings;

  {$R *.lfm}

  { TformPadXml }

procedure TformPadXml.FormCreate(Sender: TObject);
begin
  // Enable file dropping
  AllowDropFiles := True;

  // Initialize state
  FInitialized := False;
  FChanged := False;
  FFileName := '';
  FCommandLineFile := '';

  LoadFormSettings(Self);

  // Create PadFormat object
  PadFormat := TPadFormat.Create(Self);
  ClearEditor;
  UpdateCaption;

  // Set up property grid
  propertyPad.TIObject := PadFormat;

  // Set up file filters
  SetFileFilterForDialog;

  // Handle command line parameters
  HandleCommandLineParameters;

  // Chnage event handler for TStrings
  TStringList(PadFormat.NewsFeed.NewsFeed_Description_250_Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language1.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language1.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language1.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language2.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language2.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language2.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language3.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language3.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language3.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language4.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language4.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language4.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language5.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language5.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language5.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language6.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language6.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language6.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language7.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language7.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language7.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language8.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language8.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language8.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language9.CharDesc250Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language9.CharDesc450Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.ProgramDescriptions.Language9.CharDesc2000Strings).OnChange := @propertyPadModified;
  TStringList(PadFormat.Permissions.DistributionPermissionsStrings).OnChange := @propertyPadModified;
  TStringList(PadFormat.Permissions.EULAStrings).OnChange := @propertyPadModified;
end;

procedure TformPadXml.FormDestroy(Sender: TObject);
begin
  SaveFormSettings(Self);

  PadFormat.Free;
end;

procedure TformPadXml.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TformPadXml.FormShow(Sender: TObject);
begin
  if not FInitialized then
  begin
    FInitialized := True;

    // Open file from command line if specified
    OpenFileFromCommandLine;
  end;
end;

procedure TformPadXml.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := IsCanClose;
end;

procedure TformPadXml.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if Length(FileNames) = 0 then
    Exit;

  // Get the first dropped file
  OpenFile(FileNames[0]);
end;

procedure TformPadXml.menuAboutClick(Sender: TObject);
begin
  formAboutPadXml.ShowModal;
end;

procedure TformPadXml.menuBuyMeACoffeeClick(Sender: TObject);
begin
  formDonatePadXml.ShowModal;
end;

procedure TformPadXml.menuCheckForUpdatesClick(Sender: TObject);
begin
  CheckGithubLatestVersion;
end;

procedure TformPadXml.menuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TformPadXml.menuFileNewClick(Sender: TObject);
begin
  if not IsCanClose then Exit;

  ClearEditor;
  SetView;
  FFileName := '';
  FChanged := False;
  UpdateCaption;
end;

procedure TformPadXml.menuFileOpenClick(Sender: TObject);
begin
  if not IsCanClose then Exit;

  if dialogOpen.Execute then
  begin
    OpenFile(dialogOpen.FileName);
  end;
end;

procedure TformPadXml.menuFileSaveAsClick(Sender: TObject);
var
  TempFileName: string;
begin
  // Set initial filename in dialog
  if FFileName <> '' then
    dialogSave.FileName := ExtractFileName(FFileName)
  else
    dialogSave.FileName := 'untitled.xml';

  if dialogSave.Execute then
  begin
    TempFileName := dialogSave.FileName;

    // Ensure file has extension
    if ExtractFileExt(TempFileName) = '' then
      TempFileName := TempFileName + '.xml';

    if SaveFile(TempFileName) then
    begin
      FFileName := TempFileName;
      FChanged := False;
      UpdateCaption;
    end;
  end;
end;

procedure TformPadXml.menuFileSaveClick(Sender: TObject);
begin
  if FFileName = '' then
  begin
    // No filename yet, use Save As dialog
    menuFileSaveAsClick(Sender);
  end
  else
  begin
    // Save to current file
    if SaveFile(FFileName) then
    begin
      FChanged := False;
      UpdateCaption;
    end;
  end;
end;

function TformPadXml.SaveFile(AFileName: string): boolean;
var
  Output: TStringList;
begin
  Result := False;

  // Validate filename
  if Trim(AFileName) = '' then
  begin
    MessageDlg('Error', 'Invalid file name', mtError, [mbOK], 0);
    Exit;
  end;

  Output := TStringList.Create;
  try
    try
      Output.TrailingLineBreak := PadFormat.XmlConfig.XMLEndsWithLineBreak;

      // Get XML content
      Output.Text := PadFormat.SaveToXML;

      // Ensure directory exists
      ForceDirectories(ExtractFilePath(AFileName));

      // Save file with UTF-8 encoding
      Output.SaveToFile(AFileName, TEncoding.UTF8);

      Result := True;
    except
      on E: Exception do
      begin
        MessageDlg('Save Error', 'Error saving file:' + sLineBreak + E.Message,
          mtError, [mbOK], 0);
        Result := False;
      end;
    end;
  finally
    Output.Free;
  end;
end;

function TformPadXml.LoadFromFile(AFileName: string): boolean;
var
  Input: TStringList;
begin
  Result := False;

  if not FileExists(AFileName) then
  begin
    MessageDlg('Error', 'File does not exist: ' + AFileName, mtError, [mbOK], 0);
    Exit;
  end;

  // Check if file is readable
  try
    if FileSize(AFileName) > 50 * 1024 * 1024 then // 50 MB limit
    begin
      if MessageDlg('Large File', 'The file is very large (' + IntToStr(FileSize(AFileName) div 1024 div 1024) +
        ' MB).' + sLineBreak + 'Opening it may take a while. Continue?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        Exit;
    end;
  except
    // Ignore file size check errors
  end;

  Input := TStringList.Create;
  try
    Input.TrailingLineBreak := EndsWithLineBreak(AFileName);
    try
      // Load file with UTF-8 encoding (try UTF-8 first, fallback to ANSI)
      try
        Input.LoadFromFile(AFileName, TEncoding.UTF8);
      except
        // If UTF-8 fails, try ANSI
        Input.LoadFromFile(AFileName);
      end;

      // Load into PadFormat
      PadFormat.LoadFromXML(Input.Text);

      // Refresh property grid
      propertyPad.TIObject := nil;
      propertyPad.TIObject := PadFormat;

      SetView;
      Result := True;
    except
      on E: Exception do
      begin
        MessageDlg('Load Error', 'Error loading file:' + sLineBreak + E.Message + sLineBreak + 'File may be corrupted or in wrong format.',
          mtError, [mbOK], 0);
        Result := False;
      end;
    end;
  finally
    Input.Free;
  end;
end;

procedure TformPadXml.SetView;
begin
  menuAffiliates.Checked := PadFormat.Affiliates.Active;
  menuAllmyapps.Checked := PadFormat.Allmyapps.Active;
  menuAppStore.Checked := PadFormat.AppStore.Active;
  menuArticleContents.Checked := PadFormat.Article_Contents.Active;
  menuDeuPAD.Checked := PadFormat.DeuPAD.Active;
  menuDynamicPAD.Checked := PadFormat.Dynamic_PAD.Active;
  menuMSN.Checked := PadFormat.MSN.Active;
  menuNewsFeed.Checked := PadFormat.NewsFeed.Active;
  menuOnlineShops.Checked := PadFormat.OnlineShops.Active;
  menuPADCertificationPromotion.Checked := PadFormat.PAD_Certification_Promotion.Active;
  menuPADmap.Checked := PadFormat.PADmap.Active;
  menuPADRING.Checked := PadFormat.PADRING.Active;
  menuPressRelease.Checked := PadFormat.PressRelease.Active;
  menuRoboSoft.Checked := PadFormat.RoboSoft.Active;
  menuSimtel.Checked := PadFormat.Simtel.Active;
  menuSite.Checked := PadFormat.Site.Active;
  menuTPA.Checked := PadFormat.TPA.Active;
  menuIssues.Checked := (PadFormat.Issues <> '') or (PadFormat.ASBMPlannerID1stRound <> '') or (PadFormat.ASBMPlannerID2ndRound <> '');

  menuSectionClick(Self);
end;

function TformPadXml.IsCanClose: boolean;
var
  mr: TModalResult;
begin
  Result := True;

  if FChanged then
  begin
    mr := PromptSaveChanges;

    case mr of
      mrYes:
      begin
        // Try to save
        if FFileName = '' then
        begin
          // No filename, show Save As dialog
          dialogSave.FileName := '';
          if dialogSave.Execute then
          begin
            if not SaveFile(dialogSave.FileName) then
              Result := False  // Save was cancelled or failed
            else
            begin
              FFileName := dialogSave.FileName;
              FChanged := False;
            end;
          end
          else
            Result := False;  // User cancelled Save As dialog
        end
        else
        begin
          // Save to current file
          if not SaveFile(FFileName) then
            Result := False  // Save failed
          else
            FChanged := False;
        end;
      end;
      mrNo:
      begin
        // Don't save, just close
        Result := True;
      end;
      mrCancel:
      begin
        // Cancel closing
        Result := False;
      end;
    end;
  end;
end;

function TformPadXml.PromptSaveChanges: TModalResult;
var
  FileNameDisplay: string;
begin
  if FFileName = '' then
    FileNameDisplay := 'Untitled'
  else
    FileNameDisplay := ExtractFileName(FFileName);

  Result := MessageDlg('Save Changes', 'The document "' + FileNameDisplay + '" has been modified.' +
    sLineBreak + 'Do you want to save your changes?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

procedure TformPadXml.ClearEditor;
begin
  // Clear the PadFormat object
  PadFormat.Clear;
  PadFormat.MasterPadVersionInfo.MasterPadEditor := 'PadXml 1.0.0';

  // Refresh property grid
  propertyPad.TIObject := nil;
  propertyPad.TIObject := PadFormat;
end;

procedure TformPadXml.OpenFile(const AFileName: string);
begin
  // Validate file before opening
  if not ValidateFileForOpen(AFileName) then
    Exit;

  // Check if we need to save current changes
  if not IsCanClose then
    Exit;

  // Try to load the file
  if LoadFromFile(AFileName) then
  begin
    FFileName := AFileName;
    FChanged := False;
    UpdateCaption;
  end;
end;

procedure TformPadXml.OpenFileFromCommandLine;
begin
  if FCommandLineFile <> '' then
  begin
    // Open file from command line
    if ValidateFileForOpen(FCommandLineFile) then
    begin
      if LoadFromFile(FCommandLineFile) then
      begin
        FFileName := FCommandLineFile;
        FChanged := False;
        UpdateCaption;
      end;
    end
    else
    begin
      // File validation failed, create new empty document
      ClearEditor;
      FFileName := '';
      FChanged := False;
      UpdateCaption;
    end;
  end
  else
  begin
    // No file specified, create new empty document
    ClearEditor;
    FFileName := '';
    FChanged := False;
    UpdateCaption;
  end;
end;

procedure TformPadXml.SetFileFilterForDialog;
begin
  // Set up file filters for open/save dialogs
  dialogOpen.Filter :=
    'Pad Xml Files (*.xml, *.pad)|*.xml|*.pad|';

  dialogSave.Filter :=
    'Pad Xml Files (*.xml)|*.xml|' + 'Pad Files (*.pad)|*.pad|';

  // Set default filter
  dialogOpen.FilterIndex := 1;
  dialogSave.FilterIndex := 1;
end;

procedure TformPadXml.HandleCommandLineParameters;
var
  i: integer;
  Param: string;
  ValidExtensions: array of string;
  FileExt: string;
  j: integer;
begin
  ValidExtensions := ['.xml', '.pad'];

  // Skip the first parameter (executable path)
  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);

    // Skip empty parameters and command-line switches
    if (Param = '') or (Param[1] in ['-', '/']) then
      Continue;

    // Check if parameter is a file
    if FileExists(Param) then
    begin
      // Check file extension
      FileExt := LowerCase(ExtractFileExt(Param));
      for j := 0 to High(ValidExtensions) do
      begin
        if FileExt = ValidExtensions[j] then
        begin
          FCommandLineFile := Param;
          Break;
        end;
      end;

      if FCommandLineFile <> '' then
        Break;
    end
    else
    begin
      // Parameter might be a file path with spaces (passed without quotes)
      // Try to see if it's a partial path
      if Pos(' ', Param) > 0 then
      begin
        // This might be part of a path with spaces, we could try to reconstruct
        // For simplicity, we'll just store the first parameter that looks like a file
        FCommandLineFile := Param;
        // Note: In real application, you might want to handle quoted paths properly
      end;
    end;
  end;
end;

function TformPadXml.ValidateFileForOpen(const AFileName: string): boolean;
var
  ValidExtensions: array of string;
  FileExt: string;
  i: integer;
begin
  Result := False;

  // Check if file exists
  if not FileExists(AFileName) then
  begin
    MessageDlg('Error', 'File does not exist:' + sLineBreak + AFileName,
      mtError, [mbOK], 0);
    Exit;
  end;

  // Check if file is valid (optional - you can remove this if you want to accept any file)
  ValidExtensions := ['.xml', '.pad'];
  FileExt := LowerCase(ExtractFileExt(AFileName));

  for i := 0 to High(ValidExtensions) do
  begin
    if FileExt = ValidExtensions[i] then
    begin
      Result := True;
      Exit;
    end;
  end;

  // If file extension is not in our list, ask for confirmation
  if MessageDlg('Open File', 'The file "' + ExtractFileName(AFileName) + '" has an unrecognized extension.' +
    sLineBreak + 'Do you want to try opening it anyway?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Result := True;
  end;
end;

procedure TformPadXml.UpdateCaption;
var
  BaseTitle: string;
  AppName: string;
begin
  // Get application name from project settings or use default
  AppName := 'PadXml';

  if FFileName = '' then
    BaseTitle := 'Untitled'
  else
    BaseTitle := ExtractFileName(FFileName);

  if FChanged then
    Caption := BaseTitle + '* - ' + AppName
  else
    Caption := BaseTitle + ' - ' + AppName;

  // You can also set the application title for taskbar
  Application.Title := BaseTitle;
  if FChanged then
    Application.Title := Application.Title + '*';
end;

function TformPadXml.EndsWithLineBreak(const Buffer: TBytes): boolean;
begin
  Result := False;
  if Length(Buffer) = 0 then Exit;

  if (Buffer[High(Buffer)] = byte(#10)) or (Buffer[High(Buffer)] = byte(#13)) then
    Result := True;
end;

function TformPadXml.EndsWithLineBreak(const FileName: string): boolean;
var
  Bytes: TBytes;
begin
  Bytes := LoadFileAsBytes(FileName);
  Result := EndsWithLineBreak(Bytes);
end;

function TformPadXml.LoadFileAsBytes(const FileName: string): TBytes;
var
  FS: TFileStream;
begin
  Result := nil;
  SetLength(Result, 0);
  if not FileExists(FileName) then Exit;

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, FS.Size);
    if FS.Size > 0 then
      FS.ReadBuffer(Result[0], FS.Size);
  finally
    FS.Free;
  end;
end;

procedure TformPadXml.menuSectionClick(Sender: TObject);
begin
  propertyPad.TIObject := nil;
  propertyPad.TIObject := PadFormat;
end;

procedure TformPadXml.propertyPadModified(Sender: TObject);
begin
  if not FChanged then
  begin
    FChanged := True;
    UpdateCaption;
  end;
end;

procedure TformPadXml.ButtonFilterClearClick(Sender: TObject);
begin
  filter.Text := string.Empty;
  filterChange(Self);
end;

procedure TformPadXml.filterChange(Sender: TObject);
begin
  propertyPad.TIObject := nil;
  propertyPad.TIObject := PadFormat;
end;

function TformPadXml.EditorHasVisibleChild(AEditor: TPropertyEditor; const AFilter: string): boolean;
begin
  Result := False;

  if not (paSubProperties in AEditor.GetAttributes) then
    Exit;

  FFound := False;
  FFilterText := AFilter;

  AEditor.GetProperties(@EnumChildEditor);

  Result := FFound;
end;

procedure TformPadXml.EnumChildEditor(AEditor: TPropertyEditor);
begin
  if FFound then
    Exit;

  // match by value
  if (Pos(FFilterText, LowerCase(AEditor.GetValue)) > 0) or (Pos(FFilterText, LowerCase(AEditor.GetName)) > 0) then
  begin
    FFound := True;
    Exit;
  end;

  // go deeper
  if paSubProperties in AEditor.GetAttributes then
    if EditorHasVisibleChild(AEditor, FFilterText) then
      FFound := True;
end;

procedure TformPadXml.propertyPadEditorFilter(Sender: TObject; aEditor: TPropertyEditor; var aShow: boolean);
begin
  // hide Name
  if SameText(aEditor.GetName, 'Name') then
    aShow := False;

  // hide Tag
  if SameText(aEditor.GetName, 'Tag') then
    aShow := False;

  // Affiliates
  if SameText(aEditor.GetName, 'Affiliates') then
    aShow := menuAffiliates.Checked;

  // Allmyapps
  if SameText(aEditor.GetName, 'Allmyapps') then
    aShow := menuAllmyapps.Checked;

  // AppStore
  if SameText(aEditor.GetName, 'AppStore') then
    aShow := menuAppStore.Checked;

  // ArticleContents
  if SameText(aEditor.GetName, 'Article_Contents') then
    aShow := menuArticleContents.Checked;

  // DeuPAD
  if SameText(aEditor.GetName, 'DeuPAD') then
    aShow := menuDeuPAD.Checked;

  // DynamicPAD
  if SameText(aEditor.GetName, 'Dynamic_PAD') then
    aShow := menuDynamicPAD.Checked;

  // MSN
  if SameText(aEditor.GetName, 'MSN') then
    aShow := menuMSN.Checked;

  // NewsFeed
  if SameText(aEditor.GetName, 'NewsFeed') then
    aShow := menuNewsFeed.Checked;

  // OnlineShops
  if SameText(aEditor.GetName, 'OnlineShops') then
    aShow := menuOnlineShops.Checked;

  // PADCertificationPromotion
  if SameText(aEditor.GetName, 'PAD_Certification_Promotion') then
    aShow := menuPADCertificationPromotion.Checked;

  // PADmap
  if SameText(aEditor.GetName, 'PADmap') then
    aShow := menuPADmap.Checked;

  // PADRING
  if SameText(aEditor.GetName, 'PADRING') then
    aShow := menuPADRING.Checked;

  // PressRelease
  if SameText(aEditor.GetName, 'PressRelease') then
    aShow := menuPressRelease.Checked;

  // RoboSoft
  if SameText(aEditor.GetName, 'RoboSoft') then
    aShow := menuRoboSoft.Checked;

  // Simtel
  if SameText(aEditor.GetName, 'Simtel') then
    aShow := menuSimtel.Checked;

  // Site
  if SameText(aEditor.GetName, 'Site') then
    aShow := menuSite.Checked;

  // TPA
  if SameText(aEditor.GetName, 'TPA') then
    aShow := menuTPA.Checked;

  // Misc
  if SameText(aEditor.GetName, 'ASBMPlannerID1stRound') or SameText(aEditor.GetName, 'ASBMPlannerID2ndRound') or
    SameText(aEditor.GetName, 'Issues') or SameText(aEditor.GetName, 'tSuccess') or SameText(aEditor.GetName, 'tProcessed') or
    SameText(aEditor.GetName, 'Download_Link_Points_To_Non_Binary_File') then
    aShow := menuIssues.Checked;

  // Filter with hierarchy support
  if (LowerCase(filter.Text) <> '') and (aShow) then
  begin
    aShow :=
      (Pos(LowerCase(filter.Text), LowerCase(aEditor.GetValue)) > 0) or
      (Pos(LowerCase(filter.Text), LowerCase(aEditor.GetName)) > 0) or EditorHasVisibleChild(aEditor, LowerCase(filter.Text));
  end;
end;

end.
