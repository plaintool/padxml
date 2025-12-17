//-----------------------------------------------------------------------------------
//  PadXml © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/padxml
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit settings;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Forms,
  Classes,
  SysUtils,
  fpjson,
  Graphics,
  mainunit;

procedure SaveFormSettings(Form: TformPadXml);

function LoadFormSettings(Form: TformPadXml): boolean;

implementation

uses systemtool;

function GetSettingsDirectory(fileName: string = ''): string;
begin
  {$IFDEF Windows}
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('LOCALAPPDATA')) + 'padxml\'+fileName;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(GetUserDir) + '.config/padxml/' + filename;
  {$ENDIF}
end;

procedure SaveFormSettings(Form: TformPadXml);
var
  JSONObj: TJSONObject;
  FileName: string;
begin
  FileName := GetSettingsDirectory('form_settings.json'); // Get settings file name
  ForceDirectories(GetSettingsDirectory); // Ensure the directory exists
  JSONObj := TJSONObject.Create;
  try
    // Save form position and size
    if (Form.WindowState in [wsMaximized, wsMinimized]) then
    begin
      JSONObj.Add('Left', Form.RestoredLeft);
      JSONObj.Add('Top', Form.RestoredTop);
      JSONObj.Add('Width', Form.RestoredWidth);
      JSONObj.Add('Height', Form.RestoredHeight);
    end
    else
    begin
      JSONObj.Add('Left', Form.Left);
      JSONObj.Add('Top', Form.Top);
      JSONObj.Add('Width', Form.Width);
      JSONObj.Add('Height', Form.Height);
    end;
    JSONObj.Add('WindowState', Ord(Form.WindowState));

    JSONObj.Add('SplitterX', Form.propertyPad.SplitterX);

    JSONObj.Add('Affiliates', Form.menuAffiliates.Checked);
    JSONObj.Add('Allmyapps', Form.menuAllmyapps.Checked);
    JSONObj.Add('AppStore', Form.menuAppStore.Checked);
    JSONObj.Add('ArticleContents', Form.menuArticleContents.Checked);
    JSONObj.Add('ASBMPlanner', Form.menuASBMPlanner.Checked);
    JSONObj.Add('DeuPAD', Form.menuDeuPAD.Checked);
    JSONObj.Add('DynamicPAD', Form.menuDynamicPAD.Checked);
    JSONObj.Add('MSN', Form.menuMSN.Checked);
    JSONObj.Add('NewsFeed', Form.menuNewsFeed.Checked);
    JSONObj.Add('OnlineShops', Form.menuOnlineShops.Checked);
    JSONObj.Add('PADCertificationPromotion', Form.menuPADCertificationPromotion.Checked);
    JSONObj.Add('PADmap', Form.menuPADmap.Checked);
    JSONObj.Add('PADRING', Form.menuPADRING.Checked);
    JSONObj.Add('PressRelease', Form.menuPressRelease.Checked);
    JSONObj.Add('RoboSoft', Form.menuRoboSoft.Checked);
    JSONObj.Add('Simtel', Form.menuSimtel.Checked);
    JSONObj.Add('Site', Form.menuSite.Checked);
    JSONObj.Add('TPA', Form.menuTPA.Checked);

    // Write to file
    with TStringList.Create do
    try
      Add(JSONObj.AsJSON);
      SaveToFile(FileName);
    finally
      Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

function LoadFormSettings(Form: TformPadXml): boolean;
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  FileName: string;
  FileStream: TFileStream;
  FileContent: string;
begin
  Result := False;
  FileContent := string.Empty;
  FileName := GetSettingsDirectory('form_settings.json'); // Get the settings file name
  if not FileExists(FileName) then Exit; // Exit if the file does not exist

  // Read from file
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(FileContent, FileStream.Size);
    FileStream.Read(Pointer(FileContent)^, FileStream.Size);
    JSONData := GetJSON(FileContent);
    try
      JSONObj := JSONData as TJSONObject;

      // Check and load form's position and size
      if JSONObj.FindPath('Left') <> nil then
        Form.Left := JSONObj.FindPath('Left').AsInteger;

      if JSONObj.FindPath('Top') <> nil then
        Form.Top := JSONObj.FindPath('Top').AsInteger;

      if JSONObj.FindPath('Width') <> nil then
        Form.Width := JSONObj.FindPath('Width').AsInteger;

      if JSONObj.FindPath('Height') <> nil then
        Form.Height := JSONObj.FindPath('Height').AsInteger;

      if JSONObj.FindPath('WindowState') <> nil then
        Form.WindowState := TWindowState(JSONObj.FindPath('WindowState').AsInteger);

      if JSONObj.FindPath('SplitterX') <> nil then
      begin
        Form.propertyPad.SplitterX := JSONObj.FindPath('SplitterX').AsInteger;
        Form.propertyPad.PreferredSplitterX := Form.propertyPad.SplitterX;
      end;

      if JSONObj.FindPath('Affiliates') <> nil then
        Form.menuAffiliates.Checked := JSONObj.FindPath('Affiliates').AsBoolean;

      if JSONObj.FindPath('Allmyapps') <> nil then
        Form.menuAllmyapps.Checked := JSONObj.FindPath('Allmyapps').AsBoolean;

      if JSONObj.FindPath('AppStore') <> nil then
        Form.menuAppStore.Checked := JSONObj.FindPath('AppStore').AsBoolean;

      if JSONObj.FindPath('ArticleContents') <> nil then
        Form.menuArticleContents.Checked := JSONObj.FindPath('ArticleContents').AsBoolean;

      if JSONObj.FindPath('ASBMPlanner') <> nil then
        Form.menuASBMPlanner.Checked := JSONObj.FindPath('ASBMPlanner').AsBoolean;

      if JSONObj.FindPath('DeuPAD') <> nil then
        Form.menuDeuPAD.Checked := JSONObj.FindPath('DeuPAD').AsBoolean;

      if JSONObj.FindPath('DynamicPAD') <> nil then
        Form.menuDynamicPAD.Checked := JSONObj.FindPath('DynamicPAD').AsBoolean;

      if JSONObj.FindPath('MSN') <> nil then
        Form.menuMSN.Checked := JSONObj.FindPath('MSN').AsBoolean;

      if JSONObj.FindPath('NewsFeed') <> nil then
        Form.menuNewsFeed.Checked := JSONObj.FindPath('NewsFeed').AsBoolean;

      if JSONObj.FindPath('OnlineShops') <> nil then
        Form.menuOnlineShops.Checked := JSONObj.FindPath('OnlineShops').AsBoolean;

      if JSONObj.FindPath('PADCertificationPromotion') <> nil then
        Form.menuPADCertificationPromotion.Checked := JSONObj.FindPath('PADCertificationPromotion').AsBoolean;

      if JSONObj.FindPath('PADmap') <> nil then
        Form.menuPADmap.Checked := JSONObj.FindPath('PADmap').AsBoolean;

      if JSONObj.FindPath('PADRING') <> nil then
        Form.menuPADRING.Checked := JSONObj.FindPath('PADRING').AsBoolean;

      if JSONObj.FindPath('PressRelease') <> nil then
        Form.menuPressRelease.Checked := JSONObj.FindPath('PressRelease').AsBoolean;

      if JSONObj.FindPath('RoboSoft') <> nil then
        Form.menuRoboSoft.Checked := JSONObj.FindPath('RoboSoft').AsBoolean;

      if JSONObj.FindPath('Simtel') <> nil then
        Form.menuSimtel.Checked := JSONObj.FindPath('Simtel').AsBoolean;

      if JSONObj.FindPath('Site') <> nil then
        Form.menuSite.Checked := JSONObj.FindPath('Site').AsBoolean;

      if JSONObj.FindPath('TPA') <> nil then
        Form.menuTPA.Checked := JSONObj.FindPath('TPA').AsBoolean;

      Result := True;
    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
