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

      Result := True;
    finally
      JSONData.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
