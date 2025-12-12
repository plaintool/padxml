unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, padformat,
  RTTIGrids, PropEdits, ObjectInspector;

type

  { TformPadXml }

  TformPadXml = class(TForm)
    MainMenu: TMainMenu;
    menuFile: TMenuItem;
    menuFileOpen: TMenuItem;
    menuFileSave: TMenuItem;
    menuFileExit: TMenuItem;
    menuFileSaveAs: TMenuItem;
    menuFileNew: TMenuItem;
    dialogOpen: TOpenDialog;
    peditPad: TTIPropertyGrid;
    dialogSave: TSaveDialog;
    Separator1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure menuFileNewClick(Sender: TObject);
    procedure menuFileOpenClick(Sender: TObject);
    procedure menuFileSaveAsClick(Sender: TObject);
    procedure menuFileSaveClick(Sender: TObject);
    procedure peditPadEditorFilter(Sender: TObject; aEditor: TPropertyEditor; var aShow: boolean);
    procedure peditPadModified(Sender: TObject);
  private
    PadFormat: TPadFormat;
    FFileName: string;
    FChanged: boolean;
    function SaveFile(AFileName: string): boolean;
    function IsCanClose: boolean;
    procedure SetInfo;
  public
  end;

var
  formPadXml: TformPadXml;

implementation

{$R *.lfm}

{ TformPadXml }

procedure TformPadXml.FormCreate(Sender: TObject);
//var
//  XMLContent: string;
begin
  PadFormat := TPadFormat.Create(Self);
  //with TStringList.Create do
  //try
  //  LoadFromFile('E:\pads\DbSchema4.0.xml');
  //  XMLContent := Text;
  //  PadFormat.LoadFromXML(XMLContent);
  //finally
  //  Free;
  //end;

  peditPad.TIObject := PadFormat;
end;

procedure TformPadXml.FormDestroy(Sender: TObject);
begin
  //with TStringList.Create do
  //try
  //  Text := PadFormat.SaveToXML;
  //  SaveToFile('E:\pads\_DbSchema4.0.xml');
  //finally
  //  Free;
  //end;

  PadFormat.Free;
end;

procedure TformPadXml.menuFileNewClick(Sender: TObject);
begin
  PadFormat.Clear;
  FFileName := string.Empty;
end;

procedure TformPadXml.menuFileOpenClick(Sender: TObject);
begin
  if dialogOpen.Execute then
  begin
  end;
end;

procedure TformPadXml.menuFileSaveAsClick(Sender: TObject);
begin
  if dialogSave.Execute then
  begin
  end;
end;

procedure TformPadXml.menuFileSaveClick(Sender: TObject);
begin
  SaveFile(FFileName);
end;

function TformPadXml.SaveFile(AFileName: string): boolean;
begin
  Result := True;
end;

function TformPadXml.IsCanClose: boolean;
var
  UserResponse: integer;
begin
  if FChanged then
  begin
    // Show message with Yes, No, and Cancel options
    UserResponse := MessageDlg('Save changes?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);

    case UserResponse of
      mrYes:
      begin
        // Call save method and allow form to close
        Result := SaveFile(FFileName);
      end;
      mrNo:
      begin
        // Do not save, but allow form to close
        Result := True;
      end;
      else
        Result := False;
    end;
  end
  else
    Result := True; // No changes, just close the form
end;

procedure TformPadXml.SetInfo;
begin
  Caption := FFileName; // TODO if changed *
end;

procedure TformPadXml.peditPadModified(Sender: TObject);
begin
  FChanged := True;
  SetInfo;
end;

procedure TformPadXml.peditPadEditorFilter(Sender: TObject; aEditor: TPropertyEditor; var aShow: boolean);
begin
  // hide Name
  if SameText(aEditor.GetName, 'Name') then
    aShow := False;

  // hide Tag
  if SameText(aEditor.GetName, 'Tag') then
    aShow := False;
end;

end.
