//-----------------------------------------------------------------------------------
//  PadXml © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/padxml
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit formabout;

{$mode ObjFPC}{$H+}

interface

uses
  Forms,
  StdCtrls,
  ExtCtrls,
  LCLIntf;

type

  { TformAboutPadXml }

  TformAboutPadXml = class(TForm)
    buttonOk: TButton;
    imageLogo: TImage;
    labelBy: TLabel;
    labelName: TLabel;
    labelLic: TLabel;
    LabelLicUrl: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure LabelLicUrlClick(Sender: TObject);
  private
  public
  end;

var
  formAboutPadXml: TformAboutPadXml;

implementation

uses checkupdates;

  {$R *.lfm}

  { TformAboutPadXml }

procedure TformAboutPadXml.LabelLicUrlClick(Sender: TObject);
begin
  OpenUrl(labelLicUrl.Caption);
end;

procedure TformAboutPadXml.FormCreate(Sender: TObject);
begin
  labelName.Caption := 'PadXml © ' + GetAppVersion;
end;

end.
