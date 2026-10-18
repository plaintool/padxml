//-----------------------------------------------------------------------------------
//  PadXml © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/padxml
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

unit FormDonate;

{$mode ObjFPC}{$H+}

interface

uses
  Forms,
  Classes,
  StdCtrls,
  Buttons,
  Clipbrd,
  Graphics,
  LCLIntf, ExtCtrls;

type

  { TformDonatePadXml }

  TformDonatePadXml = class(TForm)
    BtnOk: TButton;
    ImageBank: TImage;
    ImageCrypto: TImage;
    LabelDonate: TLabel;
    LabelSupport: TLabel;
    LabelContribution: TLabel;
    labelBank: TLabel;
    labelCrypto: TLabel;
    procedure labelBankClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  formDonatePadXml: TformDonatePadXml;

implementation

uses darkutils;

  {$R *.lfm}

  { TformDonatePadXml }

procedure TformDonatePadXml.FormCreate(Sender: TObject);
begin
  labelBank.Font.Color := TDarkUtils.ThemeColor(clBlue, clSkyBlue);
  //labelCrypto.Font.Color := TDarkUtils.ThemeColor(clBlue, clSkyBlue);
  labelCrypto.Font.Color := TDarkUtils.ThemeColor(clBlue, clSkyBlue);
end;

procedure TformDonatePadXml.labelBankClick(Sender: TObject);
begin
  OpenUrl((Sender as TLabel).Hint);
end;

end.
