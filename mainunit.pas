unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, padformat, RTTIGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    peditPad: TTIPropertyGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    PadFormat: TPadFormat;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  XMLContent: string;
begin
  PadFormat := TPadFormat.Create(Self);
  with TStringList.Create do
  try
    LoadFromFile('E:\padxml\samples\pad4.0.xml');
    XMLContent := Text;
    PadFormat.LoadFromXML(XMLContent);
  finally
    Free;
  end;


  peditPad.TIObject := PadFormat;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

  with TStringList.Create do
  try
    Text := PadFormat.SaveToXML;
    SaveToFile('E:\padxml\samples\_pad4.0.xml');
  finally
    Free;
  end;

  PadFormat.Free;
end;

end.
