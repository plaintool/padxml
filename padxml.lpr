//-----------------------------------------------------------------------------------
//  PadXml © 2025 by Alexander Tverskoy
//  https://github.com/plaintool/padxml
//  Licensed under the MIT License
//  You may obtain a copy of the License at https://opensource.org/licenses/MIT
//-----------------------------------------------------------------------------------

program padxml;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mainunit,
  formabout,
  formdonate;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'PadXml';
  Application.Scaled := True;
  {$PUSH}
  {$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TformPadXml, formPadXml);
  Application.CreateForm(TformAboutPadXml, formAboutPadXml);
  Application.CreateForm(TformDonatePadXml, formDonatePadXml);
  Application.Run;
end.
