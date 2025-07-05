program MapViewer_Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Main, gpslistform, ConfigFrame, configframe_with_addons;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title := 'MapViewer_Demo_with_Addons';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TGPSListViewer, GPSListViewer);
  Application.Run;
end.

