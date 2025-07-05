unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs,
  mvMapViewer, mvTypes, mvGeoMath, mvPluginCommon, mvAreaSelectionPlugin;

type

  { TMainForm }

  TMainForm = class(TForm)
    lblHeader: TStaticText;
    MapView: TMapView;
    Memo: TMemo;
    PluginManager: TMvPluginManager;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);

  private
    FAreaSelectionPlugin1: TAreaSelectionPlugin;
    FAreaSelectionPlugin2: TAreaSelectionPlugin;
    procedure OnSelectedAreaChanged(Sender: TObject);
    procedure OnSelectedAreaChanging(Sender: TObject; ANewArea: TRealArea; var Allow: Boolean);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView.Active := true;
  FAreaSelectionPlugin1 := TAreaSelectionPlugin.Create(PluginManager);
  FAreaSelectionPlugin1.Caption := 'Area 1';
  FAreaSelectionPlugin1.Pen.Color := clNavy;
  FAreaSelectionPlugin1.BackgroundColor := clNavy;
  FAreaSelectionPlugin1.Font.Color := clWhite;
  FAreaSelectionPlugin1.MapView := MapView;
  FAreaSelectionPlugin1.OnSelectedAreaChanged := @OnSelectedAreaChanged;
  FAreaSelectionPlugin1.OnSelectedAreaChanging := @OnSelectedAreaChanging;

  FAreaSelectionPlugin2 := TAreaSelectionPlugin.Create(PluginManager);
  FAreaSelectionPlugin2.Caption := 'Area 2';
  FAreaSelectionPlugin2.CaptionPosition := ascpRightBottom;
  FAreaSelectionPlugin2.Pen.Color := clRed;
//  FAreaSelectionPlugin2.BackgroundColor := clRed;
//  FAreaSelectionPlugin2.Font.Color := clWhite;
  FAreaSelectionPlugin2.MapView := MapView;
  FAreaSelectionPlugin2.SelectedArea.Area.Init(-110, 40, 110, -60);
  FAreaSelectionPlugin2.OnSelectedAreaChanged := @OnSelectedAreaChanged;
  FAreaSelectionPlugin2.OnSelectedAreaChanging := @OnSelectedAreaChanging;

  lblHeader.Caption := '                                  Left     Top   Right  Bottom';
end;

procedure TMainForm.OnSelectedAreaChanged(Sender: TObject);
var
  plugin: TAreaSelectionPlugin;
begin
  plugin := Sender as TAreaSelectionPlugin;
  with plugin.SelectedArea do
    Memo.Lines.Add('[%s] %s  Selected: %6.1f° %6.1f° %6.1f° %6.1f°',[
      FormatDateTime('hh:nn:ss.zzz', Now()),
      plugin.Caption,
      Area.TopLeft.Lon,
      Area.TopLeft.Lat,
      Area.BottomRight.Lon,
      Area.BottomRight.Lat
    ]);
end;

procedure TMainForm.OnSelectedAreaChanging(Sender: TObject; ANewArea: TRealArea;
  var Allow: Boolean);
var
  plugin: TAreaSelectionPlugin;
begin
  plugin := Sender as TAreaSelectionPlugin;
  with ANewArea do
    Memo.Lines.Add('[%s] %s Selecting: %6.1f° %6.1f° %6.1f° %6.1f°',[
      FormatDateTime('hh:nn:ss.zzz', Now()),
      plugin.Caption,
      TopLeft.Lon,
      TopLeft.Lat,
      BottomRight.Lon,
      BottomRight.Lat
    ]);
end;

end.

