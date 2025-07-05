unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils, Forms, Controls, Graphics,
  Dialogs, mvMapViewer, mvTypes, mvGeoMath;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    MapView1: TMapView;
    Panel1: TPanel;
    rgCenter: TRadioGroup;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure MapView1Change(Sender: TObject);
    procedure rgCenterClick(Sender: TObject);
  private
    FInitialArea: TRealArea;
    FMinZoom: Integer;
    procedure MapCenterMoving(Sender: TObject; var NewCenter: TRealPoint; var Allow: Boolean);
    procedure MapZoomChanging(Sender: TObject; NewZoom: Integer; var Allow: Boolean);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  rgCenterClick(nil);
  MapView1Change(nil);
end;

procedure TForm1.MapView1Change(Sender: TObject);
begin
  Statusbar1.Panels[0].Text := 'Latitude ' + LatToStr(MapView1.Center.Lat, true);
  Statusbar1.Panels[1].Text := 'Longitude ' + LonToStr(MapView1.Center.Lon, true);
  Statusbar1.Panels[2].Text := 'Zoom ' + IntToStr(MapView1.Zoom);
end;

procedure TForm1.rgCenterClick(Sender: TObject);
const
  INFO = '%s cannot be moved out of the window.'#13'Only zoom levels > %d allowed.';
begin
  MapView1.OnZoomChanging := nil;
  MapView1.OnCenterMoving := nil;
  case rgCenter.ItemIndex of
    0: begin  // London
         MapView1.MapCenter.Longitude := -DMSToDeg(0, 7, 54.6);
         MapView1.MapCenter.Latitude := DMSToDeg(51, 30, 31.2);
         FMinZoom := 8;
         Label1.Caption := Format(INFO, ['London', FMinZoom]);
       end;
    1: begin   // somewhere in Siberia near dateline
         MapView1.MapCenter.Longitude := 178;
         MapView1.MapCenter.Latitude := 64.7;
         FMinZoom := 6;
         Label1.Caption := Format(INFO, ['Center', FMinZoom]);
       end;
  end;
  MapView1.Zoom := FMinZoom;
  FInitialArea := MapView1.GetVisibleArea;
  MapView1.OnZoomChanging := @MapZoomChanging;
  MapView1.OnCenterMoving := @MapCenterMoving;
end;

procedure TForm1.MapCenterMoving(Sender: TObject; var NewCenter: TRealPoint;
  var Allow: Boolean);
begin
  if not FInitialArea.ContainsPoint(NewCenter) then
    FInitialArea.MakeAreaPoint(NewCenter);
end;

procedure TForm1.MapZoomChanging(Sender: TObject; NewZoom: Integer; var Allow: Boolean);
begin
  Allow := NewZoom >= FMinZoom;
end;

end.

