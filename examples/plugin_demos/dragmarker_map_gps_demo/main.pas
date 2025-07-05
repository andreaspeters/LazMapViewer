unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  mvDLEFpc, mvMapViewer, mvTypes, mvPluginCommon, mvPlugins, mvMapGridPlugin,
  mvGPSObj;

type

  { TMainForm }

  TMainForm = class(TForm)
    POI_Images: TImageList;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    DraggableMarkerPlugin: TDraggableMarkerPlugin;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);

  procedure AddTraditionalMarker(AMapView: TMapView; const ALon, ALat: Double;
    ACaption: String);
  var
    gpsPt: TGpsPointOfInterest;
  begin
    gpsPt := TGpsPointOfInterest.Create(ALon,ALat);
    try
      gpsPt.Name := ACaption;
      gpsPt.ImageIndex := -1;
      AMapView.GPSItems.Add(gpsPt, 100);
      gpsPt := Nil;
    finally
      if Assigned(gpsPt) then
        gpsPt.Free;
    end;
  end;

  procedure AddMapMarker(AMapView: TMapView; const ALon, ALat: Double;
    ACaption: String; AImageIndex: Integer);
  var
    pt: TMapPoint;
    layer: TMapLayer;
  begin
    if AMapView.Layers.Count =0 then
      layer := AMapView.Layers.Add as TMapLayer
    else
      layer := AMapView.Layers[0];
    pt := TMapPointOfInterest.Create(layer.PointsOfInterest);
    pt.RealPoint := RealPoint(ALat, ALon);
    pt.Caption := ACaption;
    TMapPointOfInterest(pt).ImageIndex := AImageIndex;
  end;

  procedure AddMapArea(AMapView: TMapView; const LatLon: array of double);
  var
    layer: TMapLayer;
    mapArea: TMapArea;
    mapAreaPoint: TMapAreaPoint;
    i, j: Integer;
  begin
    if odd(Length(LatLon)) then
      exit;

    if AMapView.Layers.Count= 0 then
      layer := AMapView.Layers.Add as TMapLayer
    else
      layer := AMapView.Layers[0];
    mapArea := TMapArea.Create(layer.Areas);
    mapArea.LineWidth := 1;
    mapArea.LineColor := clBlue;
    j := 0;
    while j < High(LatLon) do
    begin
      mapAreaPoint := TMapAreaPoint.Create(mapArea.Points);
      mapAreaPoint.RealPoint := RealPoint(LatLon[j], LatLon[j+1]);
      inc(j, 2);
    end;
  end;

  procedure AddMapTrack(AMapView: TMapView; const LatLon: array of double);
  var
    layer: TMapLayer;
    track: TMapTrack;
    trackPoint: TMapTrackPoint;
    i, j: Integer;
  begin
    if odd(Length(LatLon)) then
      exit;

    if AMapView.Layers.Count= 0 then
      layer := AMapView.Layers.Add as TMapLayer
    else
      layer := AMapView.Layers[0];
    track := TMapTrack.Create(layer.Tracks);
    track.LineWidth := 1;
    track.LineColor := clRed;
    j := 0;
    while j < High(LatLon) do
    begin
      trackPoint := TMapTrackPoint.Create(track.Points);
      trackPoint.RealPoint := RealPoint(LatLon[j], LatLon[j+1]);
      inc(j, 2);
    end;
  end;

begin
  MapView.Active := true;
  MapView.Zoom := 1;
  AddTraditionalMarker(MapView,   -79.3884000,  43.6439500, 'CN Tower, Toronto');
  AddTraditionalMarker(MapView,   114.1497900,  22.2708100, 'The Peak, Hong Kong');
  AddTraditionalMarker(MapView,    13.377778,   52.5163890, 'Brandenburger Tor, Berlin');
  AddTraditionalMarker(MapView,   -58.3722400, -34.6084700, 'Pirámide de Mayo, Buenos Aires');

  AddMapMarker(MapView,    0.0000000,  51.4825766, 'Greenwich', 0);
  AddMapMarker(MapView, -157.7739800,  21.2716900, 'Kahala Avenue, Honolulu', 1);
  AddMapArea(MapView, [0,-45, 0,45, -45,0]);
  AddMapTrack(MapView, [30,-100, 20,-50, -20,-0]);

  MapView.Invalidate;
end;

end.

