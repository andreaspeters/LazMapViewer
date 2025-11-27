unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, LCLType,
  mvMapViewer, mvPluginCommon, mvMarkerPlugins, mvGPSObj, mvGeoMath, mvTypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    cgExtendedClicks: TCheckGroup;
    cgPointTypes: TCheckGroup;
    cbAllowClickOnBlueTrack: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo: TMemo;
    Panel1: TPanel;
    POI_Images: TImageList;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    rgClickBehaviour: TRadioGroup;
    Splitter1: TSplitter;
    procedure cgExtendedClicksItemClick(Sender: TObject; Index: integer);
    procedure cgPointTypesItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
  private
    Plugin: TMarkerClickPlugin;
    procedure MarkerCanClickHandler({%H-}AMapView: TMapView; APoint: TGPSPoint; var CanClick: Boolean);
    procedure MarkerClickHandler({%H-}AMapView: TMapView; APoint: TGPSPoint);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);

  // GPS markers are identified by the cross
  procedure AddGPSMarker(const ALon, ALat: Double;
    ACaption: String);
  var
    gpsPt: TGpsPointOfInterest;
  begin
    gpsPt := TGpsPointOfInterest.Create(ALon,ALat);
    try
      gpsPt.Name := ACaption;
      gpsPt.ImageIndex := -1;
      MapView.GPSItems.Add(gpsPt, 100);
      gpsPt := Nil;
    finally
      if Assigned(gpsPt) then
        gpsPt.Free;
    end;
  end;

  // Map markers are identified by an icon.
  procedure AddMapMarker(const ALon, ALat: Double; ACaption: String);
  var
    layer: TMapLayer;
    pt: TMapPointOfInterest;
  begin
    if MapView.Layers.Count = 0 then
      layer := MapView.Layers.Add as TMapLayer
    else
      layer := MapView.Layers[0];
    pt := layer.PointsOfInterest.Add as TMapPointOfInterest;
    pt.Latitude := ALat;
    pt.Longitude := ALon;
    pt.Caption := ACaption;
    pt.ImageIndex := 0;
  end;

  procedure AddGPSTrack(const APoints: TRealPointArray);
  var
    track: TGPSTrack;
    i: Integer;
  begin
    track := TGPSTrack.Create;
    for i := 0 to High(APoints) do
      {%H-}track.Points.Add(TGPSPoint.CreateFrom(APoints[i]));
    track.LineColor := clRed;
    track.LineWidth := 1.0;
    MapView.GPSItems.Add(track, 200);
  end;

  procedure AddMapTrack(const APoints: TRealPointArray);
  var
    layer: TMapLayer;
    track: TMapTrack;
    i: Integer;
  begin
    if MapView.Layers.Count =0 then
      layer := MapView.Layers.Add as TMapLayer
    else
      layer := MapView.Layers[0];
    track := layer.Tracks.Add as TMapTrack;
    track.Caption := 'Test track';
    track.LineColor := clBlue;
    track.LineWidth := 0.8;
    for i := 0 to High(APoints) do
      TMapTrackPoint(track.Points.Add).RealPoint := APoints[i];
  end;

var
  i: Integer;
begin
  MapView.Active := true;
  AddGPSMarker(   0.0000000, 51.4825766, 'Greenwich');
  AddGPSMarker(   2.2945500, 48.8582300, 'Tour d´Eiffel, Paris');
  AddGPSMarker( -79.3884000, 43.6439500, 'CN Tower, Toronto');
  AddMapMarker(-157.7739800, 21.2716900, 'Kahala Avenue, Honolulu');
  AddMapMarker( 114.1497900, 22.2708100, 'The Peak, Hong Kong');
  AddMapMarker(  13.377778,  52.5163890, 'Brandenburger Tor'+LineEnding+'Berlin');

  AddGPSTrack([RealPoint(-20,20), RealPoint(20, 0), RealPoint(-20,-20)]);
  AddMapTrack([RealPoint(20,20), RealPoint(-20,0), RealPoint(20,-20)]);

  Plugin := TMarkerClickPlugin.Create(PluginManager);
  Plugin.OnMarkerClick := @MarkerClickHandler;
  Plugin.OnCanClick := @MarkerCanClickHandler;
  // Plugin.MapView := MapView;  // Required if the MapView has a non-default cursor.

  for i := 0 to cgPointTypes.Items.Count-1 do
    cgPointTypes.Checked[i] := true;
end;

procedure TMainForm.cgPointTypesItemClick(Sender: TObject; Index: integer);
var
  pointTypes: TMvPointTypes;
begin
  pointTypes := Plugin.PointTypes;
  if cgPointTypes.Checked[Index] then
    Include(pointTypes, TMvPointType(Index))
  else
    Exclude(pointtypes, TMvPointType(Index));
  Plugin.PointTypes := pointTypes;
end;

procedure TMainForm.cgExtendedClicksItemClick(Sender: TObject; Index: integer);
var
  clicks: TMvExtendedClicks;
begin
  clicks := Plugin.ExtendedClicks;
  if cgExtendedClicks.Checked[Index] then
    clicks := clicks + [TMvExtendedClick(Index)]
  else
    clicks := clicks - [TMvExtendedClick(Index)];
  Plugin.ExtendedClicks := clicks;
end;

procedure TMainForm.MarkerCanClickHandler(AMapView: TMapView; APoint: TGPSPoint;
  var CanClick: Boolean);
var
  mapTrack: TMapTrack;
  i: Integer;
begin
  if cbAllowClickOnBlueTrack.Checked then
    exit;
  // Disallow clicking on the points of the (blue) map track
  mapTrack := MapView.Layers[0].Tracks[0];
  for i := 0 to mapTrack.Points.Count-1 do
  begin
    CanClick := not APoint.RealPoint.Equal(mapTrack.Points[i].RealPoint);
    if not CanClick then exit;
  end;
end;

procedure TMainForm.MarkerClickHandler(AMapView: TMapView; APoint: TGPSPoint);
var
  s, sName, sLat, sLon: String;
begin
  sName := APoint.Name;
  sLat := '  Latitude ' + LatToStr(APoint.Lat, true);
  sLon := '  Longitude ' + LonToStr(APoint.Lon, true);
  s := sLat + LineEnding + sLon;
  if sName <> '' then
    s := sName + LineEnding + s;
  case rgClickBehaviour.ItemIndex of
    0: ShowMessage(s);
    1: Memo.Lines.Add(s + LineEnding);
  end;
end;

end.

