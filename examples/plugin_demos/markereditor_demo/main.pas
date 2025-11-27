unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, LCLType,
  mvMapViewer, mvPluginCommon, mvMarkerPlugins, mvGPSObj, mvTypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnDeleteSelection: TButton;
    btnConvertToTrack: TButton;
    btnConvertToArea: TButton;
    cgPointTypes: TCheckGroup;
    cgOptions: TCheckGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    POI_Images: TImageList;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    cgExtendedClicks: TCheckGroup;
    rgNewPointType: TRadioGroup;
    procedure btnDeleteSelectionClick(Sender: TObject);
    procedure btnConvertToTrackClick(Sender: TObject);
    procedure btnConvertToAreaClick(Sender: TObject);
    procedure cgExtendedClicksItemClick(Sender: TObject; Index: integer);
    procedure cgOptionsItemClick(Sender: TObject; Index: integer);
    procedure cgPointTypesItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure rgNewPointTypeClick(Sender: TObject);
  private
    Plugin: TMarkerEditorPlugin;
    procedure MapViewKeyDownHandler(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NewPointHandler(AMapView: TMapView; APoint: TGPSPoint);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);

  // GPS markers are identified by the cross
  procedure AddGPSMarker(APoint: TRealPoint; ACaption: String);
  var
    gpsPt: TGpsPointOfInterest;
  begin
    gpsPt := TGpsPointOfInterest.CreateFrom(APoint);
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
  procedure AddMapMarker(APoint: TRealPoint; ACaption: String);
  var
    layer: TMapLayer;
    pt: TMapPointOfInterest;
  begin
    if MapView.Layers.Count = 0 then
      layer := MapView.Layers.Add as TMapLayer
    else
      layer := MapView.Layers[0];
    pt := layer.PointsOfInterest.Add as TMapPointOfInterest;
    pt.RealPoint := APoint;
    pt.Caption := ACaption;
    pt.ImageIndex := 0;
  end;

  procedure AddGPSTrack(const APoints: TRealPointArray);
  var
    track: TGPSTrack;
    i: Integer;
  begin
    track := TGPSTrack.Create;
    track.Name := 'Test GPS track';
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
    track.Caption := 'Test MAP track';
    track.LineColor := clBlue;
    track.LineWidth := 0.8;
    for i := 0 to High(APoints) do
      TMapTrackPoint(track.Points.Add).RealPoint := APoints[i];
  end;

  procedure AddGPSArea(const APoints: TRealPointArray);
  var
    area: TGPSArea;
    i: Integer;
  begin
    area := TGPSArea.Create;
    area.Name := 'Test GPS area';
    area.FillColor := clRed;
    area.LineColor := clMaroon;
    area.Opacity := 0.5;
    for i := 0 to High(APoints) do
      area.Points.Add(TGPSPoint.CreateFrom(APoints[i]));
    MapView.GPSItems.Add(area, 201);
  end;

  procedure AddMapArea(const APoints: TRealPointArray);
  var
    layer: TMapLayer;
    area: TMapArea;
    i: Integer;
  begin
    if MapView.Layers.Count = 0 then
      layer := MapView.Layers.Add as TMapLayer
    else
      layer := MapView.Layers[0];
    area := layer.Areas.Add as TMapArea;
    area.Caption := 'Test Map area';
    area.FillColor := clBlue;
    area.LineColor := clNavy;
    area.Opacity := 0.5;
    for i := 0 to High(APoints) do
      TMapAreaPoint(area.Points.Add).RealPoint := APoints[i];
  end;

var
  i: Integer;
begin
  MapView.Active := true;

  AddGPSMarker(RealPoint(51.4825766,    0.000000), 'Greenwich');
  AddGPSMarker(RealPoint(48.8582300,    2.294550), 'Tour d´Eiffel, Paris');
  AddGPSMarker(RealPoint(43.6439500,  -79.388400), 'CN Tower, Toronto');
  AddMapMarker(RealPoint(21.2716900, -157.773980), 'Kahala Avenue, Honolulu');
  AddMapMarker(RealPoint(22.2708100,  114.149790), 'The Peak, Hong Kong');
  AddMapMarker(RealPoint(52.5163890,   13.377778), 'Brandenburger Tor, Berlin');

  AddGPSTrack([RealPoint(-20,20), RealPoint(20, 0), RealPoint(-20,-20)]);
  AddMapTrack([RealPoint(20,20), RealPoint(-20,0), RealPoint(20,-20)]);

  AddGPSArea ([RealPoint(-60, -142), RealPoint(-70, -148), RealPoint(-64, -129)]);
  AddMapArea ([RealPoint(-60, -42), RealPoint(-70, -48), RealPoint(-64, -29)]);

  Plugin := TMarkerEditorPlugin.Create(PluginManager);
  Plugin.OnNewPoint := @NewPointHandler;

  MapView.TabStop := true;
  MapView.OnKeyDown := @MapViewKeydownHandler;

  for i := 0 to cgPointTypes.Items.Count-1 do
    cgPointTypes.Checked[i] := true;

  for i := 0 to cgOptions.Items.Count-1 do
    cgOptions.Checked[i] := (TMarkerOption(i) in Plugin.Options);
end;

procedure TMainForm.MapViewKeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if MapView.Focused then
    case Key of
      VK_DELETE:
        if (Plugin.Selection.Count > 0) and
           (MessageDlg('Delete selected points?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          Plugin.DeleteSelectedPoints(MapView);
        end;
      VK_LEFT:
        Plugin.MoveSelectionBy(MapView, -1.0, 0.0);
      VK_RIGHT:
        Plugin.MoveSelectionBy(MapView, +1.0, 0.0);
      VK_UP:
        Plugin.MoveSelectionBy(MapView, 0.0, 1.0);
      VK_DOWN:
        Plugin.MoveSelectionBy(MapView, 0.0, -1.0);
      else
        inherited;
        exit;
  end;
  MapView.Invalidate;
  Key := 0;
end;

procedure TMainForm.rgNewPointTypeClick(Sender: TObject);
begin
  plugin.NewPointType := TMarkerNewPointType(rgNewPointType.ItemIndex);
end;

procedure TMainForm.cgPointTypesItemClick(Sender: TObject; Index: integer);
var
  pointTypes: TMvPointTypes;
begin
  pointTypes := Plugin.PointTypes;
  if cgPointTypes.Checked[Index] then
    Include(pointTypes, TMvPointType(Index))
  else
    Exclude(pointTypes, TMvPointType(Index));
  Plugin.PointTypes := pointTypes;
end;

procedure TMainForm.btnDeleteSelectionClick(Sender: TObject);
begin
  Plugin.DeleteSelectedPoints(MapView);
end;

procedure TMainForm.btnConvertToTrackClick(Sender: TObject);
const
  counter: Integer = 0;
begin
  case Plugin.NewPointType of
    nptGPSPoint:
      with Plugin.ConvertSelectedPointsToGPSTrack(MapView, 2000 + counter) do
      begin
        LineColor := clGreen;
        lineWidth := 1.0;
      end;
    nptMapPoint:
      with Plugin.ConvertSelectedPointsToMapTrack(MapView, MapView.Layers[0]) do
      begin
        LineColor := clGreen;
        LineWidth := 1.0;
      end;
  end;
  inc(counter);
end;

procedure TMainForm.btnConvertToAreaClick(Sender: TObject);
var
  counter: Integer = 0;
begin
  case rgNewPointType.ItemIndex of
    0: with Plugin.ConvertSelectedPointsToGPSArea(MapView, 3000 + counter) do
       begin
         FillColor := clGray;
         LineColor := clBlack;
         Opacity := 0.5;
       end;
    1: with Plugin.ConvertSelectedPointsToMapArea(MapView, MapView.Layers[0]) do
       begin
         FillColor := clBlack;
         LineColor := clBlack;
         Opacity := 0.5;
       end;
  end;
  inc(counter);
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

procedure TMainForm.cgOptionsItemClick(Sender: TObject; Index: integer);
var
  optns: TMarkerOptions;
begin
  optns := Plugin.Options;
  if cgOptions.Checked[Index] then
    Include(optns, TMarkerOption(Index))
  else
    Exclude(optns, TMarkerOption(Index));
  Plugin.Options := optns;
end;

procedure TMainForm.NewPointHandler(AMapView: TMapView; APoint: TGPSPoint);
begin
  if (Plugin.NewPointType = nptMapPoint) and (APoint is TGPSPointOfInterest) then
    TGPSPointOfInterest(APoint).ImageIndex := 0;
end;

end.

