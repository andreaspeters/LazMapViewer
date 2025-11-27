unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls, LCLType, Types,
  mvMapViewer, mvPluginCommon, mvMarkerPlugins, mvGPSObj, mvGeoMath, mvTypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    cgPointTypes: TCheckGroup;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    POI_Images: TImageList;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    rgHintType: TRadioGroup;
    procedure cgPointTypesItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure rgHintTypeClick(Sender: TObject);
  private
    Plugin: TMarkerHintPlugin;
    procedure MarkerCreateHintWindowHandler(AMapView: TMapView;
      out AHintWindow: THintWindow);
    procedure MarkerHintHandler({%H-}AMapView: TMapView; APoint: TGPSPoint;
      var AHint: String);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  htmlUtils;

const
  SUPERSUBSCRIPT_RATIO = 0.8;

type
  TSimpleHTMLHintWindow = class(THintWindow)
  private
    FBitmap: TBitmap;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(ARect: TRect; const AHint: String); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: String; AData: pointer): TRect; override;
  end;

constructor TSimpleHtmlHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap.Create;
  Font.Assign(Screen.MenuFont);
  Font.Size := 12;
  Color := clInfoBk;
end;

destructor TSimpleHTMLHintWindow.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TSimpleHtmlHintWindow.ActivateHint(ARect: TRect; const AHint: String);
begin
  inherited ActivateHint(ARect, '');
  FBitmap.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
  FBitmap.Canvas.Brush.Color := Self.Color;
  FBitmap.Canvas.FillRect(0, 0, FBitmap.Width, FBitmap.Height);
  Fbitmap.Canvas.Font.Assign(Font);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  HTMLDrawText(FBitmap.Canvas, ARect, [odFocused], AHint, SUPERSUBSCRIPT_RATIO);
  Invalidate;
end;

function TSimpleHtmlHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: String; AData: pointer): TRect;
var
  lSize: TSize;
begin
  Result := inherited;
  Canvas.Font.Assign(Font);
  lSize := HTMLTextExtent(Canvas, Result, [], AHint, SUPERSUBSCRIPT_RATIO);
  Result.Bottom := Result.Top + lSize.CY;
  Result.Right := Result.Left + lSize.CX;
  InflateRect(Result, 4, 2);
end;

procedure TSimpleHtmlHintWindow.Paint;
begin
  Canvas.Draw(4, 2, FBitmap);
end;


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
      track.Points.Add(TGPSPoint.CreateFrom(APoints[i]));
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
  AddMapMarker(  13.377778,  52.5163890, 'Brandenburger Tor, Berlin');

  AddGPSTrack([RealPoint(-20,20), RealPoint(20, 0), RealPoint(-20,-20)]);
  AddMapTrack([RealPoint(20,20), RealPoint(-20,0), RealPoint(20,-20)]);

  Plugin := TMarkerHintPlugin.Create(PluginManager);
  Plugin.HintOffsetX := -1;
  Plugin.AutoHideHint := true;
  Plugin.OnCreateHintWindow := @MarkerCreateHintWindowHandler;

  for i := 0 to cgPointTypes.Items.Count-1 do
    cgPointTypes.Checked[i] := true;
end;

procedure TMainForm.rgHintTypeClick(Sender: TObject);
begin
  case rgHintType.ItemIndex of
    0: Plugin.OnHint := nil;
    1: Plugin.OnHint := @MarkerHintHandler;
  end;
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

procedure TMainForm.MarkerCreateHintWindowHandler(AMapView: TMapView;
  out AHintWindow: THintWindow);
begin
  case rgHintType.ItemIndex of
    0: AHintWindow := THintWindow.Create(AMapView);
    1: AHintWindow := TSimpleHTMLHintWindow.Create(AMapView);
  end;
end;

procedure TMainForm.MarkerHintHandler(AMapView: TMapView; APoint: TGPSPoint;
  var AHint: String);
var
  sName: String = '';
  sLatLon: String;
begin
  if rgHintType.ItemIndex = 1 then
  begin
    if APoint.Name <> '' then
      sName := Format(
        '<font color="blue" size="12"><b>%s</b></font><br>', [APoint.Name]);
    sLatLon := Format(
      '<font color="black" size="9">' +
        '<ind="10"><i>Latitude:</i> %s' +
        '<br>' +
        '<ind="10"><i>Longitude:</i> %s' +
      '</font>', [
      LatToStr(APoint.Lat, true),
      LonToStr(APoint.Lon, true)
    ]);
    AHint :=
      '<html>' +
        '<body>' +
        sName +
        sLatLon +
        '</body>' +
      '</html>';
  end;
end;

end.

