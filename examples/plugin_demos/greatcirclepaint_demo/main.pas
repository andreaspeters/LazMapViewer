unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Menus, Spin,
  mvMapViewer, mvTypes, mvGPSObj, mvPluginCommon,
  mvGeoMath, mvPlugins, mvGreatCirclePainterPlugin;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnPresetPolar: TButton;
    btnPreseEquator: TButton;
    btnPresetPorto: TButton;
    btnPresetLongestSeaWay: TButton;
    btnLongestEarthWay: TButton;
    cbCyclicMap: TCheckBox;
    cgOptions: TCheckGroup;
    clbOrthodromePenColor: TColorButton;
    clbGreatCirclePenColor: TColorButton;
    cbZOrder: TComboBox;
    gbPresets: TGroupBox;
    gbOrthodromePen: TGroupBox;
    GroupBox3: TGroupBox;
    ImageList: TImageList;
    lblInfo: TLabel;
    lblZOrder: TLabel;
    Label2: TLabel;
    lblOrthodromePenWidth: TLabel;
    lblGreatCirclePenWidth: TLabel;
    MapView: TMapView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MvPluginManager: TMvPluginManager;
    MvPluginManagerDraggableMarkerPlugin1: TDraggableMarkerPlugin;
    ParamsPanel: TPanel;
    MapPanel: TPanel;
    PolarPopupMenu: TPopupMenu;
    seOrthodromePenWidth: TSpinEdit;
    seGreatCirclePenWidth: TSpinEdit;
    StatusBar: TStatusBar;
    tbSegmentLength: TTrackBar;
    procedure btnPresetPolarClick(Sender: TObject);
    procedure btnPreseEquatorClick(Sender: TObject);
    procedure btnPresetPortoClick(Sender: TObject);
    procedure btnPresetLongestSeaWayClick(Sender: TObject);
    procedure btnLongestEarthWayClick(Sender: TObject);
    procedure cbCyclicMapChange(Sender: TObject);
    procedure cgOptionsItemClick(Sender: TObject; Index: integer);
    procedure clbOrthodromePenColorColorChanged(Sender: TObject);
    procedure clbGreatCirclePenColorColorChanged(Sender: TObject);
    procedure cbZOrderChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PoleMenuItemClick(Sender: TObject);
    procedure MvPluginManagerDraggableMarkerPlugin1DraggableMarkerMovedEvent(
      Sender: TDraggableMarkerPlugin; AMarker: TGPSPoint;
      AOrgPosition: TRealPoint);
    procedure seOrthodromePenWidthChange(Sender: TObject);
    procedure seGreatCirclePenWidthChange(Sender: TObject);
    procedure tbSegmentLengthChange(Sender: TObject);
  private
    FActivated: Boolean;
    FGreatCirclePainterPlugin : TGreatCirclePainterPlugin;
    FStartMarker : TGpsPointOfInterest;
    FDestinationMarker : TGpsPointOfInterest;
    procedure OnGreatCirclePainterGetCoords(Sender : TGreatCirclePainterPlugin;
                                            var FStart, FDestination : TRealPoint);
    procedure SetMarkerPositions(const AStartLat, AStartLon, ADestLat, ADestLon : Double);
    procedure OnGreatCirclePainterPluginChange(Sender : TObject);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);

  function AddTraditionalMarker(const ALat, ALon: Double;
    ACaption: String; const AImgIndex : Integer = -1) : TGpsPointOfInterest;
  var
    gpsPt: TGpsPointOfInterest;
  begin
    Result := Nil;
    gpsPt := TGpsPointOfInterest.Create(ALon,ALat);
    try
      gpsPt.Name := ACaption;
      gpsPt.ImageIndex := AImgIndex;
      MapView.GPSItems.Add(gpsPt, 100);
      Result := gpsPt;
      gpsPt := Nil;
    finally
      if Assigned(gpsPt) then
        gpsPt.Free;
    end;
  end;

begin
  MapView.Active := true;

  FGreatCirclePainterPlugin := TGreatCirclePainterPlugin.Create(MvPluginManager);
  MvPluginManager.PluginList.Add(FGreatCirclePainterPlugin);
  FGreatCirclePainterPlugin.ZOrder := gcpzCanvas;
  FGreatCirclePainterPlugin.OnGetStartAndDestinationCoords:=@OnGreatCirclePainterGetCoords;
  FGreatCirclePainterPlugin.OnChange := @OnGreatCirclePainterPluginChange;
  FGreatCirclePainterPlugin.MapView := MapView;
  FGreatCirclePainterPlugin.GreatCirclePen.Color:= clbGreatCirclePenColor.ButtonColor;
  FGreatCirclePainterPlugin.GreatCirclePen.Style:= psSolid;
  FGreatCirclePainterPlugin.GreatCirclePen.Width:= seGreatCirclePenWidth.Value;
  FGreatCirclePainterPlugin.OrthodromePen.Color:= clbOrthodromePenColor.ButtonColor;
  FGreatCirclePainterPlugin.OrthodromePen.Style:= psSolid;
  FGreatCirclePainterPlugin.OrthodromePen.Width:= seOrthodromePenWidth.Value;
  FStartMarker := AddTraditionalMarker(41.1578,-8.6333, 'Start',0);
  FDestinationMarker := AddTraditionalMarker(10.6722,-61.5333, 'Destination',1);
end;

procedure TMainForm.SetMarkerPositions(const AStartLat, AStartLon, ADestLat, ADestLon : Double);
begin
  FStartMarker.MoveTo(AStartLon, AStartLat);
  FDestinationMarker.MoveTo(ADestLon, ADestLat);
  MapView.Invalidate;
end;

procedure TMainForm.OnGreatCirclePainterPluginChange(Sender: TObject);
var
  s : String;
begin
  s := Format('%1.3f',[FGreatCirclePainterPlugin.OrthodromeDistance / 1000.0]);
  StatusBar.Panels[1].Text := s;
  s := Format('%1.3f',[(EARTH_CIRCUMFERENCE-FGreatCirclePainterPlugin.OrthodromeDistance) / 1000.0]);
  StatusBar.Panels[3].Text := s;
  s := Format('%1.6f:%1.6f',[FGreatCirclePainterPlugin.StartLat,FGreatCirclePainterPlugin.StartLon]);
  StatusBar.Panels[5].Text := s;
  s := Format('%1.6f:%1.6f',[FGreatCirclePainterPlugin.DestinationLat,FGreatCirclePainterPlugin.DestinationLon]);
  StatusBar.Panels[7].Text := s;
  s := Format('%1.2f°',[FGreatCirclePainterPlugin.InitialBearing]);
  StatusBar.Panels[9].Text := s;
  StatusBar.Panels[11].Text := IntToStr(FGreatCirclePainterPlugin.GreatCirclePointsCount);
end;


procedure TMainForm.PoleMenuItemClick(Sender: TObject);

begin
  if Sender is TMenuItem then
  begin
    case TMenuItem(Sender).Tag of
      1 : SetMarkerPositions(42.496909,-7.026229,-42.496909,172.973771); // Antipodes Spain / New Zealand
      2 : SetMarkerPositions(90.0,20.0,-90.0,-90.0); // Both Poles, Start North
      3 : SetMarkerPositions(-90.0,40.0,90.0,-100.0); // Both Poles, Start South
      4 : SetMarkerPositions(90.0,60.0,48.8582300,2.2945500); // Start North, Dest free
      5 : SetMarkerPositions(-90.0,80.0,22.2708100,114.1497900); // Start South, Dest free
      6 : SetMarkerPositions(-33.8707000,151.2082800,90.0,-110.0); // Dest North, Start free
      7 : SetMarkerPositions(43.6439500,-79.3884000,-90.0,-120.0); // Dest South, Start free
    end;
  end;
end;

procedure TMainForm.cbCyclicMapChange(Sender: TObject);
begin
  MapView.Cyclic := cbCyclicMap.Checked;
  MapView.Invalidate;
end;

procedure TMainForm.cgOptionsItemClick(Sender: TObject; Index: integer);
var
  opt : TGreatCirclePainterOptions = [];
begin
  if cgOptions.Checked[0] then
    Include(opt,gcpoMarkStart);
  if cgOptions.Checked[1] then
    Include(opt,gcpoMarkCenter);
  if cgOptions.Checked[2] then
    Include(opt,gcpoMarkDestination);
  FGreatCirclePainterPlugin.Options := opt;
end;

procedure TMainForm.clbOrthodromePenColorColorChanged(Sender: TObject);
begin
  FGreatCirclePainterPlugin.OrthodromePen.Color := clbOrthodromePenColor.ButtonColor;
end;

procedure TMainForm.clbGreatCirclePenColorColorChanged(Sender: TObject);
begin
  FGreatCirclePainterPlugin.GreatCirclePen.Color := clbGreatCirclePenColor.ButtonColor;
end;

procedure TMainForm.btnPresetPolarClick(Sender: TObject);
begin
  PolarPopupMenu.PopUp;
end;

procedure TMainForm.btnPreseEquatorClick(Sender: TObject);
begin
  FStartMarker.MoveTo(0.0,0.0);
  FDestinationMarker.MoveTo(100.0,0.0);
  MapView.Invalidate;
end;

procedure TMainForm.btnPresetPortoClick(Sender: TObject);
begin
  FStartMarker.MoveTo(-8.6333,41.1578);
  FDestinationMarker.MoveTo(-61.5333,10.6722);
  MapView.Invalidate;
end;

procedure TMainForm.btnPresetLongestSeaWayClick(Sender: TObject);
begin
  FStartMarker.MoveTo(DMSToDeg(66,40,0),DMSToDeg(25,17,0));
  FDestinationMarker.MoveTo(DMSToDeg(162,14,0),DMSToDeg(58,37,0));
  MapView.Invalidate;
end;

procedure TMainForm.btnLongestEarthWayClick(Sender: TObject);
begin
//  24◦33′ N, 118◦38′ E
// 37◦2′ N, 8◦55′ W
  FStartMarker.MoveTo(DMSToDeg(118,38,0),DMSToDeg(23,33,0));
  FDestinationMarker.MoveTo(-DMSToDeg(8,55,0),DMSToDeg(37,2,0));
  MapView.Invalidate;
end;

procedure TMainForm.cbZOrderChange(Sender: TObject);
begin
  case cbZOrder.ItemIndex of
    0 : FGreatCirclePainterPlugin.ZOrder := gcpzCanvas;
    1 : FGreatCirclePainterPlugin.ZOrder := gcpzInFrontOfMarkers;
    2 : FGreatCirclePainterPlugin.ZOrder := gcpzBehindMarkers;
  end;
  MapView.Invalidate;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    Constraints.MinHeight := gbPresets.Top + gbPresets.Height +
      ParamsPanel.BorderSpacing.Around * 2 + Statusbar.Height;
    Constraints.MinWidth := ParamsPanel.Width + ParamsPanel.BorderSpacing.Around * 2;
    if Height < Constraints.MinHeight then
      Height := 0;
    FActivated := true;
  end;
end;

procedure TMainForm.MvPluginManagerDraggableMarkerPlugin1DraggableMarkerMovedEvent
  (Sender: TDraggableMarkerPlugin; AMarker: TGPSPoint; AOrgPosition: TRealPoint
  );
begin
  FGreatCirclePainterPlugin.SetStartAndDestination(
    FStartMarker.RealPoint, FDestinationMarker.RealPoint
  );
end;

procedure TMainForm.seOrthodromePenWidthChange(Sender: TObject);
begin
  FGreatCirclePainterPlugin.OrthodromePen.Width := seOrthodromePenWidth.Value;
end;

procedure TMainForm.seGreatCirclePenWidthChange(Sender: TObject);
begin
  FGreatCirclePainterPlugin.GreatCirclePen.Width := seGreatCirclePenWidth.Value;
end;

procedure TMainForm.tbSegmentLengthChange(Sender: TObject);
const
  SegmentLengths : array[0..10] of Integer = (
     1,2,5,10,20,30,50,70,100,150,200
  );
begin
  FGreatCirclePainterPlugin.SegmentLength := SegmentLengths[tbSegmentLength.Position];
  MapView.Invalidate;
end;

procedure TMainForm.OnGreatCirclePainterGetCoords(
  Sender: TGreatCirclePainterPlugin; var FStart, FDestination: TRealPoint);
begin
  FStart := FStartMarker.RealPoint;
  FDestination := FDestinationMarker.RealPoint;
end;

end.

