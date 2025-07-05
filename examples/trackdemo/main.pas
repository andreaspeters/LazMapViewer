unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLIntf, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, ColorBox,
  mvMapViewer, mvGpsObj, mvGeoMath;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    cbAutoTrace: TCheckBox;
    cbUseThreads: TCheckBox;
    clbTrackColor: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblInfo: TLabel;
    lblMapCenter: TLabel;
    lblMapData: TLabel;
    lblMapPresentation: TLabel;
    MapDataLink: TLabel;
    MapLicenseLink: TLabel;
    MapStyleLink: TLabel;
    MapView: TMapView;
    Panel1: TPanel;
    Panel2: TPanel;
    MapPanel: TPanel;
    Splitter1: TSplitter;
    TrackGrid: TStringGrid;
    Timer: TTimer;
    procedure cbAutoTraceChange(Sender: TObject);
    procedure cbUseThreadsChange(Sender: TObject);
    procedure clbTrackColorChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure URLClick(Sender: TObject);
    procedure URLMouseEnter(Sender: TObject);
    procedure URLMouseLeave(Sender: TObject);
    procedure MapViewCenterMove(Sender: TObject);
    procedure MapViewZoomChange(Sender: TObject);
    procedure TrackGridClick(Sender: TObject);
    procedure TrackGridPrepareCanvas(sender: TObject; {%H-}aCol, {%H-}aRow: Integer;
      {%H-}aState: TGridDrawState);
    procedure TimerTimer(Sender: TObject);
  private
    FTrack: TGpsTrack;
    FCurrPt: TGpsPoint;
    FCurrPtIndex: Integer;
    procedure InfoCaption;
    procedure LoadGPXSampleFile;
    procedure MoveTrackPointToIndex(ANewIndex: Integer);

  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  mvTypes, mvEngine, mvGpx;

{$R *.lfm}

const
  _TRACK_PT_ = 10;

function DMSToDeg(Deg, Min: Word; Sec: Double): Double;
begin
  Result := Deg + Min/60 + Sec/3600;
end;

{ TMainForm }

procedure TMainForm.FormActivate(Sender: TObject);
var
  crs: TCursor;
begin
  crs := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    MapView.MapProvider := 'Open Topo Map';
    MapView.Active := true;

    // Center on Grand Canyon Village
    MapView.Center := RealPoint(DMSToDeg(36, 4, 32.2), -DMSToDeg(112, 7, 17.4));
    MapView.Zoom := 13;

    clbTrackColor.Selected := MapView.DefaultTrackColor;

    // Load a GPX file of a hiking trail into the Grand Canyon
    LoadGPXSampleFile;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.cbAutoTraceChange(Sender: TObject);
begin
  if cbAutoTrace.Checked then
  begin
    if FCurrPtIndex = FTrack.Points.Count-1 then
      FCurrPtIndex := 0;
    Timer.Enabled := true;
    lblInfo.Enabled := false;
    TrackGrid.Enabled := false;
  end else begin
    Timer.Enabled := false;
    TrackGrid.Enabled := true;
    lblInfo.Enabled := true;
    TrackGrid.Row := FCurrPtIndex + 1;
  end;
end;

procedure TMainForm.cbUseThreadsChange(Sender: TObject);
begin
  MapView.UseThreads := cbUseThreads.Checked;
end;

procedure TMainForm.clbTrackColorChange(Sender: TObject);
begin
  MapView.DefaultTrackColor := clbTrackColor.Selected;
end;

procedure TMainForm.InfoCaption;
begin
  lblMapCenter.Caption := Format('MapCenter: Lat %s, Lon %s, Zoom %d', [
    mvGeoMath.LatToStr(MapView.Center.Lat, true),
    mvGeoMath.LonToStr(MapView.Center.Lon, true),
    MapView.Zoom
  ]);
end;

procedure TMainForm.LoadGPXSampleFile;
const
  GPX_FILE_NAME = '../../grand_canyon_trail.gpx';
  // file 972150.gpx from https://www.summitpost.org/grand-canyon-in-a-day/972150
var
  reader: TGpxReader;
  b: TRealArea;
  i: Integer;
begin
  reader := TGpxReader.Create;
  try
    reader.LoadFromFile(Application.Location + GPX_FILE_NAME, MapView.GPSItems, b);
    MapView.Engine.ZoomOnArea(b);
    FTrack := MapView.GpsItems.GetObjectsInArea(b).Items[0] as TGpsTrack;

    FCurrPtIndex := 0;
    FCurrPt := TGpsPoint.Create(0, 0);
    FCurrPt.Assign(FTrack.Points[FCurrPtIndex]);
    FCurrPt.Name := Format('Time: %s'+LineEnding+'Elevation: %.0fm', [
      TimeToStr(FCurrPt.DateTime), FCurrPt.Elevation
    ]);
    MapView.GPSItems.Add(FCurrPt, _TRACK_PT_);

    // Show track points in grid
    TrackGrid.RowCount := FTrack.Points.Count + 1;
    for i := 0 to FTrack.Points.Count-1 do
    begin
      TrackGrid.Cells[1, i+1] := LatToStr(FTrack.Points[i].Lat, true);
      TrackGrid.Cells[2, i+1] := LonToStr(FTrack.Points[i].Lon, true);
      TrackGrid.Cells[3, i+1] := FormatFloat('0', FTrack.Points[i].Elevation)+'m';
      TrackGrid.Cells[4, i+1] := FormatDateTime('hh:nn:ss', FTrack.points[i].DateTime);
    end;
  finally
    reader.Free;
  end;
end;

procedure TMainForm.MapViewCenterMove(Sender: TObject);
begin
  InfoCaption;
end;

procedure TMainForm.MapViewZoomChange(Sender: TObject);
begin
  InfoCaption;
end;

procedure TMainForm.MoveTrackPointToIndex(ANewIndex: Integer);
begin
  if (ANewIndex < 0) or (ANewIndex >= FTrack.Points.Count) then
    exit;
  FCurrPtIndex := ANewIndex;
  FCurrPt.Assign(FTrack.Points[FCurrPtIndex]);
  FCurrPt.Name := Format('Time: %s'+LineEnding+'Elevation: %.0fm', [
    TimeToStr(FCurrPt.DateTime), FCurrPt.Elevation
  ]);
  MapView.Invalidate;
end;

procedure TMainForm.TrackGridClick(Sender: TObject);
begin
  MoveTrackPointToIndex(TrackGrid.Row-1);
end;

procedure TMainForm.TrackGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
  grid: TStringGrid;
begin
  grid := Sender as TStringGrid;
  ts := grid.Canvas.TextStyle;
  ts.Alignment := taCenter;
  grid.Canvas.TextStyle := ts;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  MoveTrackPointToIndex(FCurrPtIndex + 1);
  if FCurrPtIndex >= FTrack.Points.Count-1 then
  begin
    Timer.Enabled := false;
    cbAutoTrace.Checked := false;
  end;
end;

procedure TMainForm.URLClick(Sender: TObject);
begin
  if (Sender is TLabel) and (TLabel(Sender).Hint <> '') then
    OpenURL(TLabel(Sender).Hint);
end;

procedure TMainForm.URLMouseEnter(Sender: TObject);
begin
  if Sender is TLabel then
    TLabel(Sender).Font.Style := [fsUnderline];
end;

procedure TMainForm.URLMouseLeave(Sender: TObject);
begin
  if Sender is TLabel then
    TLabel(Sender).Font.Style := [];
end;

end.

