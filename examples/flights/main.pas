unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, Grids,
  mvMapViewer, mvTypes, mvDrawingEngine;

type
  { TMainForm }

  TMainForm = class(TForm)
    btnWarp: TSpeedButton;
    cbTightFollow: TCheckBox;
    ilPOIs: TImageList;
    ilJetSprites: TImageList;
    gbMouse: TGroupBox;
    gbCenter: TGroupBox;
    lblInfo: TLabel;
    lblCenterLatVal: TLabel;
    lblCenterLonVal: TLabel;
    lblCenterLat: TLabel;
    lblMouseLon: TLabel;
    lblMouseLat: TLabel;
    lblMouseLonVal: TLabel;
    lblMouseLatVal: TLabel;
    lblCenterLon: TLabel;
    LblZoom: TLabel;
    MapView: TMapView;
    PageControl: TPageControl;
    plInfo: TPanel;
    pnlWarpInfo: TPanel;
    pnlFlightControl: TPanel;
    PgData: TTabSheet;
    sgTracks: TStringGrid;
    btnPlay: TSpeedButton;
    btnStop: TSpeedButton;
    btnPause: TSpeedButton;
    tmrStep: TTimer;
    ZoomTrackBar: TTrackBar;
    procedure btnPauseClick(Sender: TObject);
    procedure btnWarpClick(Sender: TObject);
    procedure cbTightFollowChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MapViewMouseLeave(Sender: TObject);
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MapViewZoomChange(Sender: TObject);
    procedure sgTracksDblClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tmrStepTimer(Sender: TObject);
    procedure ZoomTrackBarChange(Sender: TObject);
  private
    procedure UpdateCoords(X, Y: Integer);
    procedure UpdateButtons;
    procedure UpdateWarp;
    procedure Fly;
    procedure Ground;
    procedure Follow;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, Math, FPImage, DateUtils, mvGeoMath, mvEngine;

type

  { TFlightTrack }

  TFlightTrack = class
  private
    FSprites: TImageList;
    FTrack: TMapTrack;
    FJet: TMapPointOfInterest;
    FIndex: Integer;
    FBearingIndex: Integer;
    FElapsed: Int64;
    FWarp: Integer;
    procedure MoveTrackPointToIndex(AIndex: Integer);
    procedure SetJet(AValue: TMapPointOfInterest);
    procedure JetDraw(Sender: TObject; ADrawer: TMvCustomDrawingEngine;
      APoint: TMapPointOfInterest);
  public
    procedure Step(AMillis: Int64);
    procedure Reset;
    function TimeInFlight: TTime;
    property Jet: TMapPointOfInterest read FJet write SetJet;
    property Track: TMapTrack read FTrack write FTrack;
    property Sprites: TImageList read FSprites write FSprites;
  end;

  // Layers with actual flight tracks
  TracksRange = 1..7;

var
  Tracks: array [TracksRange] of TFlightTrack;
  PrevTicks: Int64;

  TimeWARP: Integer = 200;  // Time multiplier
  DoFollow: Boolean = True; // False not to follow selected
  TightFollow: Boolean = True; // True to follow tight

function MillisElapsed(AReset: Boolean = True): Int64;
begin
  Result := GetTickCount64 - PrevTicks;
  if AReset then
    PrevTicks := GetTickCount64;
end;

{ TFlightTrack }

function TFlightTrack.TimeInFlight: TTime;
begin
  Result := Jet.DateTime - Track.Points[0].DateTime;
end;

procedure TFlightTrack.MoveTrackPointToIndex(AIndex: Integer);
var
  PtsCount: Integer;
  P0, P1: TMapPoint;
  Bearing: Double;
  CurDt: TDateTime;
  StepUp: Boolean = False;
  JetSecs, ToeSecs: Int64;
  Lat, Lon, ToeFrac: Double;
begin
  PtsCount := FTrack.Points.Count;
  if (AIndex < 0) or (AIndex >= PtsCount) then
  begin
    FJet.Visible := False;
    Exit;
  end;

  // Advance FIndex to the index of the most recent point of the track
  CurDt := IncMilliSecond(FTrack.Points[0].DateTime, FElapsed);
  while (AIndex < PtsCount) and (FTrack.Points[AIndex].DateTime <= CurDt) do
  begin
    StepUp := True; // Waypont changed
    FIndex := AIndex;
    Inc(AIndex);
  end;

  // Last waypoint, update jet coordinates when changed
  P0 := FTrack.Points[FIndex];
  if StepUp then
  begin
    FJet.Latitude := P0.Latitude;
    FJet.Longitude := P0.Longitude;
    FJet.Elevation := P0.Elevation;
  end;

  // Update jet time to current
  FJet.DateTime := CurDt;

  // Is there a next point?
  if FIndex < Pred(PtsCount) then
  begin
    P1 := FTrack.Points[Succ(FIndex)];

    // Is it different?
    if (P0.Latitude <> P1.Latitude) or (P0.Longitude <> P1.Longitude) then
    begin
      // Seconds to travel from the last waypoint to the next
      ToeSecs := SecondsBetween(P1.DateTime, P0.DateTime);
      if ToeSecs > 0 then
      begin
        JetSecs := SecondsBetween(P1.DateTime, FJet.DateTime);
        ToeFrac := EnsureRange(Double(ToeSecs - JetSecs) / ToeSecs, 0.0, 1.0);
        if InRange(ToeFrac, 0.001, 0.999) then
        begin
          // Calculate intermediate point based on time passed
          CalcIntermedPoint(P0.Latitude, P0.Longitude, P1.Latitude,
            P1.Longitude, ToeFrac, Lat, Lon);
          FJet.Latitude := Lat;
          FJet.Longitude := Lon;
        end;
      end;

      // Update jet bearing
      Bearing := CalcBearing(Jet.Latitude, Jet.Longitude, P1.Latitude, P1.Longitude);
      FBearingIndex := Round((Bearing - 7.5) / 15.0);
      FJet.ImageIndex := FBearingIndex;
    end;
  end;
  FJet.Visible := True;
end;

procedure TFlightTrack.JetDraw(Sender: TObject;
  ADrawer: TMvCustomDrawingEngine; APoint: TMapPointOfInterest);
var
  Sprite: TBitmap;
  P: TPoint;
begin
  if not Assigned(FSprites) then
    Exit;
  Sprite := Nil;
  try
    Sprite := TBitmap.Create;
    FSprites.GetBitmap(FBearingIndex, Sprite);
    for P in APoint.View.CyclicPointsOf(APoint.ToScreen) do
      ADrawer.DrawBitmap(P.X - FSprites.Width div 2, P.Y - FSprites.Height div 2,
        Sprite, True);
  finally
    Sprite.Free;
  end;
end;

procedure TFlightTrack.SetJet(AValue: TMapPointOfInterest);
begin
  FJet := AValue;
  FJet.OnDrawPoint := @JetDraw;
end;

procedure TFlightTrack.Step(AMillis: Int64);
begin
  Inc(FElapsed, AMillis * FWarp);
  MoveTrackPointToIndex(Succ(FIndex));
end;

procedure TFlightTrack.Reset;
begin
  FElapsed := 0;
  FIndex := 0;
  MoveTrackPointToIndex(FIndex);
end;

{ TMainForm }

procedure TMainForm.btnPlayClick(Sender: TObject);
begin
  MillisElapsed(True);
  tmrStep.Enabled := True;
  UpdateButtons;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  Ground;
  UpdateButtons;
  UpdateWarp;
end;

procedure TMainForm.tmrStepTimer(Sender: TObject);
begin
  Fly;
  Follow;
  UpdateButtons;
  UpdateWarp;
end;

procedure TMainForm.btnPauseClick(Sender: TObject);
begin
  tmrStep.Enabled := False;
  UpdateButtons;
end;

procedure TMainForm.btnWarpClick(Sender: TObject);
var
  T: TFlightTrack;
begin
  TimeWARP := 100 + TimeWARP mod 500;
  for T in Tracks do
  begin
    T.FWarp := TimeWARP;
    T.FIndex := 0; // to recalc position
  end;
  UpdateWarp;
end;

procedure TMainForm.cbTightFollowChange(Sender: TObject);
begin
  TightFollow := cbTightFollow.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  sgTracks.RowCount := Length(Tracks) + 1;
  for I := Low(Tracks) to High(Tracks) do
  begin
    Tracks[I] := TFlightTrack.Create;
    Tracks[I].Track := MapView.Layers[I].Tracks.Last;
    Tracks[I].Jet := MapView.Layers[I].PointsOfInterest.Last;
    Tracks[I].Sprites := ilJetSprites;
    Tracks[I].FWarp := TimeWARP;
    Tracks[I].Reset;
    sgTracks.Cells[0, I - Low(Tracks) + 1] := Tracks[I].Jet.Caption;
  end;

  ZoomTrackBar.Position := MapView.Zoom;
  UpdateButtons;
  UpdateWarp;

  lblMouseLonVal.Caption := '';
  lblMouseLatVal.Caption := '';
  lblCenterLonVal.Caption := '';
  lblCenterLatVal.Caption := '';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  T: TFlightTrack;
begin
  for T in Tracks do
    T.Free;
end;

procedure TMainForm.MapViewMouseLeave(Sender: TObject);
begin
  UpdateCoords(MaxInt, MaxInt);
end;

procedure TMainForm.MapViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  UpdateCoords(X, Y);
end;

procedure TMainForm.MapViewZoomChange(Sender: TObject);
begin
  ZoomTrackbar.Position := MapView.Zoom;
end;

procedure TMainForm.sgTracksDblClick(Sender: TObject);
var
  I: Integer;
begin
  I := Pred(sgTracks.Row) + Low(Tracks);
  if not InRange(I, Low(Tracks), High(Tracks)) then
    Exit;
  with Tracks[I].Jet do
  begin
    MapView.MapCenter.Latitude := Latitude;
    MapView.MapCenter.Longitude := Longitude;
    Visible := True;
  end;
end;

procedure TMainForm.UpdateCoords(X, Y: Integer);
var
  RP: TRealPoint;
  UseDMS: Boolean;
begin
  RP := MapView.Center;
  UseDMS := mvoLatLonInDMS in MapView.Options;

  lblCenterLonVal.Caption := LonToStr(RP.Lon, UseDMS);
  lblCenterLatVal.Caption := LatToStr(RP.Lat, UseDMS);

  if X <> MaxInt then
  begin
    RP := MapView.ScreenToLatLon(Point(X, Y));
    lblMouseLonVal.Caption := LonToStr(RP.Lon, UseDMS);
    lblMouseLatVal.Caption := LatToStr(RP.Lat, UseDMS);
  end
  else
  begin
    lblMouseLonVal.Caption := '-';
    lblMouseLatVal.Caption := '-';
  end;
end;

procedure TMainForm.UpdateButtons;
begin
  btnPlay.Enabled := not tmrStep.Enabled;
  btnPause.Enabled := tmrStep.Enabled;
  btnStop.Enabled := tmrStep.Enabled;
end;

procedure TMainForm.UpdateWarp;
begin
  pnlWarpInfo.Caption := Format('Elapsed: %s, Warp (x%d)',
    [FormatDateTime('hh:nn:ss', Tracks[Low(Tracks)].TimeInFlight), TimeWARP]);
end;

procedure TMainForm.ZoomTrackBarChange(Sender: TObject);
begin
  MapView.Zoom := ZoomTrackBar.Position;
  LblZoom.Caption := Format('Zoom (%d):', [ZoomTrackbar.Position]);
end;

procedure TMainForm.Fly;
var
  I, TI: Integer;
  Millis: Int64;
begin
  Millis := MillisElapsed;
  for I := Low(Tracks) to High(Tracks) do
    with Tracks[I] do
    begin
      Step(Millis);
      TI := I - Low(Tracks) + 1;
      sgTracks.Cells[1, TI] := LatToStr(Jet.Latitude, Jet.LatLonInDMS);
      sgTracks.Cells[2, TI] := LonToStr(Jet.Longitude, Jet.LatLonInDMS);
    end;
end;

procedure TMainForm.Ground;
var
  T: TFlightTrack;
begin
  tmrStep.Enabled := False;
  for T in Tracks do
    T.Reset;
end;

procedure TMainForm.Follow;
var
  I, W, H: Integer;
  R: TRect;
  Jet: TMapPointOfInterest;

  function Outside: Boolean;
  begin
    W := MapView.ClientWidth;
    H := MapView.ClientHeight;
    R.Create(0, 0, W, H).Inflate(-W div 4, -H div 4);
    Result := not R.Contains(Jet.ToScreen);
  end;

begin
  if MapView.Engine.InDrag or not DoFollow then
    Exit;
  I := Pred(sgTracks.Row) + Low(Tracks);
  if not InRange(I, Low(Tracks), High(Tracks)) then
    Exit;
  Jet := Tracks[I].Jet;
  if TightFollow or Outside then
  begin
    MapView.MapCenter.Latitude := Jet.Latitude;
    MapView.MapCenter.Longitude := Jet.Longitude;
  end;
end;

end.

