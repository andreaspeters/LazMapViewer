{-------------------------------------------------------------------------------
                              mvMarkerPlugins.pas

License: modified LGPL with linking exception (like RTL, FCL and LCL)

See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
for details about the license.

See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
--------------------------------------------------------------------------------

This unit collects some typical marker-related plugins:

- TMarkerHintPlugin
    displays a hint when the mouse hovers over a GPSPoint or a MapPoint.
    Handle the OnHint event to define the exact string to be displayed in the
    popup hint window.

- TMarkerClickPlugin
    Fires the OnMarkerClick event when the user clicks on a GPSPoint or a
    MapPoint. Which mouse key and modification shift key (CTRL, SHIFT, ALT)
    triggers the event, is defined by the Shift property.

- TMarkerEditorPlugin
    Allows to add new GPSPoints or MapPoints, to select them by clicking and
    to drag them to another location. There are also methods to convert a group
    of selected markers to a GPSTrack/MapTrack or GPSArea/MapArea, or to
    delete the points in this selection.

- TDraggableMarkerPlugin
    Allows to drag the marker hit by pressing the mouse button to a new location.
    Unlike the TMarkerEditorPlugin data are handled such that multiple maps
    can be processed by the same plugin.
-------------------------------------------------------------------------------}

unit mvMarkerPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, Controls, LCLIntf, Forms, Dialogs,
  mvStrConsts, mvTypes, mvGeoMath,
  mvMapViewer, mvDrawingEngine, mvPluginCommon, mvGPSObj;

type
  { TMarkerHintPlugin }

  { Event allowing to create a different hint window class for custom drawing
    of the hint. }
  TMarkerCreateHintWindowEvent = procedure(AMapView: TMapView;
    out AHintWindow: THintWindow) of object;

  { Event to define the hint text for the marker at the given point.
    Return an empty string when no hint should be displayed. }
  TMarkerHintEvent = procedure (AMapView: TMapView; APoint: TGPSPoint;
    var AHint: String) of object;

  TMarkerHintPlugin = class(TMvMarkerPlugin)
  private
    const
      DEFAULT_HINT_OFFSET_X = 0;
      DEFAULT_HINT_OFFSET_Y = 15;
      DEFAULT_HIDE_INTERVAL = 1000;
  private
    FAutoHideHint: Boolean;
    FHideInterval: Integer;
    FHintOffsetX: Integer;
    FHintOffsetY: Integer;
    FHintWindow: THintWindow;
    FShowHint: Boolean;
    FOnCreateHintWindow: TMarkerCreateHintWindowEvent;
    FOnHint: TMarkerHintEvent;
  protected
    function CreateHintWindow(AMapView: TMapView): THintWindow; virtual;
    procedure DisplayHint(AMapView: TMapView; APoint: TGPSPoint; X, Y: Integer); virtual;
    procedure HideHint; virtual;
  protected
    procedure MouseMove(AMapView: TMapView; {%H-}AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoHideHint: Boolean read FAutoHideHint write FAutoHideHint default false;
    property HideInterval: Integer read FHideInterval write FHideInterval default 0;
    property HintOffsetX: Integer read FHintOffsetX write FHintOffsetX default DEFAULT_HINT_OFFSET_X;
    property HintOffsetY: Integer read FHintOffsetY write FHintOffsetY default DEFAULT_HINT_OFFSET_Y;
    property ShowHint: Boolean read FShowHint write FShowHint default true;
    property OnCreateHintWindow: TMarkerCreateHintWindowEvent read FOnCreateHintWindow write FOnCreateHintWindow;
    property OnHint: TMarkerHintEvent read FOnHint write FOnHint;
  end;


  { TCustomMarkerClickPlugin }

  TMarkerCanClickEvent = procedure (AMapView: TMapView; APoint: TGPSPoint; var CanClick: Boolean) of object;
  TMarkerClickEvent = procedure (AMapView: TMapView; APoint: TGPSPoint) of object;

  TCustomMarkerClickPlugin = class(TMvMarkerPlugin)
  private
    FCursor: TCursor;
    FOnCanClick: TMarkerCanClickEvent;
    FOnMarkerClick: TMarkerClickEvent;
    FShift: TShiftState;
  protected
    FMouseDownOnMarker: Boolean;
    FMousePoint: TPoint;
    FOrigGpsPoint: TGPSPoint;
    FSavedCursor: TCursor;
    function CanClick(AMapView: TMapView; APoint: TGPSPoint): Boolean; virtual;
    procedure MouseDown(AMapView: TMapView; {%H-}Button: TMouseButton;
      AShift: TShiftState; X,Y: Integer; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; {%H-}AShift: TShiftState;
      X,Y: Integer; var Handled: Boolean); override;
    procedure MouseUp({%H-}AMapView: TMapView; {%H-}Button: TMouseButton;
      {%H-}AShift: TShiftState; {%H-}X,{%H-}Y: Integer; var {%H-}Handled: Boolean); override;
    procedure SetMapView(AValue: TMapView); override;
    property Shift: TShiftState read FShift write FShift default [ssLeft];
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Cursor: TCursor read FCursor write FCursor default crHandPoint;
    property OnCanClick: TMarkerCanClickEvent read FOnCanClick write FOnCanClick;
    property OnMarkerClick: TMarkerClickEvent read FOnMarkerClick write FOnMarkerClick;
  end;


  { TMarkerClickPlugin }

  TMarkerClickPlugin = class(TCustomMarkerClickPlugin)
  published
    property Shift;
  end;

  { TMarkerEditorPlugin }

  TMarkerDrawPointEvent = procedure (AMapView: TMapView;
    ADrawingEngine: TMvCustomDrawingEngine; AGPSPoint: TGPSPoint;
    AScreenPoint: TPoint; AMarkerSize: Integer) of object;

  TMarkerNewPointEvent = procedure (AMapView: TMapView; APoint: TGPSPoint) of object;

  TMarkerStartDragEvent = procedure (AMapView: TMapView; var CanDrag: Boolean) of object;

  TMarkerClickMode = (cmNone, cmNewPoint, cmSelectPoint, cmSelectShape, cmRubberband);

  TMarkerNewPointType = (nptGPSPoint, nptMapPoint);

  TMarkerOption = (moCanAddPoint, moCanSelectPoint, moCanDragPoint, moUnselectPrompt, moMultiSelect);
  TMarkerOptions = set of TMarkerOption;

  TMarkerEditorPlugin = class(TMarkerClickPlugin)
  private
    type
      TPluginStateEnum = (psDraggingMap, psDraggingPoint, psRubberBandMode, psExtendingSelection, psPendingUnselect);
      TPluginState = set of TPluginStateEnum;
    const
      DEFAULT_CLICKMODE = cmSelectPoint;
      DEFAULT_OPTIONS = [moCanDragPoint, moCanAddPoint, moCanSelectPoint, moUnselectPrompt];
      DEFAULT_RUBBERBAND_BORDERCOLOR = clGray;
      DEFAULT_RUBBERBAND_FILLCOLOR = clWhite;
      DEFAULT_RUBBERBAND_OPACITY = 0.55;
      DEFAULT_SHIFT_FOR_NEW_POINT = [ssRight];
      DEFAULT_SHIFT_TO_SELECT_POINT = [ssLeft];
      DEFAULT_SHIFT_TO_SELECT_SHAPE = [ssLeft, ssShift];
      DEFAULT_SHIFT_TO_SELECT_BY_RUBBERBAND = [ssLeft, ssAlt];
      DEFAULT_SHIFT_TO_EXTEND_SELECTION = [ssCtrl];  // to be used in addition to the others
  private
    FMouseDown: Boolean;
    FClickMode: TMarkerClickMode;
    FDragCursor: TCursor;
    FNewPointType: TMarkerNewPointType;
    FOptions: TMarkerOptions;
    FRubberbandBorderColor: TColor;
    FRubberbandFillColor: TColor;
    FRubberbandOpacity: Single;
    FRubberbandStartPt: TPoint;
    FRubberbandEndPt: TPoint;
    FSelection: TGPSPointList;
    FShiftForNewPoint: TShiftState;
    FShiftToSelectPoint: TShiftState;
    FShiftToSelectShape: TShiftState;
    FShiftToSelectByRubberband: TShiftState;
    FShiftToExtendSelection: TShiftState;
    FState: TPluginState;
    FOrigSelection: array of TRealPoint;  // Selection before dragging starts
    FOnDrawPoint: TMarkerDrawPointEvent;
    FOnNewPoint: TMarkerNewPointEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnStartDrag: TMarkerStartDragEvent;
    FOnEndDrag: TNotifyEvent;
    function IsOpacityStored: Boolean;
    procedure SetExtendSelection(AValue: Boolean);
    procedure SetOptions(AValue: TMarkerOptions);
  protected
    procedure AddToSelection(AMapView: TMapView; APoint: TGPSPoint; AExtendSelection: Boolean);
    function CanClick(AMapView: TMapView; APoint: TGPSPoint): Boolean; override;
    procedure DeleteFromList(AMapView: TMapView; APoint: TGPSPoint);
    procedure DoSelectionChange(AMapView: TMapView);
    procedure DragStart(AMapView: TMapView);
    procedure DragTo(AMapView: TMapView; X, Y: Integer);
    procedure DragEnd(AMapView: TMapView);
    procedure DrawPoint(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      AGpsPoint: TGPSPoint; AScreenPoint: TPoint; AMarkerSize: Integer);
    procedure DrawRubberband(AMapView: TMapView);
    procedure DrawSelection(AMapView: TMapView);
    procedure FindContainerOfPoint(AMapView: TMapView; APoint: TGPSPoint; var AContainer: TGPSObj; var AIndex: Integer);
    procedure FindMapCollection(AMapView: TMapView; APoint: TGPSPoint; var ACollection: TMapCollectionBase; var AIndex: Integer);
    function IsShiftOfClickMode(AShift: TShiftState; AClickMode: TMarkerClickMode): Boolean;
    function IsShiftSet(AShift: TShiftState): Boolean;
    function RubberbandRect: TRect;
    procedure RubberbandStart(AMapView: TMapView; X, Y: Integer);
    procedure RubberbandTo(AMapView: TMapView; X, Y: Integer);
    procedure RubberbandEnd(AMapView: TMapView; X, Y: Integer);
    procedure UnselectPoint(AMapView: TMapView; APoint: TGPSPoint);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var {%H-}Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; {%H-}Button: TMouseButton;
      AShift: TShiftState; X,Y: Integer; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; {%H-}AShift: TShiftState;
      X,Y: Integer; var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; {%H-}Button: TMouseButton;
      AShift: TShiftState; X,Y: Integer; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ConvertSelectedPointsToGPSArea(AMapView: TMapView; AreaID: Integer): TGPSArea;
    function ConvertSelectedPointsToMapArea(AMapView: TMapView; ALayer: TMapLayer): TMapArea;
    function ConvertSelectedPointsToGPSTrack(AMapView: TMapView; ATrackID: Integer): TGPSTrack;
    function ConvertSelectedPointsToMapTrack(AMapView: TMapView; ALayer: TMapLayer): TMapTrack;
    procedure DeleteSelectedPoints(AMapView: TMapView);
    procedure MoveSelectionBy(AMapView: TMapView; dx, dy: Double);
    procedure MoveSelectionBy(AMapView: TMapView; dx, dy: Integer);
    function NewPoint(AMapView: TMapView; X, Y: Integer): TGPSPoint;
    procedure SelectAllPointsOfShape(AMapView: TMapView; APoint: TGPSPoint; AExtendSelection: Boolean);
    procedure SelectInRubberband(AMapView: TMapView);
    property Selection: TGPSPointList read FSelection;
  published
//    property ClickMode: TMarkerClickMode read FClickMode write FClickMode default DEFAULT_CLICKMODE;
    property DragCursor: TCursor read FDragCursor write FDragCursor default crSizeAll;
//    property ExtendSelection: Boolean read FExtendSelection write SetExtendSelection default false;
    property NewPointType: TMarkerNewPointType read FNewPointType write FNewPointType default nptGPSPoint;
    property Options: TMarkerOptions read FOptions write SetOptions default DEFAULT_OPTIONS;
    property RubberbandBorderColor: TColor read FRubberbandBorderColor write FRubberbandBorderColor default DEFAULT_RUBBERBAND_BORDERCOLOR;
    property RubberbandFillColor: TColor read FRubberbandFillColor write FRubberbandFillColor default DEFAULT_RUBBERBAND_FILLCOLOR;
    property RubberbandOpacity: Single read FRubberbandOpacity write FRubberbandOpacity stored IsOpacityStored;
    property ShiftForNewPoint: TShiftState read FShiftForNewPoint write FShiftForNewPoint
      default DEFAULT_SHIFT_FOR_NEW_POINT;
    property ShiftToExtendSelection: TShiftState read FShiftToExtendSelection write FShiftToExtendSelection
      default DEFAULT_SHIFT_TO_EXTEND_SELECTION;
    property ShiftToSelectPoint: TShiftState read FShiftToSelectPoint write FShiftToSelectPoint
      default DEFAULT_SHIFT_TO_SELECT_POINT;
    property ShiftToSelectShape: TShiftState read FShiftToSelectShape write FShiftToSelectShape
      default DEFAULT_SHIFT_TO_SELECT_SHAPE;
    property ShiftToSelectByRubberband: TShiftState read FShiftToSelectByRubberband write FShiftToSelectByRubberband
      default DEFAULT_SHIFT_TO_SELECT_BY_RUBBERBAND;
    property OnDrawPoint: TMarkerDrawPointEvent read FOnDrawPoint write FOnDrawPoint;
    property OnEndDrag: TNotifyEvent read FOnEndDrag write FOnEndDrag;
    property OnNewPoint: TMarkerNewPointEvent read FOnNewPoint write FOnNewPoint;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnStartDrag: TMarkerStartDragEvent read FOnStartDrag write FOnStartDrag;
  end;


  { TDraggableMarkerPlugin }

  TDraggableMarkerPlugin = class;
  TDraggableMarkerCanMoveEvent = function (Sender : TDraggableMarkerPlugin; AMarker : TGPSPoint) : Boolean of object;
  TDraggableMarkerMovedEvent = procedure (Sender : TDraggableMarkerPlugin; AMarker : TGPSPoint; AOrgPosition : TRealPoint) of object;

  { TDraggableMarkerData }
  PDraggableMarkerData = ^TDraggableMarkerData;
  TDraggableMarkerData = record
    FDraggedMarker : TGPSPoint;
    FOrgPosition : TRealPoint;
  end;

  TDraggableMarkerPlugin = class(TMvMultiMapsPlugin)
  private
    const
      DEFAULT_TOLERANCE = 5;
  private
    FDraggableMarkerCanMoveEvent : TDraggableMarkerCanMoveEvent;
    FDraggableMarkerMovedEvent : TDraggableMarkerMovedEvent;
    FDragMouseButton: TMouseButton;
    FTolerance: Integer;
    function GetFirstMarkerAtMousePos(const AMapView: TMapView; const AX, AY : Integer) : TGPSPoint;
    function GetDraggedMarker(AMapView : TMapView) : TGPSPoint;
    function GetOrgPosition(AMapView : TMapView): TRealPoint;
  protected
    procedure MouseDown(AMapView: TMapView; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; {%H-}AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property DraggedMarker[AMapView : TMapView] : TGPSPoint read GetDraggedMarker;
    property OrgPosition[AMapView : TMapView] : TRealPoint read GetOrgPosition;
  published
    property DraggableMarkerCanMoveEvent : TDraggableMarkerCanMoveEvent read FDraggableMarkerCanMoveEvent write FDraggableMarkerCanMoveEvent;
    property DraggableMarkerMovedEvent : TDraggableMarkerMovedEvent read FDraggableMarkerMovedEvent write FDraggableMarkerMovedEvent;
    property DragMouseButton : TMouseButton read FDragMouseButton write FDragMouseButton default mbLeft;
    property Tolerance: Integer read FTolerance write FTolerance default DEFAULT_TOLERANCE;
  end;


implementation

uses
  Types;

function IfThen(AValue: Boolean; ACursor1, ACursor2: TCursor): TCursor;
begin
  if AValue then Result := ACursor1 else Result := ACursor2;
end;


{ TMarkerHintPlugin }

constructor TMarkerHintPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FHintOffsetX := DEFAULT_HINT_OFFSET_X;
  FHintOffsetY := DEFAULT_HINT_OFFSET_Y;
  FHideInterval := DEFAULT_HIDE_INTERVAL;
  FShowHint := true;
end;

function TMarkerHintPlugin.CreateHintWindow(AMapView: TMapView): THintWindow;
begin
  if Assigned(FOnCreateHintWindow) then
    FOnCreateHintWindow(AMapView, Result)
  else
    Result := THintWindow.Create(self);
end;

procedure TMarkerHintPlugin.DisplayHint(AMapView: TMapView; APoint: TGPSPoint;
  X, Y: Integer);
var
  hintTxt: String;
  hintRct: TRect;
  hintPt: TPoint;
  dx, dy: Integer;
begin
  if APoint.Name <> '' then
    hintTxt := Format('%s' + LineEnding + '(%s / %s)', [
      APoint.Name, LatToStr(APoint.Lat, true), LonToStr(APoint.Lon, true)
    ])
  else
    hintTxt := Format('(%s / %s)', [LatToStr(APoint.Lat, true), LonToStr(APoint.Lon, true)]);

  if Assigned(FOnHint) then
    FOnHint(AMapView, APoint, hintTxt);

  if (hintTxt = '') or not FShowHint then
    exit;

  if not Assigned(FHintWindow) then
    FHintWindow := CreateHintWindow(AMapView);
  FHintWindow.AutoHide := FAutoHideHint;
  FHintWindow.HideInterval := FHideInterval;

  hintRct := FHintWindow.CalcHintRect(AMapView.Width, hintTxt, nil);
  hintPt := AMapView.ClientToScreen(Point(X, Y));
  if FHintOffsetX = -1 then
    dx := - hintRct.Width div 2
  else
    dx := FHintOffsetX;
  if FHintOffsetY = -1 then
    dy := - hintRct.Height div 2
  else
    dy := FHintOffsetY;
  OffsetRect(hintRct, hintPt.X + dx, hintPt.Y + dy);
  FHintWindow.ActivateHint(hintRct, hintTxt);
end;

procedure TMarkerHintPlugin.HideHint;
begin
  FreeAndNil(FHintWindow);
end;

procedure TMarkerHintPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X,Y: Integer; var Handled: Boolean);
var
  gpsPoint: TGPSPoint;
begin
  if Handled then
    exit;

  gpsPoint := FindNearestMarker(AMapView, X, Y);
  if gpsPoint = nil then
    HideHint
  else
    DisplayHint(AMapView, gpsPoint, X, Y);
end;


{ TCustomMarkerClickPlugin }

constructor TCustomMarkerClickPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FCursor := crHandPoint;
  FSavedCursor := crDefault;
  FShift := [ssLeft];
end;

function TCustomMarkerClickPlugin.CanClick(AMapView: TMapView;
  APoint: TGPSPoint): Boolean;
begin
  Result := Assigned(APoint);
  if Result and Assigned(FOnCanClick) then
    FOnCanClick(AMapView, APoint, Result);
end;

procedure TCustomMarkerClickPlugin.MouseDown(AMapView: TMapView;
  Button: TMouseButton; AShift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Handled then
    exit;

  FOrigGPSPoint := FindNearestMarker(AMapView, X, Y);
  if Assigned(FOrigGPSPoint) and (AShift = FShift) then
  begin
    if not CanClick(AMapView, FOrigGPSPoint) then
      exit;
//    if Assigned(FOnMarkerClick) then
//      FOnMarkerClick(AMapView, FOrigGPSPoint);
    FMouseDownOnMarker := true;
    FMousePoint := Point(X, Y);
    Handled := true;
  end;
end;

procedure TCustomMarkerClickPlugin.MouseMove(AMapView: TMapView;
  {%H-}AShift: TShiftState; X,Y: Integer; var Handled: Boolean);
var
  gpsPoint: TGPSPoint;
begin
  if Handled then
    exit;
  if not FMouseDownOnMarker then
  begin
    gpsPoint := FindNearestMarker(AMapView, X, Y);
    AMapView.Cursor := IfThen(CanClick(AMapView, gpsPoint), FCursor, FSavedCursor);
  end;
end;

procedure TCustomMarkerClickPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  AShift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if FMouseDownOnMarker then
  begin
    if Assigned(FOnMarkerClick) then
      FOnMarkerClick(AMapView, FOrigGPSPoint);
    FMouseDownOnMarker := false;
  end;
end;

{ Store the original MapView cursor. Is used when the mouse is not over a
  clickable point. If no MapView is assigned to the plugin it is assumed that
  the MapView has the default cursor. }
procedure TCustomMarkerClickPlugin.SetMapView(AValue: TMapView);
begin
  inherited;
  if Assigned(MapView) then
    FSavedCursor := MapView.Cursor
  else
    FSavedCursor := crDefault;
end;


{ TMarkerEditorPlugin }

type
  TMarkerData = record
    Lat, Lon: Double;
    Elevation: Double;
    DateTime: TDateTime;
  end;

function GPSPointToMarkerData(P: TGPSPoint): TMarkerData;
begin
  Result.Lat := P.Lat;
  Result.Lon := P.Lon;
  Result.Elevation := P.Elevation;
  Result.DateTime := P.DateTime;
end;

procedure MarkerDataToGPSPoint(M: TMarkerData; P: TGPSPoint);
begin
  P.Lat := M.Lat;
  P.Lon := M.Lon;
  P.Elevation := M.Elevation;
  P.DateTime := M.DateTime;
end;

constructor TMarkerEditorPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FClickMode := DEFAULT_CLICKMODE;
  FDragCursor := crSizeAll;
  FOptions := DEFAULT_OPTIONS;
  FRubberbandBorderColor := DEFAULT_RUBBERBAND_BORDERCOLOR;
  FRubberbandFillColor := DEFAULT_RUBBERBAND_FILLCOLOR;
  FRubberbandOpacity := DEFAULT_RUBBERBAND_OPACITY;
  FSelection := TGPSPointList.Create(false);  // false = do not free objects
  FShiftForNewPoint := DEFAULT_SHIFT_FOR_NEW_POINT;
  FShiftToSelectPoint := DEFAULT_SHIFT_TO_SELECT_POINT;
  FShiftToSelectShape := DEFAULT_SHIFT_TO_SELECT_SHAPE;
  FShiftToSelectByRubberband := DEFAULT_SHIFT_TO_SELECT_BY_RUBBERBAND;
  FShiftToExtendSelection := [ssCtrl];  // modifier to the others to add point(s) to current selection
end;

destructor TMarkerEditorPlugin.Destroy;
begin
  FSelection.Free;
  inherited;
end;

procedure TMarkerEditorPlugin.AddToSelection(AMapView: TMapView;
  APoint: TGPSPoint; AExtendSelection: Boolean);
var
  idx: Integer;
begin
  if not (moCanSelectPoint in FOptions) then
    exit;

  if AExtendSelection and (moMultiSelect in FOptions) then
  begin
    idx := FSelection.IndexOf(APoint);
    if idx > -1 then
      FSelection.Move(idx, FSelection.Count-1)
    else
      FSelection.Add(APoint);
  end else
  begin
    FSelection.Clear;
    FSelection.Add(APoint);
  end;
  DoSelectionChange(AMapView);
end;

procedure TMarkerEditorPlugin.AfterDrawObjects(AMapView: TMapView;
  var {%H-}Handled: Boolean);
begin
  inherited;
  DrawSelection(AMapView);
  if (psRubberBandMode in FState) then
    DrawRubberband(AMapView);
end;

function TMarkerEditorPlugin.CanClick(AMapView: TMapView;
  APoint: TGPSPoint): Boolean;
begin
  Result := inherited and (moCanSelectPoint in FOptions);
end;

function TMarkerEditorPlugin.ConvertSelectedPointsToGPSArea(
  AMapView: TMapView; AreaID: Integer): TGPSArea;
var
  M: TMarkerData;
begin
  if FSelection.Count < 3 then
    raise EMvPluginException.Create(mvRS_AreaSelectionNeedsAtLeast3Points);

  Result := TGPSArea.Create;
  while FSelection.Count > 0 do
  begin
    M := GPSPointToMarkerData(FSelection[0]);
    DeleteFromList(AMapView, FSelection[0]);
    FSelection.Delete(0);
    Result.Points.Add(TGPSPoint.Create(M.Lon, M.Lat, M.Elevation, M.DateTime));
  end;
  AMapView.GPSItems.Add(Result, AreaID);

  Update;
  DoSelectionChange(AMapView);
end;

function TMarkerEditorPlugin.ConvertSelectedPointsToMapArea(
  AMapView: TMapView; ALayer: TMapLayer): TMapArea;
var
  M: TMarkerData;
  P: TMapPoint;
begin
  if FSelection.Count < 3 then
    raise EMvPluginException.Create(mvRS_AreaSelectionNeedsAtLeast3Points);

  Result := ALayer.Areas.Add as TMapArea;
  while FSelection.Count > 0 do
  begin
    M := GPSPointToMarkerData(FSelection[0]);
    DeleteFromList(AMapView, FSelection[0]);
    FSelection.Delete(0);
    P := Result.Points.Add as TMapPoint;
    MarkerDataToGPSPoint(M, TGPSPoint(P.GPSObj));
  end;
  Update;
  DoSelectionChange(AMapView);
end;

function TMarkerEditorPlugin.ConvertSelectedPointsToGPSTrack(
  AMapView: TMapView; ATrackID: Integer): TGPSTrack;
var
  M: TMarkerData;
begin
  if FSelection.Count < 2 then
    raise EMvPluginException.Create(mvRS_TrackSelectionNeedsAtLeast2Points);

  Result := TGPSTrack.Create;
  while FSelection.Count > 0 do
  begin
    M := GPSPointToMarkerData(FSelection[0]);
    DeleteFromList(AMapView, FSelection[0]);
    FSelection.Delete(0);
    Result.Points.Add(TGPSPoint.Create(M.Lon, M.Lat, M.Elevation, M.DateTime));
  end;
  AMapView.GPSItems.Add(Result, ATrackID);

  Update;
  DoSelectionChange(AMapView);
end;

function TMarkerEditorPlugin.ConvertSelectedPointsToMapTrack(
  AMapView: TMapView; ALayer: TMapLayer): TMapTrack;
var
  M: TMarkerData;
  P: TMapPoint;
begin
  if FSelection.Count < 2 then
    raise EMvPluginException.Create(mvRS_TrackSelectionNeedsAtLeast2Points);

  Result := ALayer.Tracks.Add as TMapTrack;
  while FSelection.Count > 0 do
  begin
    M := GPSPointToMarkerData(FSelection[0]);
    DeleteFromList(AMapView, FSelection[0]);
    FSelection.Delete(0);
    P := Result.Points.Add as TMapPoint;
    MarkerDataToGPSPoint(M, TGPSPoint(P.GPSObj));
  end;
  Update;
  DoSelectionChange(AMapView);
end;

{ Searches for the given point in all the point lists of the mapviewer
  (GPSItems, Layers, Tracks, Areas, POIs). If found, the point is removed
  from the list and destroyed. }
procedure TMarkerEditorPlugin.DeleteFromList(AMapView: TMapView;
  APoint: TGPSPoint);
var
  gpsObj: TGPSObj = nil;
  collection: TMapCollectionBase = nil;
  idx: Integer = -1;
begin
  // Find the point in the GPS lists
  FindContainerOfPoint(AMapView, APoint, gpsObj, idx);
  if (gpsObj is TGPSObjectList) then
  begin
    TGPSObjectList(gpsObj).Delete(APoint);
    exit;
  end else
  if (gpsObj is TGPSPolyLine) then
  begin
    TGPSPolyLine(gpsObj).Points.Delete(idx);
    exit;
  end;

  // Find the point in the map collections
  FindMapCollection(AMapView, APoint, collection, idx);
  if collection <> nil then
    collection.Delete(idx);
end;

procedure TMarkerEditorPlugin.DeleteSelectedPoints(AMapView: TMapView);
var
  i: Integer;
begin
  for i := FSelection.Count-1 downto 0 do
  begin
    DeleteFromList(AMapView, FSelection[i]);
    FSelection.Delete(i);
  end;
  Update;
end;

procedure TMarkerEditorPlugin.DoSelectionChange(AMapView: TMapView);
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(AMapView);
end;

procedure TMarkerEditorPlugin.DragStart(AMapView: TMapView);
var
  i: Integer;
  canDrag: Boolean;
begin
  if not (moCanDragPoint in FOptions) then
    exit;
  if Assigned(FOnStartDrag) then
  begin
    canDrag := true;
    FOnStartDrag(AMapView, canDrag);
    if not canDrag then exit;
  end;
  FState := FState + [psDraggingPoint] - [psPendingUnselect];
  AMapView.Cursor := DragCursor;
  // Save original selection point coordinates in case they must be restored later.
  SetLength(FOrigSelection, FSelection.Count);
  for i := 0 to High(FOrigSelection) do
    FOrigSelection[i] := FSelection[i].RealPoint;
end;

procedure TMarkerEditorPlugin.DragTo(AMapView: TMapView; X, Y: Integer);
var
  dX, dY: Integer;
begin
  if (psDraggingPoint in FState) then
  begin
    dX := X - FMousePoint.X;
    dY := Y - FMousePoint.Y;
    MoveSelectionBy(AMapView, dX, dY);
    Update;
    FMousePoint := Point(X, Y);
  end;
end;

procedure TMarkerEditorPlugin.DragEnd(AMapView: TMapView);
begin
  Exclude(FState, psDraggingPoint);
  AMapView.Cursor := FSavedCursor;
  if Assigned(FOnEndDrag) then
    FOnEndDrag(AMapView);
end;

{ Draw the selection marker for the given point. The drawing engine already
  has been setup for the correct settings. }
procedure TMarkerEditorPlugin.DrawPoint(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; AGpsPoint: TGPSPoint;
  AScreenPoint: TPoint; AMarkerSize: Integer);
begin
  if Assigned(FOnDrawPoint) then
    FOnDrawPoint(AMapView, ADrawingEngine, AGPSPoint, AScreenPoint, AMarkerSize)
  else
    ADrawingEngine.Rectangle(
      AScreenPoint.X - AMarkerSize,
      AScreenPoint.Y - AMarkerSize,
      AScreenPoint.X + AMarkerSize,
      AScreenPoint.Y + AMarkerSize
    );
end;

procedure TMarkerEditorPlugin.DrawRubberband(AMapView: TMapView);
var
  DE: TMvCustomDrawingEngine;
  R: TRect;
begin
  DE := AMapView.DrawingEngine;
  if FRubberbandFillColor = clNone then
    DE.BrushStyle := bsClear
  else
  begin
    DE.BrushColor := FRubberbandFillColor;
    DE.BrushStyle := bsSolid;
  end;
  if FRubberbandBorderColor = clNone then
    DE.PenStyle := psClear
  else
  begin
    DE.PenColor := FRubberbandBorderColor;
    DE.PenStyle := psSolid;
    DE.PenWidth := 1;
  end;
  DE.Opacity := FRubberbandOpacity;
  R := RubberbandRect;
  DE.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
end;

procedure TMarkerEditorPlugin.DrawSelection(AMapView: TMapView);
const
  MARKER_SIZE = 5;
var
  i, j: Integer;
  P: TPoint;
  markerSize: Integer;
  DE: TMvCustomDrawingEngine;
  pts: TPointArray;
begin
  if FSelection.Count = 0 then
    exit;

  DE := AMapView.DrawingEngine;
  DE.PenColor := clRed;
  DE.PenStyle := psSolid;
  DE.PenWidth := 2;
  DE.BrushColor := clBlack;
  DE.BrushStyle := bsSolid;
  markerSize := AMapView.Scale96ToFont(MARKER_SIZE);
  for i := 0 to FSelection.Count - 1 do
  begin
    if i = FSelection.Count - 1 then
    begin
      // The last point is marked as being "focused"
      DE.PenWidth := 3;
      DE.BrushColor := clLime;
      inc(markerSize, 1);
    end;
    P := AMapView.LatLonToScreen(FSelection[i].RealPoint);
    pts := AMapView.CyclicPointsOf(P);
    for j := 0 to High(pts) do
      DrawPoint(AMapView, DE, FSelection[i], pts[j], markerSize);
  end;
end;

function IsSamePoint(AItem: TGPSObj; APoint: TGPSPoint): Boolean;
begin
  Result := (AItem is TGPSPoint) and TGPSPoint(AItem).RealPoint.Equal(APoint.RealPoint);
end;

{ Tries to find the point in one of the gps-type lists.
  If found, returns the found object and the index of the point in the list.
  Otherwise, nil and -1 are returned, respectively. }
procedure TMarkerEditorPlugin.FindContainerOfPoint(AMapView: TMapView;
  APoint: TGPSPoint; var AContainer: TGPSObj; var AIndex: Integer);
var
  i, j, k: Integer;
  gpsLayer: TGPSObjectList;
  gpsPolyline: TGPSPolyLine;
  item: TGPSObj;
begin
  // Iterate over the 10 layers of GPSItems
  for i := 0 to 9 do
  begin
    gpsLayer := AMapView.GPSLayer[i];
    for j := 0 to gpsLayer.Count-1 do
    begin
      item := gpsLayer[j];
      if IsSamePoint(item, APoint) then
      begin
        AContainer := gpsLayer;
        AIndex := j;
        exit;
      end;

      if (item is TGPSPolyline) then
      begin
        gpsPolyLine := TGPSPolyLine(item);
        for k := 0 to gpsPolyLine.Points.Count-1 do
        begin
          item := gpsPolyLine.Points[k];
          if IsSamePoint(item, APoint) then
          begin
            AContainer := gpsPolyLine;
            AIndex := k;
            exit;
          end;
        end;
      end;
    end;
  end;

  // If we get here the point has not been found.
  AContainer := nil;
  AIndex := -1;
end;

{ Tries to find the point in one of the map-type collections of all layers.
  If found, returns the collection and the index of the point in the collection.
  Otherwise, nil and -1 are returned, respectively. }
procedure TMarkerEditorPlugin.FindMapCollection(AMapView: TMapView;
  APoint: TGPSPoint; var ACollection: TMapCollectionBase; var AIndex: Integer);
var
  i, j, k: Integer;
  p: TMapPoint;
  mapLayer: TMapLayer;
  mapTrack: TMapTrack;
  mapArea: TMapArea;
begin
  // Iterate over all map-layers
  for i := 0 to AMapView.Layers.Count-1 do
  begin
    mapLayer := AMapView.Layers[i];
    // Points of interest?
    for j := 0 to mapLayer.PointsOfInterest.Count-1 do
    begin
      p := mapLayer.PointsOfInterest[j];
      if IsSamePoint(p.GPSObj, APoint) then
      begin
        ACollection := mapLayer.PointsOfInterest;
        AIndex := j;
        exit;
      end;
    end;
    // Tracks?
    for j := 0 to mapLayer.Tracks.Count-1 do
    begin
      mapTrack := mapLayer.Tracks[j];
      for k := 0 to mapTrack.Points.Count-1 do
      begin
        p := mapTrack.Points[k];
        if IsSamePoint(p.GPSObj, APoint) then
        begin
          ACollection := mapTrack.Points;
          AIndex := k;
          exit;
        end;
      end;
    end;
    // Areas?
    for j := 0 to mapLayer.Areas.Count-1 do
    begin
      mapArea := mapLayer.Areas[j];
      for k := 0 to mapArea.Points.Count-1 do
      begin
        p := mapArea.Points[k];
        if IsSamePoint(p.GPSObj, APoint) then
        begin
          ACollection := mapArea.Points;
          AIndex := k;
          exit;
        end;
      end;
    end;
  end;
  // When we get here, the point has not been found.
  ACollection := nil;
  AIndex := -1;
end;

function TMarkerEditorPlugin.IsOpacityStored: Boolean;
begin
  Result := FRubberbandOpacity <> DEFAULT_RUBBERBAND_OPACITY;
end;

function TMarkerEditorPlugin.IsShiftOfClickMode(AShift: TShiftState;
  AClickMode: TMarkerClickMode): Boolean;
begin
  if not IsShiftSet(AShift) then Exit;

  case AClickMode of
    cmNewPoint:
      Result := AShift = FShiftForNewPoint;
    cmSelectPoint:
      Result := (AShift * FShiftToSelectPoint) = FShiftToSelectPoint;
    cmSelectShape:
      Result := AShift = FShiftToSelectShape;
    cmRubberband:
      Result := AShift = FShiftToSelectByRubberband;
  end;
end;

function TMarkerEditorPlugin.IsShiftSet(AShift: TShiftState): Boolean;
begin
  Result := AShift <> [];
end;

{ Moves the selection by the given pixel count in x and y direction. }
procedure TMarkerEditorPlugin.MoveSelectionBy(AMapView: TMapView; dx, dy: Integer);
var
  i: Integer;
  P: TPoint;
  rPt: TRealPoint;
begin
  for i := 0 to FSelection.Count-1 do
  begin
    P := AMapView.LatLonToScreen(FSelection[i].RealPoint);
    P.X := P.X + dx;
    P.Y := P.Y + dy;
    rPt := AMapView.ScreenToLatLon(P);
    FSelection[i].MoveTo(rPt.Lon, rPt.Lat);
  end;
end;

{ Moves the selection by the given amount of degrees in x and y direction }
procedure TMarkerEditorPlugin.MoveSelectionBy(AMapView: TMapView; dx, dy: Double);
var
  i: Integer;
  P: TPoint;
  rPt: TRealPoint;
begin
  for i := 0 to FSelection.Count-1 do
  begin
    rPt := FSelection[i].RealPoint;
    rPt.Lon := FSelection[i].Lon + dX;
    rPt.Lat := FSelection[i].Lat + dY;
    P := AMapView.LatLonToScreen(rPt);
    rPt := AMapView.ScreenToLatLon(P);
    FSelection[i].MoveTo(rPt.Lon, rPt.Lat);
  end;
end;

procedure TMarkerEditorPlugin.MouseDown(AMapView: TMapView;
  {%H-}Button: TMouseButton; {%H-}AShift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
var
  canAddPoint: Boolean;
  canDragPoint: Boolean;
  canSelectPoint: Boolean;
begin
  FMouseDown := True;

  canAddPoint := moCanAddPoint in FOptions;
  canDragPoint := moCanDragPoint in FOptions;
  canSelectPoint := moCanSelectPoint in FOptions;

  if canAddPoint and IsShiftOfClickMode(AShift, cmNewPoint) then
  begin
    FClickMode := cmNewPoint;
    Shift := FShiftForNewPoint;
  end else
  if canSelectPoint and IsShiftOfClickMode(AShift, cmRubberband) then
  begin
    FClickMode := cmRubberband;
    Shift := FShiftToSelectPoint;
  end else
  if canSelectPoint and IsShiftOfClickMode(AShift, cmSelectShape) then
  begin
    FClickMode := cmSelectShape;
    Shift := FShiftToSelectShape;
  end else
  if canSelectPoint and IsShiftOfClickMode(AShift, cmSelectPoint) then
  begin
    FClickMode := cmSelectPoint;
    Shift := FShiftToSelectPoint;
  end else
    exit;

  if IsShiftSet(FShiftToExtendSelection) and (AShift * FShiftToExtendSelection = FShiftToExtendSelection) then
    Include(FState, psExtendingSelection)
  else
    Exclude(FState, psExtendingSelection);
  if (psExtendingSelection in FState) then
    Shift := Shift + FShiftToExtendSelection;

  inherited;

  if FMouseDownOnMarker then
  begin
    if not (psExtendingSelection in FState) and (FClickMode <> cmSelectPoint) then
      FSelection.Clear;

    case FClickMode of
      cmSelectPoint:
        if FSelection.IndexOf(FOrigGPSPoint) = -1 then
        begin
          AddToSelection(AMapView, FOrigGPSPoint, psExtendingSelection in FState);
          Exclude(FState, psPendingUnselect);
        end else
        if (psExtendingSelection in FState) then
          Include(FState, psPendingUnselect);
      cmSelectShape:
        SelectAllPointsOfShape(AMapView, FOrigGPSPoint, psExtendingSelection in FState);
    end;
    Update;
    Handled := true;
  end
  else if (FClickMode <> cmRubberband) and (psRubberbandMode in FState) then
    RubberbandEnd(AMapView, X, Y);
end;

procedure TMarkerEditorPlugin.MouseMove(AMapView: TMapView;
  {%H-}AShift: TShiftState; X,Y: Integer; var Handled: Boolean);
const
  SENSITIVITY = 5;
var
  R: TRect;
begin
  inherited;
  if FMouseDownOnMarker then
  begin
    if not (psDraggingPoint in FState) then
    begin
      // The mouse must be moved by more than SENSITIVITY pixels for dragging to
      // start
      R := Rect(X - SENSITIVITY, Y - SENSITIVITY, X + SENSITIVITY, Y + SENSITIVITY);
      if not PtInRect(R, Point(X, Y)) then
        exit;
      DragStart(AMapView);
    end;
    DragTo(AMapView, X, Y);
    Handled := true;
  end else
  if not (psDraggingPoint in FState) and (FClickMode = cmRubberband) and IsShiftOfClickMode(AShift, cmRubberband) then
  begin
    if not (psRubberbandMode in FState) then
      RubberbandStart(AMapView, X, Y)
    else
      RubberbandTo(AMapView, X, Y);
    Handled := true;
  end
  else if FMouseDown then
    Include(FState, psDraggingMap);
end;

procedure TMarkerEditorPlugin.MouseUp(AMapView: TMapView;
  {%H-}Button: TMouseButton; {%H-}AShift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
var
  canUnselect: Boolean;
begin
  if FMouseDownOnMarker then
  begin
    if (psDraggingPoint in FState) then
      DragEnd(AMapView);
    if (psPendingUnselect in FState) then
      UnselectPoint(AMapView, FOrigGPSPoint);
  end
  else
  begin
    case FClickMode of
      cmNewPoint:
        begin
          FOrigGPSPoint := NewPoint(AMapView, X, Y);
          AddToSelection(AMapView, FOrigGPSPoint, psExtendingSelection in FState);
          Handled := true;
        end;
      cmRubberband:
        if (psRubberbandMode in FState) then
          RubberbandEnd(AMapView, X, Y);
      else
        if not (psDraggingMap in FState) and (FSelection.Count > 0) then
        begin
          canUnSelect := not (moUnselectPrompt in FOptions) or
            (MessageDlg(mvRS_DoYouReallyWantToUnselectThePoints, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
          if canUnselect then
          begin
            FSelection.Clear;
            DoSelectionChange(AMapView);
          end;
          Handled := true;
        end;
    end;
    Update;
  end;

  Exclude(FState, psDraggingMap);
  FClickMode := cmNone; // For stopping rubberband in MouseMove
  FMouseDown := False;

  inherited;
end;

function TMarkerEditorPlugin.NewPoint(AMapView: TMapView;
  X, Y: Integer): TGPSPoint;
const
  ID = 4000;
var
  gpsPoint: TGPSPoint;
  mapPoint: TMapPoint;
  layer: TMapLayer;
  rPt: TRealPoint;
begin
  rPt := AMapView.ScreenToLatLon(Point(X, Y));
  case FNewPointType of
    nptGPSPoint:
      begin
        gpsPoint := TGPSPointOfInterest.CreateFrom(rPt);
        AMapView.GPSItems.Add(gpsPoint, ID);
        Result := gpsPoint;
      end;
    nptMapPoint:
      begin
        if AMapView.Layers.Count = 0 then
          layer := AMapView.Layers.Add as TMapLayer
        else
          layer := AMapView.Layers[0];
        mapPoint := layer.PointsOfInterest.Add as TMapPoint;
        mapPoint.RealPoint := rPt;
        Result := mapPoint.GPSObj as TGPSPoint;
      end;
  end;
  if Assigned(FOnNewPoint) then
    FOnNewPoint(AMapView, Result);
end;

procedure TMarkerEditorPlugin.RubberbandEnd(AMapView: TMapView; X, Y: Integer);
begin
  Exclude(FState, psRubberbandMode);
  FRubberbandEndPt := Point(X, Y);
  SelectInRubberband(AMapview);
  Update;
end;

function TMarkerEditorPlugin.RubberbandRect: TRect;
begin
  Result.TopLeft := FRubberBandStartPt;
  Result.BottomRight := FRubberbandEndPt;
  Result.NormalizeRect;
end;

procedure TMarkerEditorPlugin.RubberbandStart(AMapView: TMapView; X, Y: Integer);
begin
  if not (moCanSelectPoint in FOptions) then
    exit;

  Include(FState, psRubberbandMode);
  FRubberbandStartPt := Point(X, Y);
  FRubberbandEndPt := Point(X, Y);
end;

procedure TMarkerEditorPlugin.RubberbandTo(AMapView: TMapView; X, Y: Integer);
begin
  FRubberbandEndPt := Point(X, Y);
  Update;
end;

procedure TMarkerEditorPlugin.SelectAllPointsOfShape(AMapView: TMapView;
  APoint: TGPSPoint; AExtendSelection: Boolean);
var
  obj: TGPSObj = nil;
  collection: TMapCollectionBase = nil;
  idx: Integer = -1;
  i: Integer;
  gpsPolyline: TGPSPolyLine;
  item: TGPSObj;
  p: TMapPoint;

  procedure Finished;
  begin
    AddToSelection(AMapView, APoint, true);  // Mark APoint as being focused
    Update;
    DoSelectionChange(AMapView);
  end;

begin
  if not AExtendSelection then
    FSelection.Clear;

  // Find point in gpsObj-type of containers
  FindContainerOfPoint(AMapView, APoint, obj, idx);
  // Is is a point of interest?
  if obj is TGPSObjectList then
  begin
    item := TGPSObjectList(obj).Items[idx];
    AddToSelection(AMapView, TGPSPoint(item), true);
    Finished;
    exit;
  end else
  // ... or a track / layer?
  if obj is TGPSPolyLine then
  begin
    gpsPolyLine := TGPSPolyLine(obj);
    for i := 0 to gpsPolyLine.Points.Count-1 do
    begin
      item := TGPSPoint(gpsPolyLine.Points[i]);
      AddToSelection(AMapView, TGPSPoint(item), true);
    end;
    Finished;
    exit;
  end;

  // Find point in map-type collections
  FindMapCollection(AMapView, APoint, collection, idx);
  if collection is TMapPointsOfInterest then
  begin
    p := collection.Items[idx] as TMapPoint;
    AddToSelection(AMapView, TGPSPoint(p.GPSObj), true);
  end else
  if collection <> nil then
  begin
    for i := 0 to collection.Count-1 do
    begin
      p := collection.Items[i] as TMapPoint;
      AddToSelection(AMapView, TGPSPoint(p.GPSObj), true);
    end;
  end;
  Finished;
end;

procedure TMarkerEditorPlugin.SelectInRubberband(AMapView: TMapView);
var
  area: TRealArea;
  R: TRect;
  pts: TGPSObjArray;
  i: Integer;
begin
  if not (psExtendingSelection in FState) then
    FSelection.Clear;
  R := RubberbandRect;
  area.TopLeft := AMapView.ScreenToLatLon(R.TopLeft);
  area.BottomRight := AMapView.ScreenToLatLon(R.BottomRight);
  pts := AMapView.VisiblePointsInArea(area, PointTypes);
  for i := 0 to High(pts) do
    AddToSelection(AMapView, TGPSPoint(pts[i]), true);
  Update;
end;

procedure TMarkerEditorPlugin.SetExtendSelection(AValue: Boolean);
begin
  if AValue = (psExtendingSelection in FState) then exit;
  if AValue then
    Include(FState, psExtendingSelection)
  else
    Exclude(FState, psExtendingSelection);
  if not (psExtendingSelection in FState) then
  begin
    FSelection.Clear;
    if (FOrigGPSPoint <> nil) then
      FSelection.Add(FOrigGPSPoint);
  end;
  Update;
end;

procedure TMarkerEditorPlugin.SetOptions(AValue: TMarkerOptions);
begin
  if AValue= FOptions then exit;
  FOptions := AValue;
  if not (moCanSelectPoint in FOptions) then
  begin
    FSelection.Clear;
    Update;
  end;
end;

procedure TMarkerEditorPlugin.UnselectPoint(AMapView: TMapView;
  APoint: TGPSPoint);
var
  idx: Integer;
begin
  idx := FSelection.IndexOf(APoint);
  if idx > -1 then
  begin
    FSelection.Delete(idx);
    DoSelectionChange(AMapView);
  end;
end;


{ TDraggableMarkerPlugin }

constructor TDraggableMarkerPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FTolerance := DEFAULT_TOLERANCE;
end;

procedure TDraggableMarkerPlugin.Assign(Source: TPersistent);
begin
  if Source is TDraggableMarkerPlugin then
  begin
    FDraggableMarkerCanMoveEvent := TDraggableMarkerPlugin(Source).DraggableMarkerCanMoveEvent;
    FDraggableMarkerMovedEvent := TDraggableMarkerPlugin(Source).DraggableMarkerMovedEvent;
    FDragMouseButton := TDraggableMarkerPlugin(Source).DragMouseButton;
    FTolerance := TDraggableMarkerPlugin(Source).Tolerance;
  end;
  inherited;
end;

function TDraggableMarkerPlugin.GetFirstMarkerAtMousePos(const AMapView: TMapView;
  const AX, AY: Integer): TGPSPoint;

  function FindInList(AGpsList: TGpsObjList): TGpsPoint;
  var
    i: Integer;
  begin
    if Assigned(AGpsList) then
      for i := AGpsList.Count-1 downto 0 do
      begin
        if (AGpsList[i] is TGpsPoint) then
        begin
          Result := TGpsPoint(AGpsList[i]);
          if (not Assigned(FDraggableMarkerCanMoveEvent)) or
             DraggableMarkerCanMoveEvent(Self, Result)
          then
            exit;
        end;
      end;
    Result := nil;
  end;

var
  aArea : TRealArea;
  gpsList: TGpsObjList;
  layer: TMapLayer;
  i : Integer;
begin
  Result := Nil;
  aArea.TopLeft := AMapView.ScreenToLatLon(Point(AX - FTolerance, AY - FTolerance));
  aArea.BottomRight := AMapView.ScreenToLatLon(Point(AX + FTolerance, AY + FTolerance));

  // Search in GPSItems for all gps-type-of-points
  gpsList := AMapView.GPSItems.GetObjectsInArea(aArea);
  try
    Result := FindInList(gpsList);
    if Result <> nil then
      exit;
  finally
    gpsList.Free;
  end;

  // Search in all layers for all map-type points
  for i := AMapView.Layers.Count-1 downto 0 do
  begin
    layer := AMapView.Layers[i];
    gpsList := layer.GetObjectsInArea(aArea);
    try
      Result := FindInList(gpsList);
      if Result <> nil then
        exit;
    finally
      gpsList.Free;
    end;
  end;
end;

function TDraggableMarkerPlugin.GetDraggedMarker(AMapView: TMapView): TGPSPoint;
var
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  Result := Nil;
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) then
    Result := lDraggableMarkerData.FDraggedMarker;
end;

function TDraggableMarkerPlugin.GetOrgPosition(AMapView : TMapView): TRealPoint;
var
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  Result.InitXY(0.0,0.0);
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) then
    Result := lDraggableMarkerData.FOrgPosition;
end;

procedure TDraggableMarkerPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  lDraggableMarkerData : TDraggableMarkerData;
begin
  if Handled then Exit;
  if not MapViewEnabled[AMapView] then Exit;
  if FDragMouseButton <> Button then Exit;
  lDraggableMarkerData.FDraggedMarker := GetFirstMarkerAtMousePos(AMapView,X,Y);
  if Assigned(lDraggableMarkerData.FDraggedMarker) then
  begin
    lDraggableMarkerData.FOrgPosition.Lon:= lDraggableMarkerData.FDraggedMarker.Lon;
    lDraggableMarkerData.FOrgPosition.Lat:= lDraggableMarkerData.FDraggedMarker.Lat;
    SetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
    Handled := True;
  end;
end;

procedure TDraggableMarkerPlugin.MouseMove(AMapView: TMapView;
  AShift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  pt : TPoint;
  rpt : TRealPoint;
  ele : Double;
  dt : TDateTime;
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if not MapViewEnabled[AMapView] then Exit;
  if (cnt >= SizeOf(lDraggableMarkerData)) and
     Assigned(lDraggableMarkerData.FDraggedMarker) then
  begin
    pt.X := X;
    pt.Y := Y;
    rpt := AMapView.ScreenToLatLon(pt);
    ele := lDraggableMarkerData.FDraggedMarker.Elevation;
    dt := lDraggableMarkerData.FDraggedMarker.DateTime;
    lDraggableMarkerData.FDraggedMarker.MoveTo(rpt.Lon, rpt.Lat,ele,dt);
    AMapView.Invalidate;
    Handled := True; // Prevent the dragging of the map!!
  end
  else
  begin
    if Assigned(GetFirstMarkerAtMousePos(AMapView,X,Y)) then
    begin
      AMapView.Cursor := crHandPoint;
      Handled := True;
    end
    else if not Handled then
      AMapView.Cursor := crDefault;
  end
end;

procedure TDraggableMarkerPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  lpDraggableMarkerData : PDraggableMarkerData;
begin
  if not MapViewEnabled[AMapView] then Exit;
  if FDragMouseButton <> Button then Exit;
  lpDraggableMarkerData := MapViewDataPtr[AMapView];
  if Assigned(lpDraggableMarkerData) and Assigned(lpDraggableMarkerData^.FDraggedMarker) then
  begin
    if Assigned(FDraggableMarkerMovedEvent) then
      FDraggableMarkerMovedEvent(Self,lpDraggableMarkerData^.FDraggedMarker,lpDraggableMarkerData^.FOrgPosition);
    Handled := True;
    lpDraggableMarkerData^.FDraggedMarker := Nil;
  end;
end;


initialization
  RegisterPluginClass(TMarkerHintPlugin, @mvRS_MarkerHintPlugin);
  RegisterPluginClass(TMarkerClickPlugin, @mvRS_MarkerClickPlugin);
  RegisterPluginClass(TMarkerEditorPlugin, @mvRS_MarkerEditorPlugin);
  RegisterPluginClass(TDraggableMarkerPlugin, @mvRS_DraggableMarkerPlugin);

end.

