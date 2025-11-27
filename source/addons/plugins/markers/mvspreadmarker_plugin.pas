{
 mvspreadmarker_plugin

 Copyright (C) 2024 Ekkehard Domning (www.domis.de)

 License: modified LGPL with linking exception (like RTL, FCL and LCL)

 See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
 for details about the license.

 A helper class for temporary spreading closely spaced markers on the map
}
unit mvSpreadMarker_Plugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  mvMapViewer, mvStrConsts, mvTypes, mvGpsObj, mvPluginCommon, mvDrawingEngine, mvGeoMath,
  uInactivityAlarmTimer;

type
  ESpreadMarkerPluginException = class(Exception);
  ESpreadMarkerPluginRangeCheckException = class(ESpreadMarkerPluginException);

  { TSpreadMarkerRec
    Internal record to hold the data of the spreaded Markers}
  TSpreadMarkerRec = record
    GPSPoint : TGPSPoint;
    OrgRealPt: TRealPoint;
    OfsCenterX, OfsCenterY : Integer;
  end;
  TSpreadMarkerArr = array of TSpreadMarkerRec;

  TSpreadMarkerPlugin = class;

  PSpreadMarkerData = ^TSpreadMarkerData;
  TSpreadMarkerData = record
    FLastMouseX, FLastMouseY : Integer;
    FSpreadModeActive : Boolean;
    FSpreadMarkerArr : TSpreadMarkerArr;
  end;


  { TSpreadMarkerPluginData }

  TSpreadMarkerPluginData = class(TMvMultiMapsPluginData)
  private
    FPlugin : TSpreadMarkerPlugin;
    FMouseInactivity : TInactivityAlarmTimer;
    function GetSpreadMarkerCount: Integer;
  public
    { Reset clears the internal array and stops the timer
       If AReturnMarkerToOrgPosition is true then
       spreaded markers are returned to their original positions. }
    procedure Reset(const AReturnMarkerToOrgPosition : Boolean = False);
    { LeaveSpreadMode leaves an entered spreadmode and returns all markers to their
       original positions. It calls Reset and the OnSpreadModeLeft-Event }
    function LeaveSpreadMode : Boolean;
    { EnterSpreadMode enter the spreadmode if markers are available at the last Mouse
       position. It will call the OnSpreadModeEnter-Event }
    function EnterSpreadMode : Boolean;

    { SpreadMarkerCount the count of spreaded markers. Will be only <> 0 if SpreadModeActive is true }
    property SpreadMarkerCount : Integer read GetSpreadMarkerCount;
    property MouseInactivity : TInactivityAlarmTimer read FMouseInactivity;

    property Plugin : TSpreadMarkerPlugin read FPlugin write FPlugin;
    constructor Create;
    destructor Destroy;override;
  end;

  { TSpreadMarkerAcceptEvent
     will be called to decide wether a specific marker should be spreaded or not }
  TSpreadMarkerAcceptEvent = function(Sender : TSpreadMarkerPlugin; AMapView : TMapView; AGPSPoint : TGPSPoint) : Boolean of Object;

  TSpreadMarkerNotifyEvent = procedure(Sender : TSpreadMarkerPlugin; AMapView : TMapView) of Object;

  TSpreadMarkerActiveLayer = (smaLayer0,       smaLayer1, smaLayer2, smaLayer3, smaLayer4,
                              smaDefaultLayer, smaLayer6, smaLayer7, smaLayer8, smaLayer9);
  TSpreadMarkerActiveLayers = set of TSpreadMarkerActiveLayer;
  TSpreadMarkerOption = (smoDrawLine);
  TSpreadMarkerOptions = set of TSpreadMarkerOption;

const
  DefaultSpreadMarkerActiveLayers = [smaDefaultLayer];
  DefaultSpreadMarkerLinePenStyle = psSolid;
  DefaultSpreadMarkerLinePenColor = clLime;
  DefaultSpreadMarkerLinePenWidth = 1;
  DefaultSpreadMarkerOptions = [smoDrawLine];

type
  { TSpreadMarkerPlugin
    The central helper class.
    The instance must be attached to a MapView instance and several Events must be declared
    and assigned.
    The main functionallity is that if the mouse is hovering around the map and than
    don't move for a user defined time, then the markers at this point are "spreaded"
    in circles, to allow the user to distinguish between very closely spaced markers,
    which without spreading would be lay very close together
    After a second user defined time, or if the mouse button is pressed or released,
    the markers return to their original position.
  }
  TSpreadMarkerPlugin = class(TMvMultiMapsPlugin)
  private
    FActiveLayers : TSpreadMarkerActiveLayers;
    FActiveLayersEx : array[0..9] of Boolean;
    FSpreadModeEnteredEvent : TSpreadMarkerNotifyEvent;
    FSpreadModeLeftEvent : TSpreadMarkerNotifyEvent;
    FSpreadMarkerAcceptEvent : TSpreadMarkerAcceptEvent;
    FMarkerCatchSize : Integer;
    FSpreadByPixel : Integer;
    FMarkerSpreadDelayMS : Integer;
    FMarkerCollapseDelayMS : Integer;
    FLinePenStyle : TPenStyle;
    FLinePenColor : TColor;
    FLinePenWidth : Integer;
    FOptions : TSpreadMarkerOptions;
    function GetSpreadModeActive(AMapView : TMapView) : Boolean;
    function GetSpreadMarkerCount(AMapView : TMapView) : Integer;
    function GetSpreadMarkerOfsCenterX(AMapView : TMapView; {%H-}AIndex : Integer): Integer;
    function GetSpreadMarkerOfsCenterY(AMapView : TMapView; {%H-}AIndex : Integer): Integer;
    function GetSpreadMarkerOrgPositions(AMapView : TMapView; {%H-}AIndex : Integer): TRealPoint;
    function GetSpreadMarkers(AMapView : TMapView; AIndex : Integer) : TGPSPoint;
    procedure SetMarkerSpreadDelayMS(Value : Integer);
    procedure SetMarkerCollapseDelayMS(Value : Integer);
    function  GetPluginDataFromTimer(const AMouseInactivity : TInactivityAlarmTimer) : TSpreadMarkerPluginData;
    procedure OnMouseInactive(Sender : TObject);
    procedure SetActiveLayers(Value : TSpreadMarkerActiveLayers);
    procedure DrawGpsPointLine(const AMapView : TMapView;
      const AMarkerPos: TRealPoint;
      const OfsCenterX, OfsCenterY: Integer);
    procedure SetOptions(Value : TSpreadMarkerOptions);
    procedure SetLinePenStyle(Value : TPenStyle);
    procedure SetLinePenColor(Value : TColor);
    procedure SetLinePenWidth(Value : Integer);

  protected
    procedure MouseMove(AMapView: TMapView; {%H-}AShift: TShiftState; X,Y: Integer;
      var {%H-}Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var {%H-}Handled: Boolean); override;
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure AfterDrawObjects(AMapView: TMapView; var {%H-}Handled: Boolean); override;
    procedure GPSItemsModified(AMapView: TMapView; ChangedList: TGPSObjectList;
                               ActualObjs: TGPSObjList; Adding: Boolean;
                               var {%H-}Handled : Boolean);override;

    function CreateMultiMapsPluginData : TMvMultiMapsPluginData;override;

  published
    { OnSpreadModeEntered
        if assigned, the event is called to notify the application that the
        SpreadModeActive is now true.
        This event will usually be used to initiate the redraw of the map. }
    property OnSpreadModeEntered : TSpreadMarkerNotifyEvent read FSpreadModeEnteredEvent write FSpreadModeEnteredEvent;
    { OnSpreadModeLeft
        if assigned, the event is called to notify the application that the
        SpreadModeActive is now false.
        This event will usually be used to initiate the redraw of the map. }
    property OnSpreadModeLeft : TSpreadMarkerNotifyEvent read FSpreadModeLeftEvent write FSpreadModeLeftEvent;
    { OnSpreadMarkerAccept
        if assigned, the event is called to query if a certain marker must be moved
        or not. If not assigned all found markers will be moved }
    property OnSpreadMarkerAccept : TSpreadMarkerAcceptEvent read FSpreadMarkerAcceptEvent write FSpreadMarkerAcceptEvent;
    { SpreadByPixel the number of pixels between the spreaded markers. Depends on
      the size of the markers on the map }
    property SpreadByPixel : Integer read FSpreadByPixel write FSpreadByPixel;
    { MarkerCatchSize the width/height of the rectangle (in Pixel) where the markers
      are counted. If more than one marker exists in this rectangle, the spreading is performed }
    property MarkerCatchSize : Integer read FMarkerCatchSize write FMarkerCatchSize;
    { MarkerSpreadDelayMS the time in millisecond after the last mouse move when the markers
       are spread }
    property MarkerSpreadDelayMS : Integer read FMarkerSpreadDelayMS write SetMarkerSpreadDelayMS;
    { MarkerCollapseDelayMS the time in millisecond after the last mouse move when the markers
       are returning to their original position }
    property MarkerCollapseDelayMS : Integer read FMarkerCollapseDelayMS write SetMarkerCollapseDelayMS;
    property ActiveLayers : TSpreadMarkerActiveLayers read FActiveLayers write SetActiveLayers default DefaultSpreadMarkerActiveLayers;
    property Options : TSpreadMarkerOptions read FOptions write SetOptions default DefaultSpreadMarkerOptions;
    property LinePenStyle : TPenStyle read FLinePenStyle write SetLinePenStyle default DefaultSpreadMarkerLinePenStyle;
    property LinePenColor : TColor read FLinePenColor write SetLinePenColor default DefaultSpreadMarkerLinePenColor;
    property LinePenWidth : Integer read FLinePenWidth write SetLinePenWidth default DefaultSpreadMarkerLinePenWidth;

  public
    property SpreadModeActive[AMapView : TMapView] : Boolean read GetSpreadModeActive;

    { SpreadMarkerCount the count of spreaded markers. Will be only <> 0 if SpreadModeActive is true }
    property SpreadMarkerCount[AMapView : TMapView] : Integer read GetSpreadMarkerCount;
    { SpreadMarkers the spreaded markers. Index must be between 0 and SpreadMarkerCount-1 }
    property SpreadMarkers[AMapView : TMapView; AIndex : Integer] : TGPSPoint read GetSpreadMarkers;
    { SpreadMarkersOrgPosition the original position of the spreaded markers.
      Index must be between 0 and SpreadMarkerCount-1 }
    property SpreadMarkerOrgPositions[AMapView : TMapView; AIndex : Integer] : TRealPoint read GetSpreadMarkerOrgPositions;
    { SpreadMarkerOfsCenterX the offset in pixel of the current spread marker position to the center of
      all spreaded markers (= where the mouse location on the init was).
      Index must be between 0 and SpreadMarkerCount-1 }
    property SpreadMarkerOfsCenterX[AMapView : TMapView; AIndex : Integer] : Integer read GetSpreadMarkerOfsCenterX;
    { SpreadMarkerOfsCenterY the offset in pixel of the current spread marker position to the center of
      all spreaded markers (= where the mouse location on the init was).
      Index must be between 0 and SpreadMarkerCount-1 }
    property SpreadMarkerOfsCenterY[AMapView : TMapView; AIndex : Integer] : Integer read GetSpreadMarkerOfsCenterY;

    { IndexOfMarker returns the index of the passed Item in the spreaded marker array.
       -1 if not found }
    function IndexOfMarker(AMapView : TMapView; AItem : TGPSPoint) : Integer;
    { LeaveSpreadMode leaves an entered spreadmode and returns all markers to their
       original positions. It calls Reset and the OnSpreadModeLeft-Event }
    procedure LeaveSpreadMode(AMapView : TMapView);virtual;
    { EnterSpreadMode enter the spreadmode if markers are available at the last Mouse
       position. It will call the OnSpreadModeEnter-Event }
    procedure EnterSpreadMode(AMapView : TMapView);virtual;
    { RecalculatePositions will only recalculate all positions, for the existing
      spread markers. It will not call any events }
    procedure RecalculatePositions(AMapView : TMapView);virtual;
    { Create initiate the helper to be used with the passed AMapView and the passed
       ALayer. If the ALayer is not assigned the GPSLayer[0] from the AMapView is used  }
    constructor Create(AOwner : TComponent);override;
    { Destroy frees the helper class.
      CAUTION: The method calls Reset(false), which will not change any marker positions.
               If the markers has to return to their original positions,
               call LeaveSpreadMode prior Free. }
    destructor Destroy;override;
  end;

const
  { DefaultMarkerCatchSize an array of 6x6 Pixel will be used for Marker counting }
  DefaultMarkerCatchSize = 6;
  { DefaultSpreadByPixel spreaded markers are placed on a distance of 16 pixels}
  DefaultSpreadByPixel = 16;
  { DefaultMarkerSpreadDelayMS after 150ms the spread will happen }
  DefaultMarkerSpreadDelayMS = 150;
  { DefaultMarkerCollapseDelayMS after 1.5ms the spread will be terminated }
  DefaultMarkerCollapseDelayMS = 1500;

implementation

{ TSpreadMarkerPlugin }

function TSpreadMarkerPlugin.GetSpreadModeActive(AMapView: TMapView): Boolean;
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
begin
  Result := False;
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if Assigned(sd) then
  begin
    pd := sd.GetDataPtr;
    Result := pd^.FSpreadModeActive;
  end;
end;

function TSpreadMarkerPlugin.GetSpreadMarkerCount(AMapView: TMapView): Integer;
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
begin
  Result := 0;
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if Assigned(sd) then
  begin
    pd := sd.GetDataPtr;
    Result := Length(pd^.FSpreadMarkerArr);
  end;
end;

function TSpreadMarkerPlugin.GetSpreadMarkers(AMapView: TMapView;
  AIndex: Integer): TGPSPoint;
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
begin
  Result := Nil;
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if Assigned(sd) then
  begin
    pd := sd.GetDataPtr;
    Result := pd^.FSpreadMarkerArr[AIndex].GPSPoint;
  end;
end;

function TSpreadMarkerPlugin.GetSpreadMarkerOrgPositions(AMapView: TMapView;
  AIndex: Integer): TRealPoint;
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then
    raise ESpreadMarkerPluginRangeCheckException.Create(mvRS_MapViewNotFound);
  pd := sd.GetDataPtr;
  Result := pd^.FSpreadMarkerArr[Index].OrgRealPt;
end;

function TSpreadMarkerPlugin.GetSpreadMarkerOfsCenterX(AMapView: TMapView;
  AIndex: Integer): Integer;
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then
    raise ESpreadMarkerPluginRangeCheckException.Create(mvRS_MapViewNotFound);
  pd := sd.GetDataPtr;
  Result := pd^.FSpreadMarkerArr[Index].OfsCenterX;
end;

function TSpreadMarkerPlugin.GetSpreadMarkerOfsCenterY(AMapView: TMapView;
  AIndex: Integer): Integer;
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then
    raise ESpreadMarkerPluginRangeCheckException.Create(mvRS_MapViewNotFound);
  pd := sd.GetDataPtr;
  Result := pd^.FSpreadMarkerArr[Index].OfsCenterY;
end;

procedure TSpreadMarkerPlugin.SetMarkerSpreadDelayMS(Value: Integer);
var
  i : Integer;
  sd : TSpreadMarkerPluginData;
begin
  FMarkerSpreadDelayMS := Value;
  if not Enabled then
  begin
    for i := 0 to MapDataList.Count-1 do
    begin
      sd := TSpreadMarkerPluginData(MapDataList.Items[i]);
      sd.FMouseInactivity.InactivityTimeOutMS := FMarkerSpreadDelayMS;
    end;
  end;
end;

procedure TSpreadMarkerPlugin.SetMarkerCollapseDelayMS(Value: Integer);
var
  i : Integer;
  sd : TSpreadMarkerPluginData;
begin
  FMarkerCollapseDelayMS := Value;
  if not Enabled then
  begin
    for i := 0 to MapDataList.Count-1 do
    begin
      sd := TSpreadMarkerPluginData(MapDataList.Items[i]);
      sd.FMouseInactivity.InactivityTimeOutMS := FMarkerCollapseDelayMS;
    end;
  end;
end;

function TSpreadMarkerPlugin.GetPluginDataFromTimer(
  const AMouseInactivity: TInactivityAlarmTimer): TSpreadMarkerPluginData;
var
  i : Integer;
  sd : TSpreadMarkerPluginData;
begin
  Result := Nil;
  for i := 0 to MapDataList.Count-1 do
  begin
    sd := TSpreadMarkerPluginData(MapDataList.Items[i]);
    if sd.FMouseInactivity = AMouseInactivity then
      Exit(sd);
  end;
end;

procedure TSpreadMarkerPlugin.OnMouseInactive(Sender: TObject);
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
  mb : TMouseButtons;
begin
  if not (Sender is TInactivityAlarmTimer) then Exit;
  sd := GetPluginDataFromTimer(TInactivityAlarmTimer(Sender));
  if not Assigned(sd) then Exit;
  pd := PSpreadMarkerData(sd.GetDataPtr);

  mb := PluginManager.MouseButtonDown[sd.MapView];
  if mb <> [] then
  begin
    TInactivityAlarmTimer(Sender).QuitInactivityAlarm;
    Exit;
  end;

  if pd^.FSpreadModeActive then
    sd.LeaveSpreadMode
  else
    sd.EnterSpreadMode;
end;

procedure TSpreadMarkerPlugin.SetActiveLayers(Value: TSpreadMarkerActiveLayers);
begin
  if FActiveLayers = Value then Exit;
  FActiveLayers := Value;
  FActiveLayersEx[0] := (smaLayer0 in Value);
  FActiveLayersEx[1] := (smaLayer1 in Value);
  FActiveLayersEx[2] := (smaLayer2 in Value);
  FActiveLayersEx[3] := (smaLayer3 in Value);
  FActiveLayersEx[4] := (smaLayer4 in Value);
  FActiveLayersEx[5] := (smaDefaultLayer in Value);
  FActiveLayersEx[6] := (smaLayer6 in Value);
  FActiveLayersEx[7] := (smaLayer7 in Value);
  FActiveLayersEx[8] := (smaLayer8 in Value);
  FActiveLayersEx[9] := (smaLayer9 in Value);
end;

procedure TSpreadMarkerPlugin.MouseMove(AMapView: TMapView;
  AShift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
  ld : TSpreadMarkerData;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then
  begin
    FillChar(ld{%H-},SizeOf(ld),0);
    SetMapViewData(AMapView,ld,SizeOf(ld));
  end;
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then Exit;
  pd := PSpreadMarkerData(sd.GetDataPtr);
  // (Re-)Activate the timer
  if sd.FMouseInactivity.Active then
    sd.FMouseInactivity.AliveTrigger // Trigger
  else
    sd.FMouseInactivity.Active := True; // Activate (will set the timer)
  // Store the last Mouse position for later use
  pd^.FLastMouseX := X;
  pd^.FLastMouseY := Y;
end;

procedure TSpreadMarkerPlugin.ZoomChange(AMapView: TMapView;
  var Handled: Boolean);
var
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
  ld : TSpreadMarkerData;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then
  begin
    FillChar(ld{%H-},SizeOf(ld),0);
    SetMapViewData(AMapView,ld,SizeOf(ld));
  end;
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then Exit;
  pd := PSpreadMarkerData(sd.GetDataPtr);
  // if the zoom of the map changes during the spread mode is active
  // leave the spread mode and enter immediatly again to update the
  // spreaded markers in the area
  if pd^.FSpreadModeActive then
  begin
    sd.LeaveSpreadMode;
    sd.EnterSpreadMode;
  end;
end;

procedure TSpreadMarkerPlugin.DrawGpsPointLine(const AMapView : TMapView;
  const AMarkerPos: TRealPoint;
  const OfsCenterX, OfsCenterY: Integer);
var
  lDrawer : TMvCustomDrawingEngine;
  pt, ptMarker, ptCenter : TPoint;
  ptCyc : TPointArray;
  lWorldSize : Int64;
  lOldPenColor : TColor;
  lOldPenWidth : Integer;
  lOldPenStyle : TPenStyle;
begin
  lWorldSize := mvGeoMath.ZoomFactor(AMapView.Zoom) * TileSize.CX;
  lDrawer := AMapView.DrawingEngine;
  // Preserve settings
  lOldPenColor := lDrawer.PenColor;
  lOldPenWidth := lDrawer.PenWidth;
  lOldPenStyle := lDrawer.PenStyle;
  try
    // Presetting
    lDrawer.PenColor := FLinePenColor;
    lDrawer.PenWidth := FLinePenWidth;
    lDrawer.PenStyle := FLinePenStyle;
    ptMarker := AMapView.LatLonToScreen(AMarkerPos);
    ptCyc := AMapView.CyclicPointsOf(ptMarker);
    for pt in ptCyc do
    begin
      ptMarker := pt;
      ptCenter.X := ptMarker.X - OfsCenterX;
      ptCenter.Y := ptMarker.Y - OfsCenterY;
      lDrawer.Line(ptMarker.X, ptMarker.Y, ptCenter.X, ptCenter.Y);
      if (ptCenter.X < 0) then
      begin
        ptCenter.X := ptCenter.X + lWorldSize;
        ptMarker.X := ptMarker.X + lWorldSize;
        lDrawer.Line(ptMarker.X, ptMarker.Y, ptCenter.X, ptCenter.Y);
      end
      else if (ptCenter.X > AMapView.Width) then
      begin
        ptCenter.X := ptCenter.X - lWorldSize;
        ptMarker.X := ptMarker.X - lWorldSize;
        lDrawer.Line(ptMarker.X, ptMarker.Y, ptCenter.X, ptCenter.Y);
      end;
    end;
  finally
    // Return the original setting
    lDrawer.PenColor := lOldPenColor;
    lDrawer.PenWidth := lOldPenWidth;
    lDrawer.PenStyle := lOldPenStyle;
  end;
end;

procedure TSpreadMarkerPlugin.SetOptions(Value: TSpreadMarkerOptions);
begin
  if FOptions = Value then Exit;
  FOptions := Value;
  // Invalidate??
end;

procedure TSpreadMarkerPlugin.SetLinePenStyle(Value: TPenStyle);
begin
  if FLinePenStyle = Value then Exit;
  FLinePenStyle := Value;
  // Invalidate??
end;

procedure TSpreadMarkerPlugin.SetLinePenColor(Value: TColor);
begin
  if FLinePenColor = Value then Exit;
  FLinePenColor := Value;
  // Invalidate??
end;

procedure TSpreadMarkerPlugin.SetLinePenWidth(Value: Integer);
begin
  if FLinePenWidth = Value then Exit;
  FLinePenWidth := Value;
  // Invalidate??
end;

procedure TSpreadMarkerPlugin.AfterPaint(AMapView: TMapView;
  var Handled: Boolean);
begin
  inherited;
end;

procedure TSpreadMarkerPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
var
  i : Integer;
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
  lArea: TRealArea;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then Exit;
  pd := PSpreadMarkerData(sd.GetDataPtr);
  if smoDrawLine in FOptions then
  begin
    for i := 0 to High(pd^.FSpreadMarkerArr) do
    begin
      DrawGpsPointLine(AMapView,
                       pd^.FSpreadMarkerArr[i].OrgRealPt,
                       pd^.FSpreadMarkerArr[i].OfsCenterX,
                       pd^.FSpreadMarkerArr[i].OfsCenterY);
    end;
  end;
  lArea.Init(-180.0,90.0,180.0,-90.0);
  for i := 0 to High(pd^.FSpreadMarkerArr) do
    pd^.FSpreadMarkerArr[i].GPSPoint.Draw(AMapView,lArea);
end;

procedure TSpreadMarkerPlugin.GPSItemsModified(AMapView: TMapView;
  ChangedList: TGPSObjectList; ActualObjs: TGPSObjList; Adding: Boolean;
  var Handled: Boolean);
var
  i : Integer;
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
  ndx : Integer;
  cnt : Integer;
  chg : Boolean;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then Exit;
  pd := PSpreadMarkerData(sd.GetDataPtr);
  cnt := 0;
  if (not Assigned(ChangedList)) or (ChangedList.Count <= 0) then
  begin
    sd.Reset(False);  // The list is empty, reset the markers, if any, without resetting their positions
    Exit;
  end;
  if Adding and Assigned(ActualObjs) then Exit; // Items are added, so no change here necesarry

  if Assigned(ActualObjs) then
  begin  // Removed Items are summarized in Objs
    for i := 0 to High(pd^.FSpreadMarkerArr) do
    begin
      ndx := ActualObjs.IndexOf(pd^.FSpreadMarkerArr[i].GPSPoint);
      if ndx < 0 then Continue;
      pd^.FSpreadMarkerArr[i].GPSPoint := Nil;
      Inc(cnt);
    end;
  end
  else
  begin // Items could be removed (is unkonwn due to BeginUpdate/EndUpdate usage)
    for i := 0 to High(pd^.FSpreadMarkerArr) do
    begin
      ndx := ChangedList.IndexOf(pd^.FSpreadMarkerArr[i].GPSPoint);
      if ndx < 0 then
      begin
        pd^.FSpreadMarkerArr[i].GPSPoint := Nil;
        Inc(cnt);
      end;
    end;
  end;
  if cnt <= 0 then Exit; // no change
  if cnt = Length(pd^.FSpreadMarkerArr) then
  begin
    sd.Reset(False);
    Exit;
  end;
  // At least one item has been removed, compact the array
  repeat
    chg := False;
    for i := 1 to High(pd^.FSpreadMarkerArr) do
    begin
      if (not Assigned(pd^.FSpreadMarkerArr[i-1].GPSPoint)) and
         Assigned(pd^.FSpreadMarkerArr[i].GPSPoint) then
      begin
        pd^.FSpreadMarkerArr[i-1] := pd^.FSpreadMarkerArr[i];
        pd^.FSpreadMarkerArr[i].GPSPoint := Nil;
        chg := True;
      end;
    end;
  until not chg;
  SetLength(pd^.FSpreadMarkerArr,Length(pd^.FSpreadMarkerArr)-cnt);
  // Update the marker positions will fail, since the mouse has been moved
  // Not working: RecalculatePositions(AMapView);
end;


function TSpreadMarkerPlugin.CreateMultiMapsPluginData: TMvMultiMapsPluginData;
var
  sd : TSpreadMarkerPluginData;
begin
  sd := TSpreadMarkerPluginData.Create;
  Result := sd;
  sd.Plugin := Self;
  sd.FMouseInactivity.InactivityTimeOutMS := FMarkerSpreadDelayMS;
  sd.FMouseInactivity.InactivityAlarmDetectedEvent := @OnMouseInactive;
end;

procedure TSpreadMarkerPlugin.RecalculatePositions(AMapView: TMapView);
var
  rPoint : TRealPoint;
  i : Integer;
  pt : TPoint;
  cnt : Integer;
  ndx : Integer;
  angle, anglerad : Double;
  circlendx : Integer;
  itemspercircle : Integer;
  remaincnt : Integer;
  lXd, lYd : Double;
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then
    raise ESpreadMarkerPluginRangeCheckException.Create(mvRS_MapViewNotFound);
  pd := PSpreadMarkerData(sd.GetDataPtr);
  if (not Assigned(pd)) or (not pd^.FSpreadModeActive) then Exit;

  // The markers are distributed into circles
  // The smallest circle will contain 6 items, the next bigger one 12, ...
  // If a circle can hold more items than available, the items are distributed
  // with a bigger angle
  ndx := 0; // start with the first item
  circlendx := 1; // start with the innerst circle
  while ndx < Length(pd^.FSpreadMarkerArr) do
  begin // loop until no more items are available
    // calculate how many items will fit to this circle
    itemspercircle := Trunc(circlendx * Pi() * 2);
    // calculate how many items are availabale
    remaincnt := Length(pd^.FSpreadMarkerArr) - ndx;
    // If more or equal items available tzhan fitting on the circle, take them
    if (remaincnt >= itemspercircle) then
      cnt := itemspercircle
    else // if less, use the remainig
      cnt := remaincnt;
    // calculate the distribution angle
    angle := 360.0 / cnt;
    // distribute the items
    for i := 0 to cnt-1 do
    begin
      // The position is calculated by the Sin/Cos value for the desired angle
      // and than multiplying the result by the user defined spacing and
      // arranging around the mouse position.
      anglerad := (angle * i)/180.0 * Pi();
      lXd := Sin(anglerad);
      lYd := Cos(anglerad);
      pt.X := Round(lXd * circlendx * FSpreadByPixel);
      pt.Y := Round(lYd * circlendx * FSpreadByPixel);
      pd^.FSpreadMarkerArr[ndx].OfsCenterX := pt.X;
      pd^.FSpreadMarkerArr[ndx].OfsCenterY := -pt.Y;
      pt.X := pt.X + pd^.FLastMouseX;
      pt.Y := pt.Y + pd^.FLastMouseY;
      rPoint := AMapView.ScreenToLatLon(pt);
      // Now move the item to the new, temporary position
      pd^.FSpreadMarkerArr[ndx].GPSPoint.MoveTo(
        rPoint.Lon,
        rPoint.Lat,
        pd^.FSpreadMarkerArr[ndx].GPSPoint.Elevation,
        pd^.FSpreadMarkerArr[ndx].GPSPoint.DateTime);
      // next item
      Inc(Ndx);
    end;
    // next circle
    Inc(circlendx);
  end;
end;

procedure TSpreadMarkerPlugin.EnterSpreadMode(AMapView: TMapView);
var
  sd : TSpreadMarkerPluginData;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then
    raise ESpreadMarkerPluginRangeCheckException.Create(mvRS_MapViewNotFound);
  sd.EnterSpreadMode;
end;

procedure TSpreadMarkerPlugin.LeaveSpreadMode(AMapView: TMapView);
var
  sd : TSpreadMarkerPluginData;
begin
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then
    raise ESpreadMarkerPluginRangeCheckException.Create(mvRS_MapViewNotFound);
  sd.LeaveSpreadMode;
{
  if not FSpreadModeActive then Exit;
  // Return all markers to their original positions
  // and reset the engine
  Reset(True);
  // setup the timer
  FMouseInactivity.InactivityTimeOutMS := FMarkerSpreadDelayMS;
  // deactivate the timer. This prevents the toggle between spreading and
  // collapsing if the mouse does not move
  FMouseInactivity.Active := False;
  // notify the main app
  if Assigned(FSpreadModeLeftEvent) then
    FSpreadModeLeftEvent(Self);
}
end;

function TSpreadMarkerPlugin.IndexOfMarker(AMapView: TMapView; AItem: TGPSPoint
  ): Integer;
var
  i : Integer;
  sd : TSpreadMarkerPluginData;
  pd : PSpreadMarkerData;
begin
  Result := -1;
  sd := TSpreadMarkerPluginData(MapViewDataItem[AMapView]);
  if not Assigned(sd) then Exit;
  pd := PSpreadMarkerData(sd.GetDataPtr);
  for i := 0 to High(pd^.FSpreadMarkerArr) do
    if pd^.FSpreadMarkerArr[i].GPSPoint = AItem then
      Exit(i);
end;

constructor TSpreadMarkerPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActiveLayers := DefaultSpreadMarkerActiveLayers;
  FMarkerCatchSize := DefaultMarkerCatchSize;
  FSpreadByPixel := DefaultSpreadByPixel;
  FMarkerSpreadDelayMS := DefaultMarkerSpreadDelayMS;
  FMarkerCollapseDelayMS := DefaultMarkerCollapseDelayMS;
  FOptions := DefaultSpreadMarkerOptions;
  FLinePenStyle := DefaultSpreadMarkerLinePenStyle;
  FLinePenColor := DefaultSpreadMarkerLinePenColor;
  FLinePenWidth := DefaultSpreadMarkerLinePenWidth;
end;

destructor TSpreadMarkerPlugin.Destroy;
begin
  inherited Destroy;
end;

{ TSpreadMarkerPluginData }

function TSpreadMarkerPluginData.GetSpreadMarkerCount: Integer;
var
  pd : PSpreadMarkerData;
begin
  pd := PSpreadMarkerData(GetDataPtr);
  Result := Length(pd^.FSpreadMarkerArr);
end;

procedure TSpreadMarkerPluginData.Reset(
  const AReturnMarkerToOrgPosition: Boolean);
var
  i : Integer;
  pd : PSpreadMarkerData;
begin
  pd := PSpreadMarkerData(GetDataPtr);
  if AReturnMarkerToOrgPosition and pd^.FSpreadModeActive then
  begin
    // Return all markers to their original positions
    for i := 0 to High(pd^.FSpreadMarkerArr) do
      pd^.FSpreadMarkerArr[i].GPSPoint.MoveTo(
        pd^.FSpreadMarkerArr[i].OrgRealPt.Lon,
        pd^.FSpreadMarkerArr[i].OrgRealPt.Lat,
        pd^.FSpreadMarkerArr[i].GPSPoint.Elevation,
        pd^.FSpreadMarkerArr[i].GPSPoint.DateTime);
  end;
  SetLength(pd^.FSpreadMarkerArr,0);
  pd^.FSpreadModeActive := False;
end;

function TSpreadMarkerPluginData.LeaveSpreadMode: Boolean;
var
  pd : PSpreadMarkerData;
begin
  Result := False;
  pd := PSpreadMarkerData(GetDataPtr);
  if not pd^.FSpreadModeActive then Exit;
  Result := True;

  // Return all markers to their original positions
  // and reset the engine
  Reset(True);
  MapView.Invalidate;
  // setup the timer
  FMouseInactivity.InactivityTimeOutMS := Plugin.MarkerSpreadDelayMS;
  // deactivate the timer. This prevents the toggle between spreading and
  // collapsing if the mouse does not move
  FMouseInactivity.Active := False;
  // notify the main app
  if Assigned(Plugin.OnSpreadModeLeft) then
    Plugin.OnSpreadModeLeft(Plugin,MapView);
end;

function TSpreadMarkerPluginData.EnterSpreadMode: Boolean;
var
  gpsList : TGPSObjList;
  i,j : Integer;
  pt : TPoint;
  cs2 : Integer;
  aArea : TRealArea;
  cnt : Integer;
  lGpsPt : TGpsPoint;
  pd : PSpreadMarkerData;
  list : TList;
begin
  Result := False;
  pd := PSpreadMarkerData(GetDataPtr);
  if pd^.FSpreadModeActive then Exit;
  // First setup the area to be checked.
  cs2 := Plugin.MarkerCatchSize div 2;
  if cs2 < 1 then
    cs2 := 1;
  pt.X := pd^.FLastMouseX-cs2;
  pt.Y := pd^.FLastMouseY-cs2;
  aArea.TopLeft := MapView.ScreenToLatLon(pt);
  pt.X := pd^.FLastMouseX+cs2;
  pt.Y := pd^.FLastMouseY+cs2;
  aArea.BottomRight := MapView.ScreenToLatLon(pt);

  list := TList.Create;
  try
    for j := 0 to High(Plugin.FActiveLayersEx) do
    begin
      // Ignore inactive layers
      if not Plugin.FActiveLayersEx[j] then Continue;

      // Return all items (which belongs to this "layer") on the screen in one list
      gpsList := MapView.GPSLayer[j].GetObjectsInArea(aArea);
      try
        cnt := gpsList.Count;
        for i:=0 to cnt-1 do
        begin
          if (gpsList[i] is TGpsPoint)  then
          begin
            lGpsPt := TGpsPoint(gpsList[i]);
            // Ask the user whether he want a specific item to be spreaded or not
            if (not Assigned(Plugin.OnSpreadMarkerAccept)) or
               Plugin.OnSpreadMarkerAccept(Plugin,MapView,lGpsPt) then
              list.Add(lGpsPt);
          end;
        end;
      finally
        gpsList.Free;
      end;
    end;

    // Do not enter SpreadMode if less than two items are found in the area
    cnt := list.Count;
    if cnt < 2 then Exit;
    // Now fetch the items into the array
    SetLength(pd^.FSpreadMarkerArr,cnt);
    for i := 0 to cnt-1 do
    begin
      lGpsPt := TGpsPoint(list.Items[i]);
      pd^.FSpreadMarkerArr[i].GPSPoint := lGpsPt;
      pd^.FSpreadMarkerArr[i].OrgRealPt := lGpsPt.RealPoint;
    end;

  finally
    list.Free;
  end;
  Result := True;
  // Switch to spread mode
  pd^.FSpreadModeActive := True;

  // (Re-) calculate positions
  Plugin.RecalculatePositions(MapView);
  MapView.Invalidate;

  // setup the time to the second (collapse) time
  FMouseInactivity.InactivityTimeOutMS := Plugin.MarkerCollapseDelayMS;
  // Trigger the timer, to allow the collapse even if the mouse don't move
  FMouseInactivity.AliveTrigger;
  // notify the main program
  if Assigned(Plugin.OnSpreadModeEntered) then
    Plugin.OnSpreadModeEntered(Plugin,MapView);
end;

constructor TSpreadMarkerPluginData.Create;
var
  dummy : array of Byte = Nil;
begin
  inherited Create;
  SetLength(dummy,SizeOf(TSpreadMarkerData));
  SetData(dummy[0],SizeOf(TSpreadMarkerData));
  FMouseInactivity := TInactivityAlarmTimer.Create;
  FMouseInactivity.InactivityAlarmQuitKind := iaqAutoActivity;
  FMouseInactivity.ResponseGranularityMS := 10;
end;

destructor TSpreadMarkerPluginData.Destroy;
begin
  FMouseInactivity.Active := False;
  Reset(False);
  FMouseInactivity.Free;
  inherited Destroy;
end;

initialization
  RegisterPluginClass(TSpreadMarkerPlugin, @mvRS_SpreadMarkerPlugin);

end.

