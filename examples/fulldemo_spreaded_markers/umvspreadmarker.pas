{
 umvSpreadMarker

 Copyright (C) 2024 Ekkehard Domning (www.domis.de)

 License: modified LGPL with linking exception (like RTL, FCL and LCL)

 See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
 for details about the license.

 A helper class for temporary spreading closely spaced markers on the map
}
unit umvSpreadMarker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  mvMapViewer, mvTypes, mvGpsObj,
  uInactivityAlarmTimer;

type
  { TSpreadMarkerRec
    Internal record to hold the data of the spreaded Markers}
  TSpreadMarkerRec = record
    GPSPoint : TGPSPoint;
    OrgRealPt: TRealPoint;
    OfsCenterX, OfsCenterY : Integer;
  end;
  TSpreadMarkerArr = array of TSpreadMarkerRec;

  { TMapViewSpreadMarkerAcceptEvent
     will be called to decide wether a specific marker should be spreaded or not }
  TMapViewSpreadMarkerAcceptEvent = function(Sender : TObject; AGPSPoint : TGPSPoint) : Boolean of Object;
  { TMapViewSpreadMarkerHelper
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
  TMapViewSpreadMarkerHelper = class(TObject)
  private
    FLastMouseX, FLastMouseY : Integer;
    FMouseButtonIsDown : Boolean;
    FMapView : TMapView;
    FMapLayer : TGPSObjectList;
    FSpreadModeActive : Boolean;
    FSpreadMarkerArr : TSpreadMarkerArr;
    FSpreadModeEnteredEvent : TNotifyEvent;
    FSpreadModeLeftEvent : TNotifyEvent;
    FSpreadMarkerAcceptEvent : TMapViewSpreadMarkerAcceptEvent;
    FMouseInactivity : TInactivityAlarmTimer;
    FMarkerCatchSize : Integer;
    FSpreadByPixel : Integer;
    FMarkerSpreadDelayMS : Integer;
    FMarkerCollapseDelayMS : Integer;
    function GetSpreadMarkerCount : Integer;
    function GetSpreadMarkers(Index : Integer) : TGPSPoint;
    function GetSpreadMarkerOrgPositions(Index : Integer) : TRealPoint;
    function GetSpreadMarkerOfsCenterX(Index : Integer) : Integer;
    function GetSpreadMarkerOfsCenterY(Index : Integer) : Integer;

    procedure SetMarkerSpreadDelayMS(Value : Integer);
    procedure SetMarkerCollapseDelayMS(Value : Integer);
    procedure OnMouseInactive(Sender : TObject);
  protected
  public
    { OnSpreadModeEntered
        if assigned, the event is called to notify the application that the
        SpreadModeActive is now true.
        This event will usually be used to initiate the redraw of the map. }
    property OnSpreadModeEntered : TNotifyEvent read FSpreadModeEnteredEvent write FSpreadModeEnteredEvent;
    { OnSpreadModeLeft
        if assigned, the event is called to notify the application that the
        SpreadModeActive is now false.
        This event will usually be used to initiate the redraw of the map. }
    property OnSpreadModeLeft : TNotifyEvent read FSpreadModeLeftEvent write FSpreadModeLeftEvent;
    { OnSpreadMarkerAccept
        if assigned, the event is called to query if a certain marker must be moved
        or not. If not assigned all found markers will be moved }
    property OnSpreadMarkerAccept : TMapViewSpreadMarkerAcceptEvent read FSpreadMarkerAcceptEvent write FSpreadMarkerAcceptEvent;
    { SpreadMarkerCount the count of spreaded markers. Will be only <> 0 if SpreadModeActive is true }
    property SpreadMarkerCount : Integer read GetSpreadMarkerCount;
    { SpreadMarkers the spreaded markers. Index must be between 0 and SpreadMarkerCount-1 }
    property SpreadMarkers[Index : Integer] : TGPSPoint read GetSpreadMarkers;
    { SpreadMarkersOrgPosition the original position of the spreaded markers.
      Index must be between 0 and SpreadMarkerCount-1 }
    property SpreadMarkerOrgPositions[Index : Integer] : TRealPoint read GetSpreadMarkerOrgPositions;
    { SpreadMarkerOfsCenterX the offset in pixel of the current spread marker position to the center of
      all spreaded markers (= where the mouse location on the init was).
      Index must be between 0 and SpreadMarkerCount-1 }
    property SpreadMarkerOfsCenterX[Index : Integer] : Integer read GetSpreadMarkerOfsCenterX;
    { SpreadMarkerOfsCenterY the offset in pixel of the current spread marker position to the center of
      all spreaded markers (= where the mouse location on the init was).
      Index must be between 0 and SpreadMarkerCount-1 }
    property SpreadMarkerOfsCenterY[Index : Integer] : Integer read GetSpreadMarkerOfsCenterY;

    { SpreadModeActive, if true the markers are spreaded, if false no markers are spreaded }
    property SpreadModeActive : Boolean read FSpreadModeActive;
    { SpreadByPixel the number of pixels between the spreaded markers. Depends on
      the size of the markers on the map }
    property SpreadByPixel : Integer read FSpreadByPixel write FSpreadByPixel;
    { MapView the MapView to operate on }
    property MapView : TMapView read FMapView;
    { MarkerCatchSize the width/height of the rectangle (in Pixel) where the markers
      are counted. If more than one marker exists in this rectangle, the spreading is performed }
    property MarkerCatchSize : Integer read FMarkerCatchSize write FMarkerCatchSize;
    { MarkerSpreadDelayMS the time in millisecond after the last mouse move when the markers
       are spread }
    property MarkerSpreadDelayMS : Integer read FMarkerSpreadDelayMS write SetMarkerSpreadDelayMS;
    { MarkerCollapseDelayMS the time in millisecond after the last mouse move when the markers
       are returning to their original position }
    property MarkerCollapseDelayMS : Integer read FMarkerCollapseDelayMS write SetMarkerCollapseDelayMS;
    { IndexOfMarker returns the index of the passed Item in the spreaded marker array.
       -1 if not found }
    function IndexOfMarker(Item : TGPSPoint) : Integer;
    { MapViewMouseMove must be called from the MouseMove-Event of the attached MapView }
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    { MapViewMouseDown should be called from the MouseDown-Event of the attached MapView.
       The current spread mode is left and a new spread mode is initiated.}
    procedure MapViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    { MapViewMouseUp should be called from the MouseDown-Event of the attached MapView. }
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    { MapViewZoomChange should be called from the ZoomChange-Event of the attached MapView.
        If  the spread mode is active, the spread markers are updated to the new map conditions }
    procedure MapViewZoomChange(Sender: TObject);
    { Reset clears the internal array and stops the timer
       If AReturnMarkerToOrgPosition is true then
       spreaded markers are returned to their original positions. }
    procedure Reset(const AReturnMarkerToOrgPosition : Boolean = False);
    { LeaveSpreadMode leaves an entered spreadmode and returns all markers to their
       original positions. It calls Reset and the OnSpreadModeLeft-Event }
    procedure LeaveSpreadMode;virtual;
    { EnterSpreadMode enter the spreadmode if markers are available at the last Mouse
       position. It will call the OnSpreadModeEnter-Event }
    procedure EnterSpreadMode;virtual;
    { RecalculatePositions will only recalculate all positions, for the existing
      spread markers. It will not call any events }
    procedure RecalculatePositions;virtual;
    { Create initiate the helper to be used with the passed AMapView and the passed
       ALayer. If the ALayer is not assigned the GPSLayer[0] from the AMapView is used  }
    constructor Create(const AMapView : TMapView; const AMapLayer : TGPSObjectList = Nil);
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

{ TMapViewSpreadMarkerHelper }

function TMapViewSpreadMarkerHelper.GetSpreadMarkerCount: Integer;
begin
  Result := Length(FSpreadMarkerArr);
end;

function TMapViewSpreadMarkerHelper.GetSpreadMarkers(Index: Integer): TGPSPoint;
begin
  Result := FSpreadMarkerArr[Index].GPSPoint;
end;

function TMapViewSpreadMarkerHelper.GetSpreadMarkerOrgPositions(Index: Integer
  ): TRealPoint;
begin
  Result := FSpreadMarkerArr[Index].OrgRealPt;
end;

function TMapViewSpreadMarkerHelper.GetSpreadMarkerOfsCenterX(Index: Integer
  ): Integer;
begin
  Result := FSpreadMarkerArr[Index].OfsCenterX;
end;

function TMapViewSpreadMarkerHelper.GetSpreadMarkerOfsCenterY(Index: Integer
  ): Integer;
begin
  Result := FSpreadMarkerArr[Index].OfsCenterY;
end;

procedure TMapViewSpreadMarkerHelper.SetMarkerSpreadDelayMS(Value: Integer);
begin
  FMarkerSpreadDelayMS := Value;
  if not FSpreadModeActive then
    FMouseInactivity.InactivityTimeOutMS := FMarkerSpreadDelayMS;
end;

procedure TMapViewSpreadMarkerHelper.SetMarkerCollapseDelayMS(Value: Integer);
begin
  FMarkerCollapseDelayMS := Value;
  if not FSpreadModeActive then
    FMouseInactivity.InactivityTimeOutMS := FMarkerCollapseDelayMS;
end;

procedure TMapViewSpreadMarkerHelper.OnMouseInactive(Sender: TObject);
begin
  if FSpreadModeActive then
    LeaveSpreadMode
  else
    EnterSpreadMode;
end;

procedure TMapViewSpreadMarkerHelper.RecalculatePositions;
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
begin
  if not FSpreadModeActive then Exit;

  // The markers are distributed into circles
  // The smallest circle will contain 6 items, the next bigger one 12, ...
  // If a circle can hold more items than available, the items are distributed
  // with a bigger angle
  ndx := 0; // start with the first item
  circlendx := 1; // start with the innerst circle
  while ndx < Length(FSpreadMarkerArr) do
  begin // loop until no more items are available
    // calculate how many items will fit to this circle
    itemspercircle := Trunc(circlendx * Pi() * 2);
    // calculate how many items are availabale
    remaincnt := Length(FSpreadMarkerArr) - ndx;
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
      FSpreadMarkerArr[ndx].OfsCenterX := pt.X;
      FSpreadMarkerArr[ndx].OfsCenterY := pt.Y;
      pt.X := pt.X + FLastMouseX;
      pt.Y := pt.Y + FLastMouseY;
      rPoint := FMapView.ScreenToLatLon(pt);
      // Now move the item to the new, temporary position
      FSpreadMarkerArr[ndx].GPSPoint.MoveTo(
        rPoint.Lon,
        rPoint.Lat,
        FSpreadMarkerArr[ndx].GPSPoint.Elevation,
        FSpreadMarkerArr[ndx].GPSPoint.DateTime);
      // next item
      Inc(Ndx);
    end;
    // next circle
    Inc(circlendx);
  end;
end;

procedure TMapViewSpreadMarkerHelper.EnterSpreadMode;
var
  gpsList: TGpsObjList;
  i : Integer;
  pt : TPoint;
  cs2 : Integer;
  aArea : TRealArea;
  cnt : Integer;
  ndx : Integer;
  lGpsPt : TGpsPoint;
begin
  if FSpreadModeActive then Exit;
  // First setup the area to be checked.
  cs2 := FMarkerCatchSize div 2;
  if cs2 < 1 then
    cs2 := 1;
  pt.X := FLastMouseX-cs2;
  pt.Y := FLastMouseY-cs2;
  aArea.TopLeft := FMapView.ScreenToLatLon(pt);
  pt.X := FLastMouseX+cs2;
  pt.Y := FLastMouseY+cs2;
  aArea.BottomRight := FMapView.ScreenToLatLon(pt);
  // Return all items (which belongs to this "layer") on the screen in one list
  gpsList := FMapLayer.GetObjectsInArea(aArea);
  try
    ndx := 0;
    cnt := gpsList.Count;
    // Do not enter SpreadMode if less than two items are found in the area
    if cnt < 2 then Exit;
    // Now fetch the items into the array
    // Ask the user wheter he want a specific item to be spreaded
    SetLength(FSpreadMarkerArr,cnt);
    for i:=0 to gpsList.Count-1 do
    begin
      if (gpsList[i] is TGpsPoint)  then
      begin
        lGpsPt := TGpsPoint(gpsList[i]);
        if (not Assigned(FSpreadMarkerAcceptEvent)) or
           FSpreadMarkerAcceptEvent(Self,lGpsPt) then
        begin
          FSpreadMarkerArr[ndx].GPSPoint := lGpsPt;
          FSpreadMarkerArr[ndx].OrgRealPt := lGpsPt.RealPoint;
          Inc(ndx);
        end;
      end;
    end;
    // If the user denied the usage of one or more items, adjust the length of the array
    if Length(FSpreadMarkerArr) <> ndx then
      SetLength(FSpreadMarkerArr,ndx);
  finally
    gpsList.Free;
  end;
  // Do not enter SpreadMode if less than two items are found in the area
  if Length(FSpreadMarkerArr) <= 1 then Exit;
  // Switch to spread mode
  FSpreadModeActive := True;

  // (Re-) calculate positions
  RecalculatePositions;

  // setup the time to the second (collapse) time
  FMouseInactivity.InactivityTimeOutMS := FMarkerCollapseDelayMS;
  // Trigger the timer, to allow the collapse even if the mouse don't move
  FMouseInactivity.AliveTrigger;
  // notify the main program
  if Assigned(FSpreadModeEnteredEvent) then
    FSpreadModeEnteredEvent(Self);
end;

procedure TMapViewSpreadMarkerHelper.LeaveSpreadMode;
begin
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
end;

function TMapViewSpreadMarkerHelper.IndexOfMarker(Item: TGPSPoint): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to High(FSpreadMarkerArr) do
    if FSpreadMarkerArr[i].GPSPoint = Item then
      Exit(i);
end;

procedure TMapViewSpreadMarkerHelper.MapViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // (Re-)Activate the timer
  if FMouseInactivity.Active then
    FMouseInactivity.AliveTrigger // Trigger
  else if (not FMouseButtonIsDown) then
    FMouseInactivity.Active := True; // Activate (will set the timer)
  // Store the last Mouse position for later use
  FLastMouseX := X;
  FLastMouseY := Y;
end;

procedure TMapViewSpreadMarkerHelper.MapViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Store the last Mouse position for later use
  FLastMouseX := X;
  FLastMouseY := Y;

  if FSpreadModeActive then
    LeaveSpreadMode
  else
    EnterSpreadMode;
  FMouseButtonIsDown := True;
end;

procedure TMapViewSpreadMarkerHelper.MapViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Store the last Mouse position for later use
  FLastMouseX := X;
  FLastMouseY := Y;

  FMouseButtonIsDown := False;
end;

procedure TMapViewSpreadMarkerHelper.MapViewZoomChange(Sender: TObject);
begin
  // if the zoom of the map changes during the spread mode is active
  // leave the spread mode and enter immediatly again to update the
  // spreaded markers in the area
  if FSpreadModeActive then
  begin
    LeaveSpreadMode;
    EnterSpreadMode;
  end;
end;

procedure TMapViewSpreadMarkerHelper.Reset(
  const AReturnMarkerToOrgPosition: Boolean);
var
  i : Integer;
begin
  if AReturnMarkerToOrgPosition and FSpreadModeActive then
  begin
    // Return all markers to their original positions
    for i := 0 to High(FSpreadMarkerArr) do
      FSpreadMarkerArr[i].GPSPoint.MoveTo(
        FSpreadMarkerArr[i].OrgRealPt.Lon,
        FSpreadMarkerArr[i].OrgRealPt.Lat,
        FSpreadMarkerArr[i].GPSPoint.Elevation,
        FSpreadMarkerArr[i].GPSPoint.DateTime);
  end;
  SetLength(FSpreadMarkerArr,0);
  FSpreadModeActive := False;
end;

constructor TMapViewSpreadMarkerHelper.Create(const AMapView: TMapView;
  const AMapLayer: TGPSObjectList);
begin
  inherited Create;
  FMapView := AMapView;
  if not Assigned(AMapLayer) then
    FMapLayer := FMapView.GPSLayer[0]
  else
    FMapLayer := AMapLayer;
  FMarkerCatchSize := DefaultMarkerCatchSize;
  FSpreadByPixel := DefaultSpreadByPixel;
  FMarkerSpreadDelayMS := DefaultMarkerSpreadDelayMS;
  FMarkerCollapseDelayMS := DefaultMarkerCollapseDelayMS;
  FMouseInactivity := TInactivityAlarmTimer.Create;
  FMouseInactivity.InactivityAlarmQuitKind := iaqAutoActivity;
  FMouseInactivity.InactivityTimeOutMS := FMarkerSpreadDelayMS;
  FMouseInactivity.ResponseGranularityMS := 10;
  FMouseInactivity.InactivityAlarmDetectedEvent := @OnMouseInactive;
end;

destructor TMapViewSpreadMarkerHelper.Destroy;
begin
  FMouseInactivity.Active := False;
  Reset(False);
  FMouseInactivity.Free;
  inherited Destroy;
end;

end.

