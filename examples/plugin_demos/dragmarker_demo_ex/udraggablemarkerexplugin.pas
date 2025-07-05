unit udraggablemarkerexplugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  mvMapViewer, mvPluginCommon, mvPlugins, mvGPSObj, mvTypes;
type
  { TDraggableMarkerExPlugin }
  TDraggableMarkerExPlugin = class;
  TDraggableMarkerCanMoveEvent = function (Sender : TDraggableMarkerExPlugin; AMarker : TGPSPoint) : Boolean of object;
  TDraggableMarkerMovedEvent = procedure (Sender : TDraggableMarkerExPlugin; AMarker : TGPSPoint; AOrgPosition : TRealPoint) of object;
  TDraggableMarkerAllCanMoveEvent = function (Sender : TDraggableMarkerExPlugin; AGenericMarker : TObject; var AMarkerHandled : Boolean) : Boolean of object;
  TDraggableMarkerAllMovedEvent = procedure (Sender : TDraggableMarkerExPlugin; AGenericMarker : TObject; AOrgPosition : TRealPoint) of object;
  TDraggableMarkerMapPointCanMoveEvent = function (Sender : TDraggableMarkerExPlugin; AMarker : TMapPoint) : Boolean of object;
  TDraggableMarkerMapPointMovedEvent = procedure (Sender : TDraggableMarkerExPlugin; AMarker : TMapPoint; AOrgPosition : TRealPoint) of object;

  { TDraggableMarkerMode defines the operation mode of the plugin.
    dmmGPSPoint (Default): Backward compatible, supports only TGPSPoint Markers
    dmmMapPoint: Supports only TMapPoint Marlers
    dmmAllPoints: Supports all Markers (TGPSPoint and TMapPoint)
  }
  TDraggableMarkerMode = (dmmGPSPoints,dmmMapPoints,dmmAllPoints);
const
  DefaultDraggableMarkerMode = dmmGPSPoints;
type

  { TDraggableMarkerExData }
  PDraggableMarkerData = ^TDraggableMarkerData;
  TDraggableMarkerData = record
    FDraggedMarker : TObject;
    FOrgPosition : TRealPoint;
  end;

  TDraggableMarkerExPlugin = class(TMvMultiMapsPlugin)
  private
    const
      DEFAULT_TOLERANCE = 5;
  private
    FDraggableMarkerCanMoveEvent : TDraggableMarkerCanMoveEvent;
    FDraggableMarkerMovedEvent : TDraggableMarkerMovedEvent;
    FDraggableMarkerAllCanMoveEvent : TDraggableMarkerAllCanMoveEvent;
    FDraggableMarkerAllMovedEvent : TDraggableMarkerAllMovedEvent;
    FDraggableMarkerMapPointCanMoveEvent : TDraggableMarkerMapPointCanMoveEvent;
    FDraggableMarkerMapPointMovedEvent : TDraggableMarkerMapPointMovedEvent;
    FDraggableMarkerMode : TDraggableMarkerMode;
    FDragMouseButton: TMouseButton;
    FTolerance: Integer;
    function GetFirstMarkerAtMousePos(const AMapView: TMapView; const AX, AY : Integer) : TObject;
    function GetDraggedMarker(AMapView : TMapView) : TGPSPoint;
    function GetDraggedMarkerGeneric(AMapView : TMapView) : TObject;
    function GetDraggedMarkerNew(AMapView : TMapView) : TMapPoint;
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
    property DraggedMarkerAll[AMapView : TMapView] : TObject read GetDraggedMarkerGeneric;
    property DraggedMarkerNew[AMapView : TMapView] : TMapPoint read GetDraggedMarkerNew;
    property OrgPosition[AMapView : TMapView] : TRealPoint read GetOrgPosition;
  published
    property DraggableMarkerCanMoveEvent : TDraggableMarkerCanMoveEvent read FDraggableMarkerCanMoveEvent write FDraggableMarkerCanMoveEvent;
    property DraggableMarkerMovedEvent : TDraggableMarkerMovedEvent read FDraggableMarkerMovedEvent write FDraggableMarkerMovedEvent;
    property DraggableMarkerAllCanMoveEvent : TDraggableMarkerAllCanMoveEvent read FDraggableMarkerAllCanMoveEvent write FDraggableMarkerAllCanMoveEvent;
    property DraggableMarkerAllMovedEvent : TDraggableMarkerAllMovedEvent read FDraggableMarkerAllMovedEvent write FDraggableMarkerAllMovedEvent;
    property DraggableMarkerMapPointCanMoveEvent : TDraggableMarkerMapPointCanMoveEvent read FDraggableMarkerMapPointCanMoveEvent write FDraggableMarkerMapPointCanMoveEvent;
    property DraggableMarkerMapPointMovedEvent : TDraggableMarkerMapPointMovedEvent read FDraggableMarkerMapPointMovedEvent write FDraggableMarkerMapPointMovedEvent;
    property DraggableMarkerMode : TDraggableMarkerMode read FDraggableMarkerMode write FDraggableMarkerMode default DefaultDraggableMarkerMode;
    property DragMouseButton : TMouseButton read FDragMouseButton write FDragMouseButton default mbLeft;
    property Tolerance: Integer read FTolerance write FTolerance default DEFAULT_TOLERANCE;
  end;


implementation

{ TDraggableMarkerExPlugin }

constructor TDraggableMarkerExPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FTolerance := DEFAULT_TOLERANCE;
end;

procedure TDraggableMarkerExPlugin.Assign(Source: TPersistent);
begin
  if Source is TDraggableMarkerExPlugin then
  begin
    FDraggableMarkerCanMoveEvent := TDraggableMarkerExPlugin(Source).DraggableMarkerCanMoveEvent;
    FDraggableMarkerMovedEvent := TDraggableMarkerExPlugin(Source).DraggableMarkerMovedEvent;

    FDraggableMarkerAllCanMoveEvent := TDraggableMarkerExPlugin(Source).DraggableMarkerAllCanMoveEvent;
    FDraggableMarkerAllMovedEvent := TDraggableMarkerExPlugin(Source).DraggableMarkerAllMovedEvent;
    FDraggableMarkerMapPointCanMoveEvent := TDraggableMarkerExPlugin(Source).DraggableMarkerMapPointCanMoveEvent;
    FDraggableMarkerMapPointMovedEvent := TDraggableMarkerExPlugin(Source).DraggableMarkerMapPointMovedEvent;

    FDraggableMarkerMode := TDraggableMarkerExPlugin(Source).DraggableMarkerMode;

    FDragMouseButton := TDraggableMarkerExPlugin(Source).DragMouseButton;
    FTolerance := TDraggableMarkerExPlugin(Source).Tolerance;
  end;
  inherited;
end;

function TDraggableMarkerExPlugin.GetFirstMarkerAtMousePos(
  const AMapView: TMapView; const AX, AY: Integer): TObject;
var
  aArea : TRealArea;
  lGpsPt : TGpsPoint;
  gpsList: TGpsObjList;
  lMapPt : TMapPoint;
  mapObjList : TMapObjectList;
  lMarkerHandled : Boolean;
  lMarkerCanMove : Boolean;
  i : Integer;
begin
  Result := Nil;
  aArea.TopLeft := AMapView.ScreenToLatLon(Point(AX - FTolerance, AY - FTolerance));
  aArea.BottomRight := AMapView.ScreenToLatLon(Point(AX + FTolerance, AY + FTolerance));
  if (FDraggableMarkerMode = dmmGPSPoints) or
     (FDraggableMarkerMode = dmmAllPoints) then
  begin
    gpsList := AMapView.GPSItems.GetObjectsInArea(aArea);
    try
      for i := gpsList.Count-1 downto 0 do
      begin
        if (gpsList[i] is TGpsPoint)  then
        begin
          lGpsPt := TGpsPoint(gpsList[i]);
          lMarkerHandled := False;
          lMarkerCanMove := True;
          // Try the Generic Event first
          if Assigned(FDraggableMarkerAllCanMoveEvent) then
             lMarkerCanMove := FDraggableMarkerAllCanMoveEvent(Self,lGpsPt,lMarkerHandled);
          // Marker handled and canMove do so, or ask the User
          if (lMarkerHandled and lMarkerCanMove) or
             (not Assigned(FDraggableMarkerCanMoveEvent)) or
             FDraggableMarkerCanMoveEvent(Self,lGpsPt) then
          begin
            Result := lGpsPt;
            Exit;
          end;
        end;
      end;
    finally
      gpsList.Free;
    end;
  end;
  if (FDraggableMarkerMode = dmmMapPoints) or
     (FDraggableMarkerMode = dmmAllPoints) then
  begin
    mapObjList := AMapView.Layers.HitTest(aArea);
    if Assigned(mapObjList) then
    try
      for i := mapObjList.Count-1 downto 0 do
      begin
        if (mapObjList[i] is TMapPoint)  then
        begin
          lMapPt := TMapPoint(mapObjList[i]);
          lMarkerCanMove := True;
          // Try the Generic Event first
          if Assigned(FDraggableMarkerAllCanMoveEvent) then
             lMarkerCanMove := FDraggableMarkerAllCanMoveEvent(Self,lMapPt,lMarkerHandled);
          // Marker handled and canMove do so, or ask the User
          if (lMarkerHandled and lMarkerCanMove) or
             (not Assigned(FDraggableMarkerMapPointCanMoveEvent)) or
             FDraggableMarkerMapPointCanMoveEvent(Self,lMapPt) then
          begin
            Result := lMapPt;
            Exit;
          end;
        end;
      end;
    finally
      if Assigned(mapObjList) then
        mapObjList.Free;
    end;
  end;
end;

function TDraggableMarkerExPlugin.GetDraggedMarker(AMapView: TMapView): TGPSPoint;
var
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  Result := Nil;
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) and
     (lDraggableMarkerData.FDraggedMarker is TGPSPoint) then
    Result := TGPSPoint(lDraggableMarkerData.FDraggedMarker);
end;

function TDraggableMarkerExPlugin.GetDraggedMarkerGeneric(AMapView: TMapView
  ): TObject;
var
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  Result := Nil;
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) then
    Result := lDraggableMarkerData.FDraggedMarker;
end;

function TDraggableMarkerExPlugin.GetDraggedMarkerNew(AMapView: TMapView
  ): TMapPoint;
var
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  Result := Nil;
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) and
     (lDraggableMarkerData.FDraggedMarker is TMapPoint) then
    Result := TMapPoint(lDraggableMarkerData.FDraggedMarker);
end;

function TDraggableMarkerExPlugin.GetOrgPosition(AMapView : TMapView): TRealPoint;
var
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  Result.InitXY(0.0,0.0);
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) then
    Result := lDraggableMarkerData.FOrgPosition;
end;

procedure TDraggableMarkerExPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
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
    if lDraggableMarkerData.FDraggedMarker is TGPSPoint then
    begin
      lDraggableMarkerData.FOrgPosition.Lon:= TGPSPoint(lDraggableMarkerData.FDraggedMarker).Lon;
      lDraggableMarkerData.FOrgPosition.Lat:= TGPSPoint(lDraggableMarkerData.FDraggedMarker).Lat;
    end
    else if lDraggableMarkerData.FDraggedMarker is TMapPoint then
    begin
      lDraggableMarkerData.FOrgPosition.Lon:= TMapPoint(lDraggableMarkerData.FDraggedMarker).Longitude;
      lDraggableMarkerData.FOrgPosition.Lat:= TMapPoint(lDraggableMarkerData.FDraggedMarker).Latitude;
    end
    else
      Exit;
    SetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
    Handled := True;
  end;
end;

procedure TDraggableMarkerExPlugin.MouseMove(AMapView: TMapView;
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
    if lDraggableMarkerData.FDraggedMarker is TGPSPoint then
    begin
      ele := TGPSPoint(lDraggableMarkerData.FDraggedMarker).Elevation;
      dt := TGPSPoint(lDraggableMarkerData.FDraggedMarker).DateTime;
      TGPSPoint(lDraggableMarkerData.FDraggedMarker).MoveTo(rpt.Lon, rpt.Lat,ele,dt);
    end
    else if lDraggableMarkerData.FDraggedMarker is TMapPoint then
      TMapPoint(lDraggableMarkerData.FDraggedMarker).RealPoint := rpt
    else
      Exit;
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

procedure TDraggableMarkerExPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  lpDraggableMarkerData : PDraggableMarkerData;
begin
  if not MapViewEnabled[AMapView] then Exit;
  if FDragMouseButton <> Button then Exit;
  lpDraggableMarkerData := MapViewDataPtr[AMapView];
  if Assigned(lpDraggableMarkerData) and Assigned(lpDraggableMarkerData^.FDraggedMarker) then
  begin
    case FDraggableMarkerMode of
      dmmGPSPoints :
        if Assigned(FDraggableMarkerMovedEvent) and
           (lpDraggableMarkerData^.FDraggedMarker is TGPSPoint) then
          FDraggableMarkerMovedEvent(Self,TGPSPoint(lpDraggableMarkerData^.FDraggedMarker),lpDraggableMarkerData^.FOrgPosition);
      dmmMapPoints :
        if Assigned(FDraggableMarkerMapPointMovedEvent) and
           (lpDraggableMarkerData^.FDraggedMarker is TMapPoint) then
          FDraggableMarkerMapPointMovedEvent(Self,TMapPoint(lpDraggableMarkerData^.FDraggedMarker),lpDraggableMarkerData^.FOrgPosition);
      dmmAllPoints :
        if Assigned(FDraggableMarkerAllMovedEvent) then
          FDraggableMarkerAllMovedEvent(Self,lpDraggableMarkerData^.FDraggedMarker,lpDraggableMarkerData^.FOrgPosition);
    end;
    Handled := True;
    lpDraggableMarkerData^.FDraggedMarker := Nil;
  end;
end;



end.

