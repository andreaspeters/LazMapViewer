{
 mvAreaSelectionPlugin

 Copyright (C) 2025 Ekkehard Domning (www.domis.de)

 License: modified LGPL with linking exception (like RTL, FCL and LCL)

 See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
 for details about the license.

 This plugin allows the user to select an area on the map.
 
 The Plugin can be used only on *one* MapView, so it is mandatory to assign a
 MapView to the MapView property.
 
 The plugin is able to deal with the Cyclic property of the MapView. This allows
 the selection of "WrapAround"-Areas, which are changing the date border.
 Those areas have the property that the BottomRight.Lon is *smaller* than the 
 TopLeft.Lon!
 
 Example:
 The Area
   TopLeft.Lon     = 13.4 (Berlin)
   BottomRight.Lon = 0.0 (London)
 wraps around the Date-Border, while the Area
    TopLeft.Lon     = 0.0 (London)
    BottomRight.Lon = 13.4 (Berlin)
 does not!
 
 Two events are implemented. One is called if the selection of a new area is in
 progress, the other when the selection is finished.
 
 The user can change the rectangle by dragging some anchors with the mouse.
 
 Those anchors are the 4 lines, the 4 corners and a small area around the top line.
 The method of changing is indicated by the mouse pointer.
 
 Assigning a new SelectedArea while changing is not supported
}
unit mvAreaSelectionPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Types, Math, Graphics, Contnrs,
  mvPluginCommon, mvPlugins, mvMapViewer, mvTypes, mvGeoMath, mvDrawingEngine;

type
  { class TMouseHitItem
    A helper class to hold a single detectable Item, Like corner, edge, etc.
    Not intended to be used outside this unit.
  }
  TMouseHitItem = class(TObject)
  private
    FTag : Integer;
    FZOrder : Integer;
    FActiveRect : TRect;
    FCursor : TCursor;
    FItemHit : Boolean;
    FMouseDownFlag : Boolean;
    FOrgX, FOrgY : Integer;
    FMouseDownX, FMouseDownY : Integer;
    FMouseDeltaX, FMouseDeltaY : Integer;
  public
    constructor Create;
    constructor Create(const AActiveRect : TRect;
                       const AOrgX : Integer;
                       const AOrgY : Integer;
                       const AZOrder : Integer = 0;
                       const ACursor : TCursor = crSize;
                       const ATag : Integer = 0);
    procedure OnMouseDown(Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Shift: TShiftState; X, Y: Integer);

    property Tag : Integer read FTag write FTag;
    property ZOrder : Integer read FZOrder write FZOrder;
    property ActiveRect : TRect read FActiveRect write FActiveRect;
    property Cursor : TCursor read FCursor  write FCursor;
    property OrgX : Integer read FOrgX;
    property OrgY : Integer read FOrgY;
    property ItemHit : Boolean read FItemHit;
    property MouseDownFlag : Boolean read FMouseDownFlag write FMouseDownFlag;
    property MouseDownX : Integer read FMouseDownX;
    property MouseDownY : Integer read FMouseDownY;
    property MouseDeltaX : Integer read FMouseDeltaX;
    property MouseDeltaY : Integer read FMouseDeltaY;
  end;

type
  TSelectedAreaChangingEvent = procedure (Sender : TObject; ANewArea : TRealArea; var Allow : Boolean) of Object;
  TAreaSelectionCaptionPosition = (ascpLeftTop, ascpCenterTop, ascpRightTop, ascpCenter, ascpLeftBottom,
                                   ascpCenterBottom, ascpRightBottom);
                                   
  { TAreaSelectionPlugin }
  TAreaSelectionPlugin = class(TMvDrawPlugin)
  private
    const
      DEFAULT_PEN_COLOR = clNavy;
      DEFAULT_PEN_STYLE = psSolid;
      DEFAULT_PEN_WIDTH = 3;
      DEFAULT_SENSITIVE_AREA_INFLATION = 2; // this increases the pixels of the sensitive items
  private
    FMouseHitItems : TObjectList; // The list with the clickable items
    FMouseButton : TMouseButton;
    FShifterXInverseMode : Boolean; // Two Flags to ease the user interface for the flat and cylindrical world
    FShifterYInverseMode : Boolean;
    FSelectedArea: TMapRealArea;
    FLastMouseMoveHandled : Boolean;
    FAreaInflation : Integer;
    FMouseMapCoords : TRealPoint;
    FSelectedAreaHitEvent : TNotifyEvent;
    FSelectedAreaBeginChangeEvent : TNotifyEvent;
    FSelectedAreaChangedEvent : TNotifyEvent;
    FSelectedAreaChangingEvent : TSelectedAreaChangingEvent;
    FCaption : String;
    FCaptionPosition : TAreaSelectionCaptionPosition;
    function GetCurrentItem : TMouseHitItem;
    function GetItemsCount: Integer;
    function GetItems(AIndex : Integer) : TMouseHitItem;
    procedure SetCaption(Value : String);
    procedure SetCaptionPosition(Value : TAreaSelectionCaptionPosition);
    procedure SetMouseButton(AValue: TMouseButton);
    procedure SetSensitiveAreaInflation(Value : Integer);
    procedure SetSelectedArea(Value: TMapRealArea);
  protected
    procedure AddSelectionArea(const ARect: TRect; const AInflate: Integer);
    procedure AddSelectionAreaEx(const ARect: TRect; const ARectParts: array of Integer;
      const AInflate: Integer);
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer; var Handled: Boolean); override;
    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); override;
    procedure Resize(AMapView: TMapView; var Handled: Boolean); override;

    property ItemsCount : Integer read GetItemsCount;
    property Items[AIndex : Integer] : TMouseHitItem read GetItems;
    { property CurrentItem returns either the Item where the Mouse is down or the one where the mouse is hovering above }
    property CurrentItem : TMouseHitItem read GetCurrentItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    { procedure Clear the list of all items. Usually called in MouseUp and prior the rebuilding of a new setup }
    procedure Clear;
    { procedure SetupRectShifter creates the mouse shifting items, must be called if the display changes
      needed e.g.if the size of the MapView is altered}
    procedure SetupRectShifter;
  published
    property Caption : String read FCaption write SetCaption;
    property CaptionPosition : TAreaSelectionCaptionPosition read FCaptionPosition write SetCaptionPosition;
    property MouseButton: TMouseButton read FMouseButton write SetMouseButton default mbLeft;
    property SelectedArea: TMapRealArea read FSelectedArea write SetSelectedArea;
    property SensitiveAreaInflation: Integer read FAreaInflation write SetSensitiveAreaInflation default DEFAULT_SENSITIVE_AREA_INFLATION;
    property OnSelectedAreaHit : TNotifyEvent read FSelectedAreaHitEvent write FSelectedAreaHitEvent;
    property OnSelectedAreaBeginChange : TNotifyEvent read FSelectedAreaBeginChangeEvent write FSelectedAreaBeginChangeEvent;
    property OnSelectedAreaChanged: TNotifyEvent read FSelectedAreaChangedEvent write FSelectedAreaChangedEvent;
    property OnSelectedAreaChanging: TSelectedAreaChangingEvent read FSelectedAreaChangingEvent write FSelectedAreaChangingEvent;
    property BackgroundColor default clNone;
    //property BackgroundOpacity; // cannot be used because we paint in the canvas rather than the drawing engine
    property Font;
    property Pen;
  end;

implementation

const
  tagRectArea        = 1;
  tagRectLeft        = 2;
  tagRectTop         = 3;
  tagRectRight       = 4;
  tagRectBottom      = 5;
  tagRectTopLeft     = 6;
  tagRectTopRight    = 7;
  tagRectBottomRight = 8;
  tagRectBottomLeft  = 9;

{function RealAreaWidth calculates the width of an area on earth.
   if the BottomRight.Lon is smaller than the TopLeft.Lon, the
   area is treated as wrapping around the date border}
function RealAreaWidth(const ARealArea : TRealArea) : Double;
begin
  Result := ARealArea.BottomRight.Lon-ARealArea.TopLeft.Lon;
  if ARealArea.BottomRight.Lon < ARealArea.TopLeft.Lon then
    Result := Result + 360.0;
end;

procedure RealAreaNormed(var ARealArea : TRealArea);
begin
  ARealArea.TopLeft.Lat := EnsureRange(ARealArea.TopLeft.Lat, -90, 90);
  ARealArea.TopLeft.Lon := EnsureRange(ARealArea.TopLeft.Lon, -180, 180);
  ARealArea.BottomRight.Lat := EnsureRange(ARealArea.BottomRight.Lat, -90, 90);
  ARealArea.BottomRight.Lon := EnsureRange(ArealArea.BottomRight.Lon, -180, 180);
  {
  if ARealArea.TopLeft.Lat > 90.0 then
    ARealArea.TopLeft.Lat := 90.0
  else if ARealArea.TopLeft.Lat < -90.0 then
    ARealArea.TopLeft.Lat := -90.0;
  if ARealArea.TopLeft.Lon < -180.0 then
    ARealArea.TopLeft.Lon := -180.0
  else if ARealArea.TopLeft.Lon > 180.0 then
    ARealArea.TopLeft.Lon := 180.0;
  if ARealArea.BottomRight.Lat > 90.0 then
    ARealArea.BottomRight.Lat := 90.0
  else if ARealArea.BottomRight.Lat < -90.0 then
    ARealArea.BottomRight.Lat := -90.0;
  if ARealArea.BottomRight.Lon < -180.0 then
    ARealArea.BottomRight.Lon := -180.0
  else if ARealArea.BottomRight.Lon > 180.0 then
    ARealArea.BottomRight.Lon := 180.0;
    }
end;

{ function IsInRectangle returns true, if the passed X and Y coords are inside the rectangle
   Parameter:
     ARect : The rectangle to test
     AX, AY : The coords to test
     AInflate : The amount of inflating the rectangle
   Remarks: The function works different from the definition in uLazQuadTreeGeometry.
     The rectangle here is treated having the right and bottom border as part of the
     rectangle! This is needed, because the right and bottom border must be hit for
     resizing.
}
function IsInRectangle(const ARect : TRect; const AX, AY : Integer; const AInflate : Integer = 0): Boolean;
var
  R: TRect;
begin
  R := ARect;
  if AInflate > 0 then
    InflateRect(R, AInflate, AInflate);
  Result := PtInRect(R, Point(AX, AY));
end;

{ TMouseHitItem }

procedure TMouseHitItem.OnMouseDown(Shift: TShiftState; X,
  Y: Integer);
begin
  Unused(Shift);
  if IsInRectangle(FActiveRect, X, Y, 0) then
  begin
    FMouseDownFlag := True;
    FMouseDeltaX := 0;
    FMouseDeltaY := 0;
    FMouseDownX := X;
    FMouseDownY := Y;
  end;
end;

procedure TMouseHitItem.OnMouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  Unused(Shift);
  if FMouseDownFlag then
  begin
    FMouseDeltaX := X-FMouseDownX;
    FMouseDeltaY := Y-FMouseDownY;
  end
  else
  begin
    FMouseDeltaX := 0;
    FMouseDeltaY := 0;
    FItemHit := IsInRectangle(FActiveRect, X, Y, 0);
  end;
end;

procedure TMouseHitItem.OnMouseUp(Shift: TShiftState; X,
  Y: Integer);
begin
  Unused(Shift);
  if FMouseDownFlag then
  begin
    FMouseDeltaX := X-FMouseDownX;
    FMouseDeltaY := Y-FMouseDownY;
    FMouseDownFlag := False;
  end
  else
  begin
    FMouseDeltaX := 0;
    FMouseDeltaY := 0;
  end;
end;

constructor TMouseHitItem.Create;
begin
  inherited Create;
end;

constructor TMouseHitItem.Create(const AActiveRect: TRect;
  const AOrgX: Integer; const AOrgY: Integer; const AZOrder: Integer;
  const ACursor: TCursor; const ATag: Integer);
begin
  Create();
  FActiveRect := AActiveRect;
  FOrgX :=  AOrgX;
  FOrgY :=  AOrgY;
  FZOrder :=  AZOrder;
  FCursor :=  ACursor;
  FTag :=  ATag;
end;


{ TAreaSelectionPlugin }

constructor TAreaSelectionPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FMouseHitItems := TObjectList.Create(True);

  FSelectedArea := TMapRealArea.Create(Self);
  FSelectedArea.Area.Init(-100,50,100,-50);
  FSelectedArea.OnChange := @Changed;

  Pen.Color := DEFAULT_PEN_COLOR;
  Pen.Width := DEFAULT_PEN_WIDTH;
  Pen.Style := DEFAULT_PEN_STYLE;
  BackgroundColor := clNone;

  FAreaInflation := DEFAULT_SENSITIVE_AREA_INFLATION;
end;

destructor TAreaSelectionPlugin.Destroy;
begin
  FreeAndNil(FMouseHitItems);
  inherited;
end;

function TAreaSelectionPlugin.GetCurrentItem: TMouseHitItem;
var
  i : Integer;
  lItem : TMouseHitItem;
begin
  Result := Nil;
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    if lItem.FMouseDownFlag then
    begin
      if (not Assigned(Result)) or
         (Result.ZOrder < lItem.ZOrder) then
        Result := lItem;
    end;
  end;
  if Assigned(Result) then Exit;
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    if lItem.ItemHit then
    begin
      if (not Assigned(Result)) or (Result.ZOrder < lItem.ZOrder) then
        Result := lItem;
    end;
  end;
end;

function TAreaSelectionPlugin.GetItemsCount: Integer;
begin
  Result := FMouseHitItems.Count;
end;

function TAreaSelectionPlugin.GetItems(AIndex: Integer): TMouseHitItem;
begin
  Result := TMouseHitItem(FMouseHitItems[AIndex]);
end;

procedure TAreaSelectionPlugin.SetCaption(Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Update;
  end;
end;

procedure TAreaSelectionPlugin.SetCaptionPosition(
  Value: TAreaSelectionCaptionPosition);
begin
  if FCaptionPosition <> Value then
  begin
    FCaptionPosition := Value;
    Update;
  end;
end;

procedure TAreaSelectionPlugin.SetMouseButton(AValue: TMouseButton);
begin
  if FMouseButton=AValue then Exit;
  FMouseButton:=AValue;
end;

procedure TAreaSelectionPlugin.SetSensitiveAreaInflation(Value: Integer);
begin
  if FAreaInflation <> Value then
  begin
    FAreaInflation := Value;
    if Assigned(MapView) then
      MapView.Invalidate;
  end;
end;

procedure TAreaSelectionPlugin.SetSelectedArea(Value: TMapRealArea);
begin
  if not FSelectedArea.Area.Equal(Value.Area) then
  begin
    FSelectedArea.Area := Value.Area;
    if not Assigned(MapView) then Exit;
    MapView.Invalidate;
    SetupRectShifter;
  end;
end;

procedure TAreaSelectionPlugin.Clear;
begin
  FMouseHitItems.Clear;
end;

procedure TAreaSelectionPlugin.AddSelectionArea(const ARect: TRect;
  const AInflate: Integer);
begin
  AddSelectionAreaEx(ARect,
                 [tagRectArea, tagRectLeft, tagRectTop, tagRectRight, tagRectBottom,
                  tagRectTopLeft, tagRectTopRight, tagRectBottomRight, tagRectBottomLeft],
                 AInflate);
end;

procedure TAreaSelectionPlugin.AddSelectionAreaEx(const ARect: TRect;
  const ARectParts: array of Integer; const AInflate: Integer);
var
  lMaxExtentRect : TRect;
  lRect : TRect;
  i : Integer;
  llr : TRect;
  lItem : TMouseHitItem = Nil;
begin
  lMaxExtentRect.Left := 0;
  lMaxExtentRect.Top := 0;
  lMaxExtentRect.Right := MapView.Width;
  lMaxExtentRect.Bottom := MapView.Height;
  lRect := ARect;
  if ARect.Left < lMaxExtentRect.Left then
    lRect.Left := lMaxExtentRect.Left
  else if ARect.Left > lMaxExtentRect.Right then
    lRect.Left := lMaxExtentRect.Right;
  if ARect.Right < lMaxExtentRect.Left then
    lRect.Right := lMaxExtentRect.Left
  else if ARect.Right > lMaxExtentRect.Right then
    lRect.Right := lMaxExtentRect.Right;
  if ARect.Top < lMaxExtentRect.Top then
    lRect.Top := lMaxExtentRect.Top
  else if ARect.Top > lMaxExtentRect.Bottom then
    lRect.Top := lMaxExtentRect.Bottom;
  if ARect.Bottom < lMaxExtentRect.Top then
    lRect.Bottom := lMaxExtentRect.Top
  else if ARect.Bottom > lMaxExtentRect.Bottom then
    lRect.Bottom := lMaxExtentRect.Bottom;

  for i := 0 to High(ARectParts) do
  begin
    case ARectParts[i] of
      tagRectArea :
        begin
          llr.Left:=lRect.Left;
          llr.Top:=lRect.Top-3*AInflate;
          llr.Right:=lRect.Right;
          llr.Bottom:=lRect.Top+3*AInflate;
          lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Top,0,crSize,
                                        tagRectArea);
        end;
      tagRectLeft :
        begin
          // Left Border
          if (ARect.Left > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Left < lMaxExtentRect.Right+(2*AInflate)) then
          begin
            llr.Left:=ARect.Left-AInflate;
            llr.Top:=lRect.Top;
            llr.Right:=ARect.Left+AInflate;
            llr.Bottom:=lRect.Bottom;
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Top,1,crSizeWE,
                                          tagRectLeft);
          end;
        end;
      tagRectTop :
        begin
          // Top Border
          if (ARect.Top > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Top < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left:=lRect.Left;
            llr.Top:=ARect.Top-AInflate;
            llr.Right:=lRect.Right;
            llr.Bottom:=ARect.Top+AInflate;
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Top,1,crSizeNS,
                                          tagRectTop);
          end;
        end;
      tagRectRight :
        begin
          // Right Border
          if (ARect.Right > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Right < lMaxExtentRect.Right+(2*AInflate)) then
          begin
            llr.Left:=ARect.Right-AInflate;
            llr.Top:=lRect.Top;
            llr.Right:=ARect.Right+AInflate;
            llr.Bottom:=lRect.Bottom;
            lItem := TMouseHitItem.Create(llr,ARect.Right,ARect.Top,1,crSizeWE,
                                          tagRectRight);
          end;
        end;
      tagRectBottom :
        begin
          // Bottom border
          if (ARect.Bottom > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Bottom < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left:=lRect.Left;
            llr.Top:=ARect.Bottom-AInflate;
            llr.Right:=lRect.Right;
            llr.Bottom:=ARect.Bottom+AInflate;
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Bottom,1,crSizeNS,
                                          tagRectBottom);
          end;
        end;
      tagRectTopLeft :
        begin
          // Northwest edge
          if (ARect.Left > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Left < lMaxExtentRect.Right+(2*AInflate)) and
             (ARect.Top > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Top < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left := ARect.Left;
            llr.Top := ARect.Top;
            llr.Right := ARect.Left;
            llr.Bottom := ARect.Top;
            llr.Inflate(AInflate,AInflate);
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Top,2,crSizeNWSE,
                                          tagRectTopLeft);
         end;
        end;
      tagRectTopRight :
        begin
          // NorthEast edge
          if (ARect.Right > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Right < lMaxExtentRect.Right+(2*AInflate)) and
             (ARect.Top > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Top < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left := ARect.Right;
            llr.Top := ARect.Top;
            llr.Right := ARect.Right;
            llr.Bottom := ARect.Top;
            llr.Inflate(AInflate,AInflate);
            lItem := TMouseHitItem.Create(llr,ARect.Right,ARect.Top,2,crSizeNESW,
                                          tagRectTopRight);
          end;
        end;
      tagRectBottomRight :
        begin
          // SouthEast edge
          if (ARect.Right > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Right < lMaxExtentRect.Right+(2*AInflate)) and
             (ARect.Bottom > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Bottom < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left := ARect.Right;
            llr.Top := ARect.Bottom;
            llr.Right := ARect.Right;
            llr.Bottom := ARect.Bottom;
            llr.Inflate(AInflate,AInflate);
            lItem := TMouseHitItem.Create(llr,ARect.Right,ARect.Bottom,2,crSizeNWSE,
                                          tagRectBottomRight);
         end;
        end;
      tagRectBottomLeft :
        begin
          // SouthWest edge
          if (ARect.Left > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Left < lMaxExtentRect.Right+(2*AInflate)) and
             (ARect.Bottom > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Bottom < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left := ARect.Left;
            llr.Top := ARect.Bottom;
            llr.Right := ARect.Left;
            llr.Bottom := ARect.Bottom;
            llr.Inflate(AInflate,AInflate);
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Bottom,2,crSizeNESW,
                                          tagRectBottomLeft);
          end;
        end;
    end;
    if Assigned(lItem) then
      FMouseHitItems.Add(lItem);
    lItem := Nil;
  end;
end;

procedure TAreaSelectionPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);

  procedure DrawRectangle(ARect : TRect);
  begin
    if Abs(ARect.Left-ARect.Right) < Pen.Width then
      Inc(ARect.Right,Pen.Width);
    if Abs(ARect.Top-ARect.Bottom) < Pen.Width then
      Inc(ARect.Bottom,Pen.Width);

    if ARect.Top > 0 then
      MapView.Canvas.Line(ARect.Left,ARect.Top,ARect.Right,ARect.Top);
    if ARect.Bottom < MapView.Height then
      MapView.Canvas.Line(ARect.Left,ARect.Bottom,ARect.Right,ARect.Bottom);
    if ARect.Left > 0 then
      MapView.Canvas.Line(ARect.Left,ARect.Top,ARect.Left,ARect.Bottom);
    if ARect.Right < MapView.Width then
      MapView.Canvas.Line(ARect.Right,ARect.Top,ARect.Right,ARect.Bottom);
  end;

  procedure PaintCaption(ARect : TRect);
  var
    oldFont : TFont;
    sz : TSize;
    dx,dy : Integer;
    w, h : Integer;
    pw : Integer;
  begin
    pw := Pen.Width;
    MapView.Canvas.Font.Assign(Font);
    sz := MapView.Canvas.TextExtent(FCaption);
    w := ARect.Right - ARect.Left;
    h := ARect.Bottom - ARect.Top;

    // Calculate left/top corner of total text
    dx := ARect.Left;
    case FCaptionPosition of
      ascpLeftTop, ascpLeftBottom :
        dx := dx + pw div 2 + 2 ;
      ascpCenterTop, ascpCenter, ascpCenterBottom :
        dx := dx + (w - sz.cx) div 2;
      ascpRightTop, ascpRightBottom:
        dx := dx + w - sz.cx - pw div 2 - 1;
    end;
    dy := ARect.Top;
    case FCaptionPosition of
      ascpLeftTop, ascpCenterTop, ascpRightTop :
        dy := dy + pw div 2 + 1;
      ascpCenter :
        dy := dy + (h - sz.cy) div 2;
      ascpLeftBottom, ascpCenterBottom, ascpRightBottom:
        dy := dy + h - sz.cy - pw div 2;
    end;
    AMapView.Canvas.TextOut(dx,dy,FCaption)
  end;

var
  r0 : TRect;
  mapw : Int64;
  topLeftPt, bottomRightPt : TPoint;
  rectW : Integer;
  lRect : TRect;
  ptArr : TPointArray;
  pt : TPoint;
  sl : TStringList;
  s : String;
begin
  Unused(AMapView, Handled);

  topLeftPt := MapView.LatLonToScreen(FSelectedArea.North, FSelectedArea.West);
  lRect.Top := topLeftPt.Y;
  lRect.Left := topLeftPt.X;
  bottomRightPt := MapView.LatLonToScreen(FSelectedArea.South, FSelectedArea.East);
  lRect.Bottom := bottomRightPt.Y;
  lRect.Right := bottomRightPt.X;
  mapw := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
  if lRect.Left > lRect.Right then
    lRect.Left := lRect.Left - mapw;
  rectW := lRect.Right-lRect.Left;

  if BackgroundColor = clNone then
    MapView.Canvas.Brush.Style := bsClear
  else
    MapView.Canvas.Brush.Color := ColorToRGB(BackgroundColor);
  MapView.Canvas.Pen.Assign(Pen);

  sl := TStringList.Create;
  try
    sl.Sorted := True;
    sl.Duplicates := dupIgnore;

    // Paint rectangle and caption, in the cyclic world multiple times
    ptArr := MapView.CyclicPointsOf(topLeftPt);
    for pt in ptArr do
    begin
      r0.Left := pt.X;
      // Avoid painting duplicate rectangles and captions
      s := IntToStr(r0.Left);
      if sl.IndexOf(s) >= 0 then Continue;
      sl.Add(s);

      r0.Top := lRect.Top;
      r0.Right := pt.X+rectW;
      r0.Bottom := lRect.Bottom;
      DrawRectangle(r0);
      if Length(FCaption) > 0 then
        PaintCaption(r0);
    end;

    // Catch the case of the missing left rectangle and caption,
    // but ignore the rectangle and captions already painted
    ptArr := MapView.CyclicPointsOf(bottomRightPt);
    for pt in ptArr do
    begin
      r0.Left := pt.X-rectW;
      // Avoid painting duplicate rectangles and captions
      s := IntToStr(r0.Left);
      if sl.IndexOf(s) >= 0 then Continue;
      sl.Add(s);

      r0.Top := lRect.Top;
      r0.Right := pt.X;
      r0.Bottom := lRect.Bottom;
      DrawRectangle(r0);
      if Length(FCaption) > 0 then
        PaintCaption(r0);
    end;
  finally
    sl.Free;
  end;
end;

procedure TAreaSelectionPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X, Y: Integer; var Handled: Boolean);

  // The following "PerformXXX" procedures, do the actual adjusting of the
  // side of the active rectangle.
  procedure PerformRightSide(const ANewX : Double; var ARect : TRealArea);
  begin
    if AMapView.Cyclic then
    begin
      ARect.BottomRight.Lon := ANewX;
      Exit;
    end;
    if FShifterXInverseMode then
    begin
      if (ANewX > ARect.BottomRight.Lon) then
        FShifterXInverseMode := False;
    end
    else
    begin
      if (ANewX < ARect.TopLeft.Lon) then
        FShifterXInverseMode := True;
    end;
    if FShifterXInverseMode then
      ARect.TopLeft.Lon := ANewX
    else
      ARect.BottomRight.Lon := ANewX;
  end;

  procedure PerformLeftSide(const ANewX : Double; var ARect : TRealArea);
  begin
    if AMapView.Cyclic then
    begin
      ARect.TopLeft.Lon := ANewX;
      Exit;
    end;
    if FShifterXInverseMode then
    begin
      if (ANewX < ARect.TopLeft.Lon) then
        FShifterXInverseMode := False;
    end
    else
    begin
      if (ANewX > ARect.BottomRight.Lon) then
        FShifterXInverseMode := True;
    end;
    if FShifterXInverseMode then
      ARect.BottomRight.Lon := ANewX
    else
      ARect.TopLeft.Lon := ANewX;
  end;

  procedure PerformTopSide(const ANewY : Double; var ARect : TRealArea);
  begin
    if FShifterYInverseMode then
    begin
      if (ANewY < ARect.TopLeft.Lat) then
        FShifterYInverseMode := False;
    end
    else
    begin
      if (ANewY > ARect.BottomRight.Lat) then
        FShifterYInverseMode := True;
    end;
    if FShifterYInverseMode then
      ARect.BottomRight.Lat := ANewY
    else
      ARect.TopLeft.Lat := ANewY;
  end;

  procedure PerformBottomSide(const ANewY : Double; var ARect : TRealArea);
  begin
    if FShifterYInverseMode then
    begin
      if (ANewY > ARect.BottomRight.Lat) then
        FShifterYInverseMode := False;
    end
    else
    begin
      if (ANewY < ARect.TopLeft.Lat) then
        FShifterYInverseMode := True;
    end;
    if FShifterYInverseMode then
      ARect.TopLeft.Lat := ANewY
    else
      ARect.BottomRight.Lat := ANewY;
  end;

var
  w, h : Double;
  lx,ly: Double;
  lRect0 : TRealArea;
  lHitItem : TMouseHitItem;
  ptR : TRealPoint;
  chgAllowed : Boolean;
  i : Integer;
  lItem : TMouseHitItem;
begin
  if Handled then Exit;
  
  // Check if the GlobalMouseDown Flag is down, but the current item not.
  // This means, that some other plugin catched the MouseDown, but not this one
  if (PluginManager.MouseButtonDown[AMapView] <> []) and
     Assigned(CurrentItem) and
     (not CurrentItem.FMouseDownFlag) then Exit;
     
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    lItem.OnMouseMove(AShift,X,Y);
  end;
  lHitItem := CurrentItem;
  Handled := Assigned(lHitItem);
  
  // Here we have to act carefully, since we should not change the Mouse Pointer
  // if we not had set him.
  if (not Handled) and FLastMouseMoveHandled then
    MapView.Cursor := crDefault;  // no hit, but hit previously, set the default cursor

  // fire the hit event if not hit before and not mouse down
  if Handled and (not FLastMouseMoveHandled) and
    (PluginManager.MouseButtonDown[AMapView] = []) and
    (not lHitItem.MouseDownFlag) and Assigned(FSelectedAreaHitEvent) then
    FSelectedAreaHitEvent(Self);
    
  FLastMouseMoveHandled := Handled;
  if not Handled then Exit;
  // lHitItem will be assigned if one item is selected.
  MapView.Cursor := lHitItem.Cursor;  // Set the cursor to the hit items kind
  if lHitItem.MouseDownFlag then // if the mouse is down
  begin
    lRect0 := FSelectedArea.Area;
    // Invert the axis to ease calculation
    lRect0.TopLeft.Lat := -lRect0.TopLeft.Lat;
    lRect0.BottomRight.Lat := -lRect0.BottomRight.Lat;
    FMouseMapCoords := MapView.ScreenToLatLon(Point(X,Y));
    ptR := MapView.ScreenToLatLon(Point(lHitItem.OrgX+lHitItem.MouseDeltaX,
                                        lHitItem.OrgY+lHitItem.MouseDeltaY));
    ptR.Lat := -ptR.Lat;
    lx := ptR.Lon;
    ly := ptR.Lat;
    // case of the several actions
    case (lHitItem.Tag and $F) of
      tagRectArea :
        begin
          // Special operation for the movement of the full rectangle.
          // Just locate the rectangle at the new position ...
          w := RealAreaWidth(lRect0);
          h := lRect0.BottomRight.Lat-lRect0.TopLeft.Lat;
          lRect0.TopLeft := ptR;
          lRect0.BottomRight.Lon := lRect0.TopLeft.Lon + w;
          lRect0.BottomRight.Lat := lRect0.TopLeft.Lat + h;
          // ... but disallow shifting beyond the borders
          if MapView.Cyclic then
          begin
            while lRect0.BottomRight.Lon >= 180.0 do
              lRect0.BottomRight.Lon := lRect0.BottomRight.Lon -360.0;
          end
          else
          begin
            // In the flat world model right and left are fixed borders
            if lRect0.BottomRight.Lon >= 180.0 then
              lRect0.TopLeft.Lon := 180.0 - w;
          end;
          // In the flat and cylindrical world model top and bottom are fixed borders
          lRect0.BottomRight.Lat := lRect0.TopLeft.Lat + h;
          if lRect0.BottomRight.Lat >= 90.0 then
            lRect0.TopLeft.Lat := 90.0 - h;
        end;
      tagRectLeft :
        begin
          PerformLeftSide(lx,lRect0);
        end;
      tagRectTop :
        begin
          PerformTopSide(ly,lRect0);
        end;
      tagRectRight :
        begin
          PerformRightSide(lx,lRect0);
        end;
      tagRectBottom :
        begin
          PerformBottomSide(ly,lRect0);
        end;
      tagRectTopLeft :
        begin
          PerformTopSide(ly,lRect0);
          PerformLeftSide(lx,lRect0);
        end;
      tagRectTopRight :
        begin
          PerformTopSide(ly,lRect0);
          PerformRightSide(lx,lRect0);
        end;
      tagRectBottomRight :
        begin
          PerformBottomSide(ly,lRect0);
          PerformRightSide(lx,lRect0);
        end;
      tagRectBottomLeft :
        begin
          PerformBottomSide(ly,lRect0);
          PerformLeftSide(lx,lRect0);
        end;
    end;
    // Now return adjust the changed coords and transform to the world coordinates
    // Invert the axis to ease calculation
    lRect0.TopLeft.Lat := -lRect0.TopLeft.Lat;
    lRect0.BottomRight.Lat := -lRect0.BottomRight.Lat;
    RealAreaNormed(lRect0);
    chgAllowed := True;
    if Assigned(FSelectedAreaChangingEvent) then
      FSelectedAreaChangingEvent(Self, lRect0, chgAllowed);
    if chgAllowed then
      FSelectedArea.Area := lRect0;
    MapView.Invalidate; // redraw the map
  end;
end;

procedure TAreaSelectionPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  lMouseWasDown : Boolean;
begin
  Unused(AMapView);
  // Caution the order of the following statemens are crucial
  if Button <> FMouseButton then Exit;  // Exit if not the defined button
  lMouseWasDown := Assigned(CurrentItem) and CurrentItem.FMouseDownFlag; // Save the information if we hold the button
  SetupRectShifter;  // Setup the HelperClass for the current setting
  if Handled then Exit; // if already handled by an other plugin  then exit
  Handled := lMouseWasDown; // set handled to true, if we holded the button
  if lMouseWasDown and Assigned(FSelectedAreaChangedEvent) then // notify if we holded the button
    FSelectedAreaChangedEvent(Self);
end;

procedure TAreaSelectionPlugin.MouseDown(AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
var
  i : Integer;
  lItem, lCurrItem : TMouseHitItem;
begin
  Unused(AMapView);
  if Button <> FMouseButton then Exit;
  if Handled then Exit;

  // Forward the MouseDown-Event to all Items
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    lItem.OnMouseDown(Shift,X,Y);
  end;

  lCurrItem := GetCurrentItem;
  Handled := Assigned(lCurrItem); // Reserve this event for us (Prvent dragging the map)

  // Resetting all other Item's MouseDown-Flags but not from the current item.
  // Might not be necessary, but double tap ;-)
  if Assigned(lCurrItem) then
  begin
    for i := 0 to ItemsCount-1 do
    begin
      lItem := TMouseHitItem(FMouseHitItems[i]);
      if lItem = lCurrItem then Continue;
      if lItem.MouseDownFlag then
        lItem.FMouseDownFlag := False;
    end;

    if Assigned(FSelectedAreaBeginChangeEvent) then
      FSelectedAreaBeginChangeEvent(Self);
  end;
  // Reset the inverters
  FShifterXInverseMode := False;
  FShifterYInverseMode := False;
end;

procedure TAreaSelectionPlugin.CenterMove(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView, Handled);

  SetupRectShifter;
end;

procedure TAreaSelectionPlugin.ZoomChange(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView, Handled);

  SetupRectShifter;
end;

procedure TAreaSelectionPlugin.Resize(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
  SetupRectShifter;
end;

{ SetupRectShifter will fill the Helper Class for the mouse movement with the
    values, so that the mouse pointer will show sensitive items }
type
  TRectArr = array of TRect;

procedure TAreaSelectionPlugin.SetupRectShifter;
var
  r0 : TRect;
  mapw : Int64;
  topLeftPt, bottomRightPt : TPoint;
  rectW : Integer;
  pt : TPoint;
  lRect : TRect;
  ptArr : TPointArray;
  rArr : TRectArr = Nil;
  i : Integer;
  found : Boolean;
begin
  Clear;

  topLeftPt := MapView.LatLonToScreen(FSelectedArea.North, FSelectedArea.West);
  lRect.Top := topLeftPt.Y;
  lRect.Left := topLeftPt.X;
  bottomRightPt := MapView.LatLonToScreen(FSelectedArea.South, FSelectedArea.East);
  lRect.Bottom := bottomRightPt.Y;
  lRect.Right := bottomRightPt.X;
  mapw := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
  if lRect.Left > lRect.Right then
    lRect.Left := lRect.Left - mapw;
  rectW := lRect.Right-lRect.Left;

  // Rectangles might be duplicated in the cyclic view
  ptArr := MapView.CyclicPointsOf(topLeftPt);
  SetLength(rArr,Length(ptArr));
  for i := 0 to High(ptArr) do
  begin
    pt := ptArr[i];
    r0.Left := pt.X;
    r0.Top := lRect.Top;
    r0.Right := pt.X+rectW;
    r0.Bottom := lRect.Bottom;
    rArr[i] := r0;
      AddSelectionArea(r0,FAreaInflation);
  end;

  // Catch the case of the missing left rectangle.
  ptArr := MapView.CyclicPointsOf(bottomRightPt);
  for pt in ptArr do
  begin
    r0.Left := pt.X-rectW;
    r0.Top := lRect.Top;
    r0.Right := pt.X;
    r0.Bottom := lRect.Bottom;
    found := False;
    for i := 0 to High(rArr) do
    begin
      if r0 = rArr[i] then
      begin
        found := True;
        Break;
      end;
    end;
    if not found then
      AddSelectionArea(r0,FAreaInflation);
  end;
end;

initialization
  RegisterPluginClass(TAreaSelectionPlugin, 'Area selection');

end.

