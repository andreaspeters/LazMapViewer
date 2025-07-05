{ The TDragColoredItemPlugin implements an example of sharing the mouse events
  between different Plugins.
  The Plugin will react on the three mouse events (MouseDown, MouseMove and MouseUp)
  in the way that it allows the user to drag the items with the right mouse button
  down. While hovering around the plugin will signalize the user the possible
  drag option if the mouse is not down.
}

unit uDragColoredItemPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  mvPluginCommon, mvPlugins, mvMapViewer, mvTypes, mvGPSObj, mvGeoMath;

type

  { TDragColoredItemPlugin }

  TDragColoredItemPlugin = class(TMvPlugin)
  private
    FMouseDown : Boolean;
    FCurrentCoord : TRealPoint;
    FRectSize : Integer;
    FColor : TColor;
    FLastPointer : Boolean;
    FMouseCursor : TCursor;
    FShowCaption : Boolean;
    function IsAtMousePosition(const X,Y: Integer) : Boolean;
    procedure SetRectSize(Value : Integer);
  protected
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
          var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
          X,Y: Integer; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
          X,Y: Integer; var Handled: Boolean); override;
    procedure MouseWheel(AMapView: TMapView; AShift: TShiftState;
      AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean); override;
  public
    property Color : TColor read FColor write FColor;
    property MouseCursor : TCursor read FMouseCursor write FMouseCursor;
    property RectSize : Integer read FRectSize write SetRectSize;
    property ShowCaption : Boolean read FShowCaption write FShowCaption;
    constructor Create(AOwner: TComponent); override;
  end;

const
  MinRectSize = 6;
  MaxRectSize = 100;
  DefaultRectSize = 20;

implementation
{ TDragColoredItemPlugin }

function TDragColoredItemPlugin.IsAtMousePosition(const X, Y: Integer): Boolean;
var
  pt, ptc : TPoint;
  aArea : TRealArea;
  ptR : TRealPoint;
  w2 : Integer;
begin
  Result := False;
  pt.X := X;
  pt.Y := Y;
  ptR := MapView.ScreenToLatLon(pt);
  ptc := MapView.LatLonToScreen(FCurrentCoord);
  w2 := FRectSize div 2;
  pt.X := ptc.X-w2;
  pt.Y := ptc.Y-w2;
  aArea.TopLeft := MapView.ScreenToLatLon(pt);
  pt.X := ptc.X+w2;
  pt.Y := ptc.Y+w2;
  aArea.BottomRight := MapView.ScreenToLatLon(pt);
  Result := False;
  if PtInsideArea(ptR,aArea) then
    Result := True;
end;

procedure TDragColoredItemPlugin.SetRectSize(Value: Integer);
var
  newsz : Integer;
begin
  if Value < MinRectSize then
    newsz := MinRectSize
  else if Value > MaxRectSize then
    newsz := MaxRectSize
  else
    newsz := Value;
  if FRectSize <> newsz then
  begin
     FRectSize := newsz;
     MapView.Invalidate;
  end;
end;

procedure TDragColoredItemPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
var
  ptCurrentScreen : TPoint;
  s : String;
  w2 : Integer;
begin
  MapView.Canvas.Pen.Style:= psSolid;
  MapView.Canvas.Pen.Width:= 3;
  MapView.Canvas.Pen.Color:= FColor;
  ptCurrentScreen := MapView.LatLonToScreen(FCurrentCoord);
  w2 := (FRectSize div 2);
  MapView.Canvas.Rectangle(ptCurrentScreen.X-w2,ptCurrentScreen.Y-w2,
                           ptCurrentScreen.X+w2,ptCurrentScreen.Y+w2);
  if FShowCaption then
  begin
    s := Format('Index: %d',[Index]);
    MapView.Canvas.TextOut(ptCurrentScreen.X+w2, ptCurrentScreen.Y+w2, s);
  end;
end;

procedure TDragColoredItemPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
begin
  // Three different things to catch
  // 1. In mouse down mode, drag the item
  // 2. In mouse up mode
  //   2.a) ensure that the mouse button is globally up
  //   2.b) if inside the rectangle change the MouseCursor
  //   2.c) if outside release the mouse pointer, but only if we changed him before

  if FMouseDown then
  begin // we have the mouse down
    FCurrentCoord := MapView.ScreenToLatLon(Point(X,Y));  // Move the rectangle
    Handled := True; // prevent others from also operate
    MapView.Invalidate; // init the new painting
  end
  else if (not Handled) and
          (PluginManager.MouseButtonDown[MapView] = []) then
  begin // the mouse is not down (any other operation) and the moving is not consumed
    if IsAtMousePosition(X,Y) then
    begin // inside the rectangle
      MapView.Cursor := FMouseCursor;  // change the cursor
      FLastPointer := True; // remember we do so
      Handled := True; // prevent others from changing
    end
    else if FLastPointer then
    begin // outside the rectangle
      MapView.Cursor := crDefault; // return to default cursor (might be overwirtten by the following plugins)
      FLastPointer := False; // reset the flag
    end;
  end;
end;

procedure TDragColoredItemPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Button <> mbRight then Exit; // We react only on the right mouse button
  if FMouseDown then // if we are in mouse down mode...
  begin
    FMouseDown := False; // ...  we have to release the mode
    MapView.Cursor := crDefault; // return to default
    Handled := True; // we consumed the event (this will not change the behavior)
  end;
end;

procedure TDragColoredItemPlugin.MouseDown(AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
begin
  if Button <> mbRight then Exit; // We react only on the right mouse button
  if Handled then Exit; // if the mouse down button has been consumed, nothing further to do
  if IsAtMousePosition(X,Y) then // check the coords
  begin
    FMouseDown := True; // remember that we are in MouseDown mode
    FCurrentCoord := MapView.ScreenToLatLon(Point(X,Y)); // store the current position
    Handled := True; // prevent later plugins to consume this event
  end;
end;

procedure TDragColoredItemPlugin.MouseWheel(AMapView: TMapView;
  AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint;
  var Handled: Boolean);
begin
  if Handled then Exit;
  if IsAtMousePosition(AMousePos.X,AMousePos.Y) then
  begin
    Handled := True;
    RectSize := RectSize + (AWheelDelta div 20);
  end;
end;

constructor TDragColoredItemPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FRectSize := DefaultRectSize;
end;

end.

