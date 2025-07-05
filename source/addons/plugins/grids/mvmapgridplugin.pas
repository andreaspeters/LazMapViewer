{ TMapGridPlugin - draws a grid of constant longitudes and constant latitudes}

unit mvMapGridPlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Graphics, Controls, LCLIntf, // LazLoggerBase,
  mvMapViewer, mvDrawingEngine, mvPluginCommon,  mvGeoMath, mvTypes;

type
  TMapGridPlugin = class;
  TMapGridLabelPosition = (glpLeft, glpTop, glpRight, glpBottom);
  TMapGridLabelPositions = set of TMapGridLabelPosition;

  TMapGridPlugin = class(TMvDrawPlugin)
  private
    const
      DEFAULT_LABEL_POSITIONS = [glpLeft, glpTop];
      DEFAULT_MAX_DISTANCE = 200;
      DEFAULT_MIN_DISTANCE = 80;
    type
      TGridCoordType = (gctLatitude, gctLongitude);
  private
    FIncrement: Double;
    FLabelDistance: Integer;
    FLabelPositions: TMapGridLabelPositions;
    FMaxDistance: Integer;
    FMinDistance: Integer;
    procedure SetIncrement(AValue: Double);
    procedure SetLabelDistance(AValue: Integer);
    procedure SetLabelPositions(AValue: TMapGridLabelPositions);
    procedure SetMaxDistance(AValue: Integer);
    procedure SetMinDistance(AValue: Integer);
  protected
    procedure BeforeDrawObjects(AMapView: TMapView; var {%H-}Handled: Boolean); override;
    function CalcIncrement(AMapView: TMapView; Area: TRealArea): Double;
    function CalcVisibleArea(AMapView: TMapView): TRealArea;
    procedure DrawGridLine(ADrawingEngine: TMvCustomDrawingEngine;
      AValue: Double; P1, P2: TPoint; AMapRect: TRect);
    procedure DrawHorGridLines(AMapView: TMapView; Area: TRealArea; AMapRect: TRect; AIncrement: Double);
    procedure DrawVertGridlines(AMapView: TMapView; Area: TRealArea; AMapRect: TRect; AIncrement: Double);
    function GetLatLonAsString(AValue: Double; ACoordType: TGridCoordType): String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Increment: Double read FIncrement write SetIncrement; // 0 = automatic increment detection
    property LabelDistance: Integer read FLabelDistance write SetLabelDistance default 0;
    property LabelPositions: TMapGridLabelPositions read FLabelPositions write SetLabelPositions default DEFAULT_LABEL_POSITIONS;
    property MaxDistance: Integer read FMaxDistance write SetMaxDistance default DEFAULT_MAX_DISTANCE;
    property MinDistance: Integer read FMinDistance write SetMinDistance default DEFAULT_MIN_DISTANCE;
    // inherited properties
    property BackgroundColor;
    property BackgroundOpacity;
    property Font;
    property Pen;
  end;

implementation

uses
  Types;


{ TMapGridPlugin }

constructor TMapGridPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FMaxDistance := DEFAULT_MAX_DISTANCE;
  FMinDistance := DEFAULT_MIN_DISTANCE;
  FLabelPositions := DEFAULT_LABEL_POSITIONS;
end;

procedure TMapGridPlugin.Assign(ASource: TPersistent);
begin
  if (ASource is TMapGridPlugin) then
  begin
    FIncrement := TMapGridPlugin(ASource).Increment;
    FLabelDistance := TMapGridPlugin(ASource).LabelDistance;
    FLabelPositions := TMapGridPlugin(ASource).LabelPositions;
    FMaxDistance := TMapGridPlugin(ASource).MaxDistance;
    FMinDistance := TMapGridPlugin(ASource).MinDistance;
  end;
  inherited Assign(ASource);
end;

procedure TMapGridPlugin.BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean);
var
  area: TRealArea;
  coordType: TGridCoordType;
  incr: Double;
  mapRectPx: TRect;
begin
  area := CalcVisibleArea(AMapView);

  mapRectPx.TopLeft := AMapView.Engine.LatLonToScreen(area.TopLeft);
  mapRectPx.BottomRight := AMapView.Engine.LatLonToScreen(area.BottomRight);
  if AMapView.Cyclic then begin
    mapRectPx.Left := 0;
    mapRectPx.Right := AMapView.Width;
  end;

  if FIncrement <= 0.0 then
    incr := CalcIncrement(AMapView, area)
  else
    incr := FIncrement;

  AMapView.DrawingEngine.PenStyle := Pen.Style;
  AMapView.DrawingEngine.PenWidth := Pen.Width;
  AMapView.DrawingEngine.PenColor := Pen.Color;
  AMapView.DrawingEngine.BrushColor := BackgroundColor;
  AMapView.DrawingEngine.FontName := Font.Name;
  AMapView.DrawingEngine.FontSize := Font.Size;
  AMapView.DrawingEngine.FontStyle := Font.Style;
  AMapView.DrawingEngine.FontColor := Font.Color;

  for coordType in TGridCoordType do
  begin
    case coordType of
      gctLatitude: DrawHorGridLines(AMapView, area, mapRectPx, incr);
      gctLongitude: DrawVertGridLines(AMapView, area, mapRectPx, incr);
    end;
  end;
end;

{ Calculates the increment of the grid lines. Since, due to the projection, the
  latitude lines have a non-constant spacing, their increment is taken from
  the equator value, i.e. is assumed to be equal to the longitude lines spacing. }
function TMapGridPlugin.CalcIncrement(AMapView: TMapView; Area: TRealArea): Double;
type
  TIncrement = (deg180, deg90, deg45, deg30, deg10, deg5, deg2, deg1,
    min30, min20, min10, min5, min2, min1,
    sec30, sec20, sec10, sec5, sec2, sec1);
const
  IncrementValues: array[TIncrement] of double = (
    180.0, 90.0, 45.0, 30.0, 10.0, 5.0, 2.5, 1.0,
    30.0/60, 20.0/60, 10.0/60, 5.0/60, 2.5/60, 1.0/60,
    30.0/3600, 20.0/3600, 10.0/3600, 5.0/3600, 2.5/3600, 1.0/3600);
var
  fullRange: Double;
  deg2px: Double;
  incr: TIncrement;
  incrementPx: Integer;
begin
  Result := 90;
  fullRange := abs(Area.BottomRight.Lon - Area.TopLeft.Lon);
  deg2px := fullRange / AMapView.ClientWidth;

  for incr in TIncrement do
  begin
    incrementPx := trunc(IncrementValues[incr] / deg2px);
    if (incrementPx >= MinDistance) and (incrementPx <= MaxDistance) then
    begin
      Result := IncrementValues[incr];
      break;
    end;
  end;
end;

{ Calculates the corner coordinates (in degrees) for the displayed map.
  The longitude values are expanded such that they increase monotonously
  from left to right. }
function TMapGridPlugin.CalcVisibleArea(AMapView: TMapView): TRealArea;
var
  worldWidth: Integer;
  numWorlds: Integer;
begin
  Result := AMapView.Engine.ScreenRectToRealArea(Rect(0, 0, AMapView.ClientWidth, AMapView.ClientHeight));
  if AMapView.Engine.CrossesDateline then
  begin
    if Result.BottomRight.Lon <= Result.TopLeft.Lon then
      Result.BottomRight.Lon := Result.BottomRight.Lon + 360;
    worldWidth := mvGeoMath.ZoomFactor(AMapView.Zoom) * TileSize.CX;
    numWorlds := trunc(AMapView.ClientWidth / worldWidth);
    Result.BottomRight.Lon := Result.BottomRight.Lon + numWorlds * 360;
  end;

  // Workaround for cyclic map at lowest zoom reaching across the date line:
  // When its width is decreased slowly and the dateline is just crossed then
  // there is only a small difference between left and right longitude numbers
  // although the real difference is almost 180°.
  // This causes a hang in the drawing routine. --> Add 360° to right longitude.
  if (AMapView.Zoom < 2) and (Result.BottomRight.Lon - Result.TopLeft.Lon < 2) then
    Result.BottomRight.Lon := Result.BottomRight.Lon + 360;
end;

procedure TMapGridPlugin.DrawGridLine(ADrawingEngine: TMvCustomDrawingEngine;
  AValue: Double; P1, P2: TPoint; AMapRect: TRect);
var
  s: String;
  txtSize: TSize;
  txtRect: array[TMapGridLabelPosition] of TRect;
  glp: TMapGridLabelPosition;
  coordType: TGridCoordType;
begin
  if P1.Y = P2.Y then
  begin
    coordType := gctLatitude;
    if (P1.Y < AMapRect.Top) or (P2.Y > AMapRect.Bottom) then   // Pixel coordinates grow downwards
      exit;
  end else
  if P1.X = P2.X then
  begin
    coordType := gctLongitude;
    AValue := NormalizeLon(AValue);
    if (P1.X < AMapRect.Left) or (P2.X > AMapRect.Right) then
      exit;
  end else
    exit;

  // Calculate label text and position
  s := GetLatLonAsString(AValue, coordType);
  txtSize := ADrawingEngine.TextExtent(s);
  case coordType of
    gctLatitude:
      begin
        if (glpLeft in FLabelPositions) and InRange(P1.X, AMapRect.Left, AMapRect.Right) then
        begin
          txtRect[glpLeft] := Rect(0, 0, txtSize.CX, txtSize.CY);
          OffsetRect(txtRect[glpLeft], P1.X + FLabelDistance, P1.Y - txtSize.CY div 2);
        end else
          txtRect[glpLeft] := Rect(AMapRect.Left, P1.Y, AMapRect.Left, P1.Y);
        if (glpRight in FLabelPositions) and InRange(P1.X, AMapRect.Left, AMapRect.Right) then
        begin
          txtRect[glpRight] := Rect(0, 0, txtSize.CX, txtSize.CY);
          OffsetRect(txtRect[glpRight], P2.X - txtSize.CX - FLabelDistance, P1.Y - txtSize.CY div 2);
        end else
          txtRect[glpRight] := Rect(AMapRect.Right, P1.Y, AMapRect.Right, P1.Y);
      end;
    gctLongitude:
      begin
        if (glpTop in FLabelPositions) then
        begin
          txtRect[glpTop] := Rect(0, 0, txtSize.CX, txtSize.CY);
          OffsetRect(txtRect[glpTop], P1.X - txtSize.CX div 2, P1.Y + FLabelDistance);
        end else
          txtRect[glpTop] := Rect(P1.X, AMapRect.Top, P1.X, AMapRect.Top);
        if (glpBottom in FLabelPositions) then
        begin
          txtRect[glpBottom] := Rect(0, 0, txtSize.CX, txtSize.CY);
          OffsetRect(txtRect[glpBottom], P1.X - txtSize.CX div 2, P2.Y - txtSize.CY - FLabelDistance);
        end else
          txtRect[glpBottom] := Rect(P1.X, AMapRect.Bottom, P1.X, AMapRect.Bottom);
      end;
    end;
    for glp in TMapGridLabelPositions do
    begin
      if txtRect[glp].Top < AMapRect.Top then OffsetRect(txtRect[glp], 0, AMapRect.Top - txtRect[glp].Top);
      if txtRect[glp].Bottom > AMapRect.Bottom then OffsetRect(txtRect[glp], 0, AMapRect.Bottom - txtRect[glp].Bottom);
      if txtRect[glp].Left < AMapRect.Left then OffsetRect(txtRect[glp], AMapRect.Left - txtRect[glp].Left, 0);
      if txtRect[glp].Right > AMapRect.Right then OffsetRect(txtRect[glp], AMapRect.Right - txtRect[glp].Right, 0);
    end;

    // Draw latitude/longitude lines
    ADrawingEngine.Opacity := 1.0;
    case coordType of
      gctLatitude: ADrawingEngine.Line(txtRect[glpLeft].Right, P1.Y, txtRect[glpRight].Left, P2.Y);
      gctLongitude: ADrawingEngine.line(P1.X, txtRect[glpTop].Bottom, P2.X, txtRect[glpBottom].Top);
    end;

    // Draw (semi-transparent) background
    if BackgroundColor <> clNone then
    begin
      ADrawingEngine.Opacity := BackgroundOpacity;
      ADrawingEngine.BrushStyle := bsSolid;
      case coordType of
        gctLatitude:
          begin
            if (glpLeft in FLabelPositions) then
              with txtRect[glpLeft] do
                ADrawingEngine.FillRect(Left, Top, Right, Bottom);
            if (glpRight in FLabelPositions) then
              with txtRect[glpRight] do
                ADrawingEngine.FillRect(Left, Top, Right, Bottom);
          end;
        gctLongitude:
          begin
            if (glpTop in FLabelPositions) then
              with txtRect[glpTop] do
                ADrawingEngine.FillRect(Left, top, Right, Bottom);
            if (glpBottom in FLabelPositions) then
              with txtRect[glpBottom] do
                ADrawingEngine.FillRect(Left, Top, Right, Bottom);
          end;
      end;
    end;

    // Draw grid coordinates as text
    ADrawingEngine.BrushStyle := bsClear;
    ADrawingEngine.Opacity := 1.0;
    case coordType of
      gctLatitude:
        begin
          if glpLeft in FLabelPositions then
            with txtRect[glpLeft] do
              ADrawingEngine.TextOut(Left, Top, s);
          if glpRight in FLabelPositions then
            with txtRect[glpRight] do
              ADrawingEngine.TextOut(Left, Top, s);
        end;
      gctLongitude:
        begin
          if glpTop in FLabelPositions then
            with txtRect[glpTop] do ADrawingEngine.TextOut(Left, Top, s);
          if glpBottom in FLabelPositions then
            with txtRect[glpBottom] do ADrawingEngine.TextOut(Left, Top, s);
        end;
    end;
end;

{ Draws horizontal grid lines (constant longitudes)
  * Area - Area covered by the map, in (monotonous) degrees
  * AMapRect - dto., in pixels.
  * AIncrement - distance between grid lines, in degrees. }
procedure TMapGridPlugin.DrawHorGridLines(AMapView: TMapView; Area: TRealArea;
  AMapRect: TRect; AIncrement: Double);
var
  rP1, rP2: TRealPoint;    // Top and bottom points of each gridline, in degrees
  P1, P2: TPoint;          // ... and in pixels
  lat: Double;
begin
  // Northern hemispere...
  lat := trunc(Area.BottomRight.Lat / AIncrement) * AIncrement;
  rP1 := RealPoint(lat, Area.TopLeft.Lon);
  rP2 := RealPoint(lat, Area.BottomRight.Lon);
  while (rP2.Lat <= Area.TopLeft.Lat) do
  begin
    P1 := AMapView.Engine.LatLonToScreen(rP1);
    P2 := AMapView.Engine.LatLonToScreen(rP2);
    if AMapView.Cyclic then
    begin
      P1.X := 0;
      P2.X := AMapView.ClientWidth;
    end;
    DrawGridLine(AMapView.DrawingEngine, rP1.Lat, P1, P2, AMapRect);
    rP1.Lat := rP1.Lat + AIncrement;
    rP2.Lat := rP2.Lat + AIncrement;
  end;

  // Southern hemisphere...
  rP1 := RealPoint(0.0, Area.TopLeft.Lon);
  rP2 := RealPoint(0.0, Area.BottomRight.Lon);
  while (rP2.Lat >= Area.BottomRight.Lat) do
  begin
    P1 := AMapView.Engine.LatLonToScreen(rP1);
    P2 := AMapView.Engine.LatLonToScreen(rP2);
    if AMapView.Cyclic then
    begin
      P1.X := 0;
      P2.X := AMapView.ClientWidth;
    end;
    DrawGridLine(AMapView.DrawingEngine, rP1.Lat, P1, P2, AMapRect);
    rP1.Lat := rP1.Lat - AIncrement;
    rP2.Lat := rP2.Lat - AIncrement;
  end;
end;

{ Draws vertical grid lines (constant longitude).
  * Area - Area covered by the map, in (monotonous) degrees
  * AMapRect - dto., in pixels.
  * AIncrement - distance between grid lines, in degrees. }
procedure TMapGridPlugin.DrawVertGridlines(AMapView: TMapView; Area: TRealArea;
  AMapRect: TRect; AIncrement: Double);
var
  rP1, rP2: TRealPoint;    // top and bottom points of each gridline, in degrees
  P1, P2: TPoint;          // ... and in pixels
  incrPx: integer;         // increment (distance between grid lines), in pixels
  lon: Double;             // longitude of grid line, in (monotonous) degrees
  lonPx, lonPxLeft, lonPxRight: Integer;  // longitude parameters and its limits, in pixels
  worldSize: Int64;
begin
  // Left and right pixel borders of the area to be handled.
  if AMapView.Cyclic then
  begin
    lonPxLeft := 0;
    lonPxRight := AMapView.Width;
  end else
  begin
    lonPxLeft := AMapRect.Left;
    lonPxRight := AMapRect.Right;
  end;

  // Increment in pixels
  worldSize := mvGeoMath.ZoomFactor(AMapView.Zoom) * TileSize.CX;
  incrPx := round(AIncrement / 360 * worldSize);

  // Rounded longitude for grid point
  lon := trunc(Area.TopLeft.Lon / AIncrement) * AIncrement;
  rP1 := RealPoint(Area.TopLeft.Lat, lon);
  rP2 := RealPoint(Area.BottomRight.Lat, lon);
  P1 := AMapView.Engine.LatLonToScreen(rP1);
  P2 := AMapView.Engine.LatLonToScreen(rP2);

  // Find starting point at the left side, outside the map
  lonPx := P1.X;
  while lonPx > lonPxLeft do
  begin
    dec(lonPx, incrPx);
    lon := lon - AIncrement;
  end;

  // Now go to the right in AIncrement steps and draw a vertical line
  while lonPx < lonPxRight do
  begin
    DrawGridLine(AMapView.DrawingEngine, lon, Point(lonPx, P1.Y), Point(lonPx, P2.Y), AMapRect);
    lon := lon + AIncrement;
    lonPx := lonPx + incrPx;
  end;
end;

function TMapGridPlugin.GetLatLonAsString(AValue: Double; ACoordType: TGridCoordType): String;
var
  degs, mins, secs: Double;
  secsZero: Boolean;
begin
  SplitGPS(AValue, degs, mins, secs);
  secsZero := SameValue(secs, 0.0, 2E-1);

  if (mins = 0) and secsZero then
    Result := Format('%.0f°', [abs(degs)])
  else
  if secsZero then
    Result := Format('%.0f° %.0f''', [abs(degs), mins])
  else
    Result := Format('%.0f° %.0f'' %.0f"', [abs(degs), mins, secs]);
  case ACoordType of
    gctLatitude:
      if AValue > 0 then
        Result := Result + ' N'
      else if AValue < 0 then
        Result := Result + ' S';
    gctLongitude:
      if (abs(AValue) <> 180) and (AValue <> 0) then
      begin
        if (AValue > 0) then
          Result := Result + ' E'
        else
          Result := Result + ' W';
      end;
  end;
end;

procedure TMapGridPlugin.SetIncrement(AValue: Double);
begin
  if FIncrement <> AValue then
  begin
    FIncrement := AValue;
    Update;
  end;
end;

procedure TMapGridPlugin.SetLabelDistance(AValue: Integer);
begin
  if FLabelDistance <> AValue then
  begin
    FLabelDistance := AValue;
    Update;
  end;
end;

procedure TMapGridPlugin.SetLabelPositions(AValue: TMapGridLabelPositions);
begin
  if FLabelPositions <> AValue then
  begin
    FLabelPositions := AValue;
    Update;
  end;
end;

procedure TMapGridPlugin.SetMaxDistance(AValue: Integer);
begin
  if FMaxDistance <> AValue then
  begin
    FMaxDistance := AValue;
    Update;
  end;
end;

procedure TMapGridPlugin.SetMinDistance(AValue: Integer);
begin
  if FMinDistance <> AValue then
  begin
    FMinDistance := AValue;
    Update;
  end;
end;


initialization
  RegisterPluginClass(TMapGridPlugin, 'Map grid');

end.

