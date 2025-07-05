{ TMapScalePlugin - draws a length scale corresponding to the current zoom level }

unit mvMapScalePlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Graphics, Controls, Types,
  mvMapViewer, mvGeoMath, mvPluginCommon;

type
  { TMapScalePlugin }

  TScaleAlignSet = set of alTop..alRight;

  TMapScalePlugin = class(TMvDrawPlugin)
  private
    FSpaceY: Integer;
    FAlignSet: TScaleAlignSet;
    FImperial: Boolean;
    FSpaceX: Integer;
    FWidthMax: Integer;
    FZoomMin: Integer;
    procedure SetAlignSet(AValue: TScaleAlignSet);
    procedure SetImperial(AValue: Boolean);
    procedure SetSpaceX(AValue: Integer);
    procedure SetSpaceY(AValue: Integer);
    procedure SetWidthMax(AValue: Integer);
    procedure SetZoomMin(AValue: Integer);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var {%H-}Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property AlignSet: TScaleAlignSet read FAlignSet write SetAlignSet default [alRight, alBottom];
    property Imperial: Boolean read FImperial write SetImperial default False;
    property SpaceX: Integer read FSpaceX write SetSpaceX default 10;
    property SpaceY: Integer read FSpaceY write SetSpaceY default 10;
    property WidthMax: Integer read FWidthMax write SetWidthMax default 250;
    property ZoomMin: Integer read FZoomMin write SetZoomMin default 8;
    // inherited properties
    property BackgroundColor;
    property BackgroundOpacity;
    property Font;
    property Pen;
  end;

implementation

constructor TMapScalePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignSet := [alRight, alBottom];
  FImperial := False;
  FSpaceX := 10;
  FSpaceY := 10;
  FWidthMax := 250;
  FZoomMin := 8;
end;

procedure TMapScalePlugin.Assign(ASource: TPersistent);
begin
  if ASource is TMapScalePlugin then
  begin
    FAlignSet := TMapScalePlugin(ASource).AlignSet;
    FImperial := TMapScalePlugin(ASource).Imperial;
    FSpaceX := TMapScalePlugin(ASource).SpaceX;
    FSpaceY := TMapScalePlugin(ASource).SpaceY;
    FWidthMax := TMapScalePlugin(ASource).WidthMax;
    FZoomMin := TMapScalePlugin(ASource).ZoomMin;
  end;
  inherited Assign(ASource);
end;

procedure TMapScalePlugin.AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
var
  Dist, V: Double;
  Digits: Integer;
  R: TRect;
  OldOpacity: Single;
  OldPenStyle: TPenStyle;
  Capt: String;
  Extent: TSize;
  W, H, HalfHeight, SpcX, SpcY: Integer;
  MaxW: Integer;
begin
  if AMapView.Zoom < FZoomMin then
    exit;

  SpcX := FSpaceX;
  SpcY := FSpaceY;
  MaxW := Min(WidthMax, AMapView.ClientWidth);
  HalfHeight := AMapView.Height div 2;

  with AMapView.Engine.ScreenRectToRealArea(Rect(0, HalfHeight, MaxW, HalfHeight)) do
    Dist := mvGeoMath.CalcGeoDistance(TopLeft.Lat, TopLeft.Lon, TopLeft.Lat, BottomRight.Lon, duMeters);

  if Imperial then
  begin
    Dist := Dist * 0.62137E-3; // to miles
    Capt := 'mi';
    if Dist < 1.0 then
    begin
      Dist := Dist * 5280;  // 1mi = 5280ft
      Capt := 'ft';
    end;
  end
  else
  begin
    Capt := 'm';
    if Dist >= 1000 then
    begin
      Dist := Dist * 0.001;
      Capt := 'km';
    end;
  end;

  Digits := Trunc(Math.Log10(Dist));
  V := Power(10, Digits);

  // 5, 3, 2, 1 multipliers
  if V * 5 < Dist then
    V := V * 5
  else if V * 3 < Dist then
    V := V * 3
  else if V * 2 < Dist then
    V := V * 2;

  // Caption
  Capt := Round(V).ToString + ' ' + Capt;
  Extent := AMapView.DrawingEngine.TextExtent(Capt);

  // Width and height
  W := Round(MaxW * (V / Dist));
  H := Extent.Height + 3 + 3;

  if W + SpcX >= AMapView.ClientWidth then
    SpcX := Max(1, AMapView.ClientWidth - W - 1);
  if H + SpcY > AMapView.ClientHeight then
    SpcY := Max(1, AMapView.ClientHeight - H - 1);

  R := Rect(0, 0, W, H);

  // Fix align set
  if FAlignSet * [alLeft, alRight] = [] then
    Include(FAlignSet, alRight);
  if FAlignSet * [alTop, alBottom] = [] then
    Include(FAlignSet, alBottom);

  // Horizontal position
  if alLeft in AlignSet then
    if alRight in AlignSet then
      R.Offset((AMapView.ClientWidth - W) div 2, 0) // Both alLeft+alRight=Center
    else
      R.Offset(SpcX, 0) // to the left
  else
    if alRight in AlignSet then
      R.Offset((AMapView.ClientWidth - W) - SpcX, 0); // to the right

  // Vertical position
  if alTop in AlignSet then
    if alBottom in AlignSet then
      R.Offset(0, (AMapView.ClientHeight - H) div 2) // Both alTop+alBottom=Middle
    else
      R.Offset(0, SpcY) // to the top
  else
    if alBottom in AlignSet then
      R.Offset(0, (AMapView.ClientHeight - H) - SpcY); // to the bottom

  OldOpacity := AMapView.DrawingEngine.Opacity;
  OldPenStyle := AMapView.DrawingEngine.PenStyle;
  with AMapView.DrawingEngine do
  try
    // Semitransparent background
    Opacity := BackgroundOpacity;
    BrushStyle := bsSolid;
    BrushColor := BackgroundColor;
    FillRect(R.Left, R.Top, R.Right, R.Bottom);

    // Bar
    Opacity := 1.0;
    PenStyle := Self.Pen.Style;
    PenColor := Self.Pen.Color;
    PenWidth := Self.Pen.Width;
    Polyline([
      R.TopLeft + Point(0, 10),
      R.TopLeft,
      Point(R.Right, R.Top),
      Point(R.Right, R.Top) + Point(0, 10)
    ]);

    // Caption
    BrushStyle := bsClear;
    FontName := Self.Font.Name;
    SetFont(Self.Font.Name, Self.Font.Size, Self.Font.Style, ColorToRGB(Self.Font.Color));
    TextOut(R.CenterPoint.X - Extent.CX div 2, R.Top + 3, Capt);
  finally
    Opacity := OldOpacity;
    PenStyle := OldPenStyle;
  end;
end;

procedure TMapScalePlugin.SetAlignSet(AValue: TScaleAlignSet);
begin
  if FAlignSet = AValue then Exit;
  FAlignSet := AValue;
  Update;
end;

procedure TMapScalePlugin.SetImperial(AValue: Boolean);
begin
  if FImperial = AValue then Exit;
  FImperial := AValue;
  Update;
end;

procedure TMapScalePlugin.SetSpaceX(AValue: Integer);
begin
  if FSpaceX = AValue then Exit;
  FSpaceX := AValue;
  Update;
end;

procedure TMapScalePlugin.SetSpaceY(AValue: Integer);
begin
  if FSpaceY = AValue then Exit;
  FSpaceY := AValue;
  Update;
end;

procedure TMapScalePlugin.SetWidthMax(AValue: Integer);
begin
  if FWidthMax = AValue then Exit;
  FWidthMax := AValue;
  Update;
end;

procedure TMapScalePlugin.SetZoomMin(AValue: Integer);
begin
  if FZoomMin = AValue then Exit;
  FZoomMin := AValue;
  Update;
end;

initialization
  RegisterPluginClass(TMapScalePlugin, 'Map scale');

end.

