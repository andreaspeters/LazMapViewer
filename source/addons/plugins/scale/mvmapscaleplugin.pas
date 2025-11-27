{-------------------------------------------------------------------------------
                              mvMapScalePlugin.pas

License: modified LGPL with linking exception (like RTL, FCL and LCL)

See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
for details about the license.

See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
--------------------------------------------------------------------------------

TMapScalePlugin
---------------

The TMapScalePlugin draws a scale bar in the map to indicate the current
zoom magnification. The scale bar is labeled by its length in real-world units.

Note that due to the projection of the earth onto a plane the magnficiation
factor depends on the latitude. In particular at low zoom levels, the
magnification can vary between the top and bottom of the displayed map.

Properties
----------

AlignSet: TScaleAlignSet
  a set of alignment values (alLeft, alTop, alRight, alBottom) determining the
  map edge at which the scale bar is positioned.
  Including both alLeft and alTop centers the bar horizontally,
  including alTop and alBottom centers it vertically.

Imperial: Boolean
   Allows to switch between metric (km, m) and imperial (miles, feet) length
   units

SpaceX: Integer
  Distance of the scale bar from  the left or right side of the map

SpaceY: Integer
  Distance of the scale bar from the top or bottom side of the map

WidthMax: Integer
  Determines the maximum width of the scale bar. The width, however, usually
  is reduced so that the length label has a "nice" value.

ZoomMin: Integer
  Mininum zoom level at which or above which the scale bar is displayed. When
  the current zoom level is smaller than this limit the bar is hidden because
  the magnfication varies noticeably across the latitudes shown in the map
  due to the map projection

BackgroundColor: TColor
  Background color of the scale bar. The background is transparent for the
  color clNone, or when the BackgroundOpacity is 0

BackgroundOpacity: single
  Determines the transparency of the background of the scale bar.
  The value can range between 0 (fully transparent) and 1 (fully opaque).

Font: TFont
  Determines the font used for the length label of the scale bar

Pen: TPen
  Determines the color, width and style of the scale bar line
-------------------------------------------------------------------------------}

unit mvMapScalePlugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Graphics, Controls, Types,
  mvMapViewer, mvStrConsts, mvTypes, mvGeoMath, mvPluginCommon;

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
  W, H, HalfHeight, SpcX, SpcY, MaxW: Integer;
  P1, P2: TPoint;
  RP1, RP2: TRealPoint;
begin
  if AMapView.Zoom < FZoomMin then
    exit;

  SpcX := FSpaceX;
  SpcY := FSpaceY;
  MaxW := Min(WidthMax, AMapView.ClientWidth);
  HalfHeight := AMapView.Height div 2;    // --> measure distance in screen center

  P1 := Point(0, HalfHeight);
  P2 := Point(MaxW, HalfHeight);
  if (AMapView.Engine.MapLeft > 0) and (not AMapView.Cyclic) then
  begin
    inc(P1.X, AMapView.Engine.MapLeft);
    inc(P2.X, AMapView.Engine.MapLeft);
  end;
  RP1 := AMapView.ScreenToLatLon(P1);
  RP2 := AMapView.ScreenToLatLon(P2);
  Dist := mvGeoMath.CalcGeoDistance(RP1.Lat, RP1.Lon, RP2.Lat, RP2.Lon, duMeters);

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
  RegisterPluginClass(TMapScalePlugin, @mvRS_MapScalePlugin);

end.

