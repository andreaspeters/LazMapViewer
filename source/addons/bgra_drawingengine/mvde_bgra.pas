{
  Drawing Engine for BGRABitmap library
  Copyright (C) 2019 user jc99 at Lazarus forum https://forum.lazarus.freepascal.org

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDE_BGRA;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Types, Graphics,
  mvStrConsts, mvDrawingEngine, mvCache,
  BGRAGraphics, BGRAVectorize, BGRABitmap;

type

  { TBGRABitmapCacheItem }

  TBGRABitmapCacheItem = class(TPictureCacheItem)
  private
    FImage: TBGRABitmap;
    function GetImage: TBGRABitmap;
  protected
    function GetImageObject: TObject; override;
    procedure StretchImageIfNeeded(var AImage: TBGRABitmap; ANewWidth, ANewHeight: Integer);
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    property Image: TBGRABitmap read GetImage;
  end;

  { TMvBGRADrawingEngine }

  TMvBGRADrawingEngine = class(TMvCustomDrawingEngine)
  private
    FFontName: String;
    FFontColor: TColor;
    FFontOrientation: Integer;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FOpacity: Single;

  protected
    FBuffer: TBGRABitmap;
    procedure ApplyFont;
    function GetBrushColor: TColor; override;
    function GetBrushStyle: TBrushStyle; override;
    function GetFontColor: TColor; override;
    function GetFontName: String; override;
    function GetFontOrientation: Single; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TFontStyles; override;
    function GetOpacity: Single; override;
    function GetPenColor: TColor; override;
    function GetPenStyle: TPenStyle; override;
    function GetPenWidth: Integer; override;
    procedure SetBrushColor(AValue: TColor); override;
    procedure SetBrushStyle(AValue: TBrushStyle); override;
    procedure SetFontColor(AValue: TColor); override;
    procedure SetFontOrientation(AValue: Single); override;
    procedure SetFontName(AValue: String); override;
    procedure SetFontSize(AValue: Integer); override;
    procedure SetFontStyle(AValue: TFontStyles); override;
    procedure SetOpacity(AValue: Single); override;
    procedure SetPenColor(AValue: TColor); override;
    procedure SetPenStyle(AValue: TPenStyle); override;
    procedure SetPenWidth(AValue: Integer); override;

  public
    destructor Destroy; override;
    procedure CreateBuffer(AWidth, AHeight: Integer); override;
    procedure DrawBitmap(X, Y: Integer; ABitmap: TCustomBitmap;
      {%H-}UseAlphaChannel: Boolean); override;
    procedure DrawCacheItem(X, Y: Integer; AImg: TPictureCacheItem;
      ADrawMode: TItemDrawMode = idmDraw; AOpacity: Single = 1.0); override;
    procedure DrawScaledCacheItem(DestRect, SrcRect: TRect; ASrcImg: TPictureCacheItem); override;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); override;
    procedure FillPixels(X1, Y1, X2, Y2: Integer; AColor: TColor); override;
    procedure FillRect(X1, Y1, X2, Y2: Integer); override;
    procedure Line(X1, Y1, X2, Y2: Integer); override;
    procedure Polyline(const Points: array of TPoint); override;
    procedure Polygon(const Points: array of TPoint); override;
    procedure PolyBezier(const Points: array of TPoint; Filled: Boolean = False;
      Continuous: Boolean = True); override;
    procedure PaintToCanvas(ACanvas: TCanvas; Origin: TPoint); override;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); override;
    function SaveToImage(AClass: TRasterImageClass): TRasterImage; override;
    function TextExtent(const AText: String; ARotated: Boolean = false): TSize; override;
    procedure TextOut(X, Y: Integer; const AText: String); override;
    function GetCacheItemClass: TPictureCacheItemClass; override;
  end;

procedure Register;


implementation

uses
  GraphType, LCLType, Math, FPImage, IntfGraphics,
  mvTypes,
  BGRABitmapTypes;

procedure Register;
begin
  RegisterComponents(PALETTE_PAGE, [TMvBGRADrawingEngine]);
end;

function RotatePointF(P: TPointF; sinPhi, cosPhi: Double): TPointF;
begin
  Result.X :=  cosPhi * P.X + sinPhi * P.Y;
  Result.Y := -sinPhi * P.X + cosPhi * P.Y;
end;

function OpacityToAlpha(AOpacity: Single): Byte;
begin
  if AOpacity > 1.0 then
    AOpacity := 1.0
  else if AOpacity < 0.0 then
    AOpacity := 0.0;
  Result := round(AOpacity * 255);
end;


{ TBGRABitmapCacheItem }

constructor TBGRABitmapCacheItem.Create(AStream: TStream);
var
  Reader: TFPCustomImageReader;
begin
  FImage := Nil;
  Reader := GetImageReader(AStream);
  if not Assigned(Reader) then
    raise EInvalidGraphic.Create(mvRS_PngJpegExpected);
  try
    FImage := TBGRABitmap.Create;
    try
      FImage.LoadFromStream(AStream, Reader);
      // Make sure that all tiles have the size defined by TileSize.
      StretchImageIfNeeded(FImage, TileSize.CX, TileSize.CY);
    except
      FreeAndNil(FImage);
    end;
  finally
    FreeAndNil(Reader);
  end;
end;

function TBGRABitmapCacheItem.GetImageObject: TObject;
begin
  Result := FImage;
end;

function TBGRABitmapCacheItem.GetImage: TBGRABitmap;
begin
  Result := FImage;
end;

{ Scales the image to the new size if the original size is different.
  This is needed to have all tiles at the same size. }
procedure TBGRABitmapCacheItem.StretchImageIfNeeded(var AImage: TBGRABitmap;
  ANewWidth, ANewHeight: Integer);
var
  img: TBGRABitmap;
begin
  if Assigned(AImage) then
    if (AImage.Width <> ANewWidth) or (AImage.Height <> ANewHeight) then
  begin
    img := AImage.Resample(ANewWidth, ANewHeight);
    AImage.Free;
    AImage := img;
  end;
end;

destructor TBGRABitmapCacheItem.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

destructor TMvBGRADrawingEngine.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TMvBGRADrawingEngine.CreateBuffer(AWidth, AHeight: Integer);
begin
  FreeAndNil(FBuffer);
  FBuffer := TBGRABitmap.Create(AWidth, AHeight);
  // Speeding up text output: https://github.com/bgrabitmap/bgrabitmap/issues/302
  FBuffer.FontRenderer := TBGRAVectorizedFontRenderer.Create;
end;

procedure TMvBGRADrawingEngine.DrawBitmap(X, Y: Integer;
  ABitmap: TCustomBitmap; UseAlphaChannel: Boolean);
var
  bmp: TBGRABitmap;
  img: TLazIntfImage;
begin
  if ABitmap is TBitmap then
    FBuffer.CanvasBGRA.Draw(X, Y, TBitmap(ABitmap))
  else
  begin
    img := ABitmap.CreateIntfImage;
    bmp := TBGRABitmap.Create(img);
    FBuffer.CanvasBGRA.Draw(X, Y, bmp);
    bmp.Free;
    img.Free;
  end;
end;

procedure TMvBGRADrawingEngine.DrawCacheItem(X, Y: Integer;
  AImg: TPictureCacheItem; ADrawMode: TItemDrawMode; AOpacity: Single);
var
  Img: TBGRABitmap;
begin
  Img := (AImg as TBGRABitmapCacheItem).Image;
  case ADrawMode of
    idmDraw: FBuffer.PutImage(x, y, Img, dmSet);
    idmUseOpacity: FBuffer.PutImage(x, y, Img, dmDrawWithTransparency, Round(AOpacity * 255));
    idmUseSourceAlpha: FBuffer.CanvasBGRA.Draw(x, y, Img);
  end;
end;

procedure TMvBGRADrawingEngine.DrawScaledCacheItem(DestRect, SrcRect: TRect;
  ASrcImg: TPictureCacheItem);
var
  SrcImg: TBGRABitmap;
begin
  SrcImg := (ASrcImg as TBGRABitmapCacheItem).Image;
  FBuffer.CanvasBGRA.CopyRect(DestRect, SrcImg, SrcRect);
end;

procedure TMvBGRADrawingEngine.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.CanvasBGRA.Ellipse(X1, Y1, X2, Y2);
end;

procedure TMvBGRADrawingEngine.FillPixels(X1, Y1, X2, Y2: Integer;
  AColor: TColor);
begin
  FBuffer.FillRect(X1, Y1, X2, Y2, AColor);
end;

procedure TMvBGRADrawingEngine.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.CanvasBGRA.FillRect(X1, Y1, X2, Y2);
end;

function TMvBGRADrawingEngine.GetPenStyle: TPenStyle;
begin
  Result := FBuffer.CanvasBGRA.Pen.Style;
end;

function TMvBGRADrawingEngine.GetBrushColor: TColor;
begin
  Result := FBuffer.CanvasBGRA.Brush.Color;
end;

function TMvBGRADrawingEngine.GetBrushStyle: TBrushStyle;
begin
  Result := FBuffer.CanvasBGRA.Brush.Style;
end;

function TMvBGRADrawingEngine.GetFontColor: TColor;
begin
  Result := FFontColor
end;

function TMvBGRADrawingEngine.GetFontName: String;
begin
  Result := FFontName;
end;

function TMvBGRADrawingEngine.GetFontOrientation: Single;
begin
  Result := FFontOrientation * 0.1;
end;

function TMvBGRADrawingEngine.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMvBGRADrawingEngine.GetFontStyle: TFontStyles;
begin
  Result := FFontStyle;
end;

function TMvBGRADrawingEngine.GetPenColor: TColor;
begin
  Result := FBuffer.CanvasBGRA.Pen.Color;
end;

function TMvBGRADrawingEngine.GetPenWidth: Integer;
begin
  Result := FBuffer.CanvasBGRA.Pen.Width
end;

function TMvBGRADrawingEngine.GetOpacity: Single;
begin
  Result := FOpacity;
end;

procedure TMvBGRADrawingEngine.SetOpacity(AValue: Single);
var
  A: Byte;
begin
  FOpacity := AValue;
  A := OpacityToAlpha(FOpacity);
  FBuffer.CanvasBGRA.Pen.BGRAColor.alpha := A;
  FBuffer.CanvasBGRA.Brush.BGRAColor.alpha := A;
end;

procedure TMvBGRADrawingEngine.SetPenStyle(AValue: TPenStyle);
begin
  FBuffer.CanvasBGRA.Pen.Style := AValue;
end;

procedure TMvBGRADrawingEngine.Line(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.CanvasBGRA.Line(X1, Y1, X2, Y2);
end;

procedure TMvBGRADrawingEngine.Polyline(const Points: array of TPoint);
begin
  FBuffer.CanvasBGRA.Polyline(Points);
end;

procedure TMvBGRADrawingEngine.Polygon(const Points: array of TPoint);
begin
  FBuffer.CanvasBGRA.Polygon(Points);
end;

procedure TMvBGRADrawingEngine.PolyBezier(const Points: array of TPoint;
  Filled: Boolean; Continuous: Boolean);
begin
  FBuffer.CanvasBGRA.PolyBezier(Points, Filled, Continuous);
end;

procedure TMvBGRADrawingEngine.PaintToCanvas(ACanvas: TCanvas; Origin: TPoint);
begin
  FBuffer.Draw(ACanvas, Origin.X, Origin.Y);
end;

procedure TMvBGRADrawingEngine.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.CanvasBGRA.Rectangle(X1, Y1, X2, Y2);
end;

function TMvBGRADrawingEngine.SaveToImage(AClass: TRasterImageClass): TRasterImage;
begin
  Result := AClass.Create;
  Result.Width := FBuffer.Width;
  Result.Height := FBuffer.Height;
  Result.Canvas.FillRect(0, 0, FBuffer.Width, FBuffer.Height);
  FBuffer.Draw(Result.Canvas, 0, 0);
end;

procedure TMvBGRADrawingEngine.SetBrushColor(AValue: TColor);
begin
  if AValue = clNone then
    FBuffer.CanvasBGRA.Brush.BGRAColor.Alpha := 0
  else
    FBuffer.CanvasBGRA.Brush.Color := ColorToRGB(AValue);
end;

procedure TMvBGRADrawingEngine.SetBrushStyle(AValue: TBrushStyle);
begin
  FBuffer.CanvasBGRA.Brush.Style := AValue;
end;

procedure TMvBGRADrawingEngine.SetFontColor(AValue: TColor);
begin
  FFontColor := AValue;
end;

procedure TMvBGRADrawingEngine.SetFontName(AValue: String);
begin
  FFontName := AValue;
end;

procedure TMvBGRADrawingEngine.SetFontOrientation(AValue: Single);
begin
  FFontOrientation := round(AValue * 10.0);
end;

procedure TMvBGRADrawingEngine.SetFontSize(AValue: Integer);
begin
  FFontSize := AValue;
end;

procedure TMvBGRADrawingEngine.SetFontStyle(AValue: TFontStyles);
begin
  FFontStyle := AValue;
end;

procedure TMvBGRADrawingEngine.SetPenColor(AValue: TColor);
begin
  FBuffer.CanvasBGRA.Pen.Color := AValue;
end;

procedure TMvBGRADrawingEngine.SetPenWidth(AValue: Integer);
begin
  FBuffer.CanvasBGRA.Pen.Width := AValue;
end;

procedure TMvBGRADrawingEngine.ApplyFont;
var
  fntSize: Integer;
begin
  FBuffer.CanvasBGRA.Font.Name := FFontName;
  if FFontSize = 0 then fntSize := 10 else fntSize := FFontSize;
  FBuffer.CanvasBGRA.Font.Height := -Round(ScreenInfo.PixelsPerInchY / 72.0  * fntSize);
  FBuffer.CanvasBGRA.Font.Style := FFontStyle;
  FBuffer.CanvasBGRA.Font.Color := FFontColor;
  FBuffer.CanvasBGRA.Font.Orientation := FFontOrientation;
  FBuffer.CanvasBGRA.Font.Antialiasing := true;
end;

function TMvBGRADrawingEngine.TextExtent(const AText: String;
  ARotated: Boolean = false): TSize;
var
  s, c: Double;
  pts: Array[0..3] of TPointF;
begin
  ApplyFont;
  Result := FBuffer.CanvasBGRA.TextExtent(AText);
  if (FFontOrientation <> 0) and ARotated then
  begin
    SinCos(FFontOrientation * pi / 1800, s, c);
    pts[0] := PointF(0, 0);
    pts[1] := RotatePointF(PointF(Result.CX, 0), s, c);
    pts[2] := RotatePointF(PointF(Result.CX, Result.CY), s, c);
    pts[3] := RotatePointF(PointF(0, Result.CY), s, c);
    Result.CX := round(
      MaxValue([pts[0].X, pts[1].X, pts[2].X, pts[3].X]) -
      MinValue([pts[0].X, pts[1].X, pts[2].X, pts[3].X])
    );
    Result.CY := round(
      MaxValue([pts[0].Y, pts[1].Y, pts[2].Y, pts[3].Y]) -
      MinValue([pts[0].Y, pts[1].Y, pts[2].Y, pts[3].Y])
    );
  end;
end;

procedure TMvBGRADrawingEngine.TextOut(X, Y: Integer; const AText: String);
var
  ext: TSize;
  dx, dy: Single;
  ctr: TPointF;
  R: TRectF;
  Pts: Array[0..3] of TPointF;
  s, c: Double;
begin
  if (AText <> '') then
  begin
    ApplyFont;
    if FFontOrientation = 0 then
    begin
      FBuffer.FontVerticalAnchor := fvaTop;
      FBuffer.CanvasBGRA.TextOut(X, Y, AText);
    end else
    begin
      ext := FBuffer.CanvasBGRA.TextExtent(AText);
      dx := ext.CX/2;
      dy := ext.CY/2;
      SinCos(FFontOrientation * pi / 1800, s, c);
      Pts[0] := RotatePointF(PointF(-dx, -dy), s, c);
      Pts[1] := RotatePointF(PointF(+dx, -dy), s, c);
      Pts[2] := RotatePointF(PointF(+dx, +dy), s, c);
      Pts[3] := RotatePointF(PointF(-dx, +dy), s, c);
      R := RectF(
        MinValue([Pts[0].X, Pts[1].X, Pts[2].X, Pts[3].X]),
        MinValue([Pts[0].Y, Pts[1].Y, Pts[2].Y, Pts[3].Y]),
        MaxValue([Pts[0].X, Pts[1].X, Pts[2].X, Pts[3].X]),
        MaxValue([Pts[0].Y, Pts[1].Y, Pts[2].Y, Pts[3].Y])
      );

      dx := R.Width/2;
      dy := R.Height/2;
      ctr := PointF(X + dx, Y + dy);
      FBuffer.CanvasBGRA.PolygonF([Pts[0] + ctr, Pts[1] + ctr, Pts[2] + ctr, Pts[3] + ctr], False, True);
      FBuffer.FontVerticalAnchor := fvaCapLine;
      FBuffer.TextOut(ctr.X + Pts[0].X, ctr.Y + Pts[0].Y, AText, FontColor);
    end;
  end;
end;
{
procedure TMvBGRADrawingEngine.TextOut(X, Y: Integer; const AText: String);
begin
  if (AText <> '') then
  begin
    ApplyFont;
    FBuffer.CanvasBGRA.TextOut(X, Y, AText);
  end;
end;
 }
function TMvBGRADrawingEngine.GetCacheItemClass: TPictureCacheItemClass;
begin
  Result := TBGRABitmapCacheItem;
end;

end.

