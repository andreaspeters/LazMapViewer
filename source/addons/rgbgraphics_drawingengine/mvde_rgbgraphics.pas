{
  Drawing engine for RGBGraphics library
    (C) 2014 ti_dic@hotmail.com

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDE_RGBGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics,
  mvTypes, mvDrawingEngine, mvCache,
  rgbGraphics, rgbTypes, rgbRoutines;

type

  { TRGB32BitmapCacheItem }

  TRGB32BitmapCacheItem = class(TPictureCacheItem)
  private
    FImage: TRGB32Bitmap;
    function GetImage: TRGB32Bitmap;
  protected
    function GetImageObject: TObject; override;
    procedure StretchImageIfNeeded(var AImage: TRGB32Bitmap; ANewWidth, ANewHeight: Integer);
  public
    constructor Create(ASource : TPictureCacheItem); override;
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    property Image: TRGB32Bitmap read GetImage;
  end;


  { TMvRGBGraphicsDrawingEngine }

  TMvRGBGraphicsDrawingEngine = class(TMvCustomDrawingEngine)
  private
    FBuffer: TRGB32Bitmap;
    FBrushStyle: TBrushStyle;
    FFontName: String;
    FFontColor: TColor;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FPenStyle: TPenStyle;
    FPenWidth: Integer;
    FOpacity: Single;
    FOpacityByte: Byte;
    FPenColor: TRGB32Pixel;
    FBrushColor: TRGB32Pixel;
    FLineProc: TLineDrawProc;
    procedure SettleLineProc; inline;
  protected
    procedure LineDrawOpacity(X1, Y1, X2, Y2: Integer);
    procedure PixelDrawOpacity(X, Y: Integer);
    procedure DrawBitmapOT(X, Y: Integer; ABitmap: TCustomBitmap; AOpaqueColor, ATransparentColor: TColor);
    function GetBrushColor: TColor; override;
    function GetBrushStyle: TBrushStyle; override;
    function GetFontColor: TColor; override;
    function GetFontName: String; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TFontStyles; override;
    function GetPenColor: TColor; override;
    function GetPenStyle: TPenStyle; override;
    function GetPenWidth: Integer; override;
    function GetOpacity: Single; override;
    procedure SetOpacity(AValue: Single); override;
    procedure SetBrushColor(AValue: TColor); override;
    procedure SetBrushStyle(AValue: TBrushStyle); override;
    procedure SetFontColor(AValue: TColor); override;
    procedure SetFontName(AValue: String); override;
    procedure SetFontSize(AValue: Integer); override;
    procedure SetFontStyle(AValue: TFontStyles); override;
    procedure SetPenColor(AValue: TColor); override;
    procedure SetPenStyle(AValue: TPenStyle); override;
    procedure SetPenWidth(AValue: Integer); override;
    procedure SetDrawMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateBuffer(AWidth, AHeight: Integer); override;
    procedure DrawBitmap(X, Y: Integer; ABitmap: TCustomBitmap;
      UseAlphaChannel: Boolean); override;
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
    function TextExtent(const AText: String): TSize; override;
    procedure TextOut(X, Y: Integer; const AText: String); override;
    function GetCacheItemClass: TPictureCacheItemClass; override;
  end;

procedure Register;


implementation

uses
  GraphType, LCLType, FPImage, Math;

procedure Register;
begin
  RegisterComponents(PALETTE_PAGE, [TMvRGBGraphicsDrawingEngine]);
end;

{ We must duplicate some routines from rgbTypes which are not accessible in the implementation section. }

{$if defined(LCLWin32) or defined(LCLWin64)}
  {$define RGB}
{$endif}
{$ifdef LCLqt}
  {$define RGB}
{$endif}
{$ifdef LCLqt5}
  {$define RGB}
{$endif}
{$ifdef LCLqt6}
  {$define RGB}
{$endif}

function GetRedInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF RGB}
  Result := (P and $FF0000) shr 16;
  {$ELSE}
  Result := P and $FF;
  {$ENDIF}
end;

function GetGreenInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF RGB}
  Result := (P and $FF00) shr 8;
  {$ELSE}
  Result := (P and $FF00) shr 8;
  {$ENDIF}
end;

function GetBlueInline(P: TRGB32Pixel): Byte; inline;
begin
  {$IFDEF RGB}
  Result := P and $FF;
  {$ELSE}
  Result := (P and $FF0000) shr 16;
  {$ENDIF}
end;

function RGBToRGB32PixelInline(R, G, B: Byte): TRGB32Pixel; inline;
begin
  {$IFDEF RGB}
  Result := B or (G shl 8) or (R shl 16);
  {$ELSE}
  Result := R or (G shl 8) or (B shl 16);
  {$ENDIF}
end;

function ColorToRGB32PixelInline(C: TColor): TRGB32Pixel; inline;
begin
  {$IFDEF RGB}
  Result := ((C and $FF0000) shr 16) or (C and $FF00) or ((C and $FF) shl 16);
  {$ELSE}
  Result := C and $FFFFFF;
  {$ENDIF}
end;

function CalculateGray(P: TRGB32Pixel): Byte; inline;
var
  r, g, b: Byte;
begin
  r := GetRedInline(P);
  g := GetGreenInline(P);
  b := GetBlueInline(P);
  Result := round(0.299 * r + 0.587 * g + 0.114 * b);
end;

// P1 points to background pixel, P2 to overlay image pixel
procedure AlphaBlendRGB32Pixel(Alpha: Byte; P1, P2, AResult: PRGB32Pixel); inline;
var
  r1, g1, b1: Byte;
  r2, g2, b2: Byte;
begin
  r1 := GetRedInline(P1^);
  g1 := GetGreenInline(P1^);
  b1 := GetBlueInline(P1^);

  r2 := GetRedInline(P2^);
  g2 := GetGreenInline(P2^);
  b2 := GetBlueInline(P2^);

  r1 := (word((255 - Alpha) * r1) + word(Alpha) * r2) shr 8;
  g1 := (word((255 - Alpha) * g1) + word(Alpha) * g2) shr 8;
  b1 := (word((255 - Alpha) * b1) + word(Alpha) * b2) shr 8;

  AResult^ := RGBToRGB32PixelInline(r1, g1, b1);
end;

{ Alpha-blends the bitmap into the background image with the specified alpha
  for the bitmap.
  When AAlpha = -1 it is assumed that ABitmap itself contains an alpha-channel
  to be used for the blending process. }
procedure AlphaBlendImages(ABackground, ABitmap: TRGB32Bitmap; X, Y: Integer;
  AAlpha: Integer = -1);
var
  BkgX, BkgY, XMax, YMax, I, J, ImgX, ImgY: Integer;
  P1, P2: PRGB32Pixel;
  alpha: PRGB8Pixel;
begin
  BkgX := Max(0, X);
  BkgY := Max(0, Y);
  ImgX := Max(0, -X);
  ImgY := Max(0, -Y);
  XMax := Min(ABitmap.Width - ImgX, ABackground.Width - BkgX) - 1;
  YMax := Min(ABitmap.Height - ImgY, ABackground.Height - BkgY) - 1;

  for J := YMax downto 0 do
  begin
    P1 := ABackground.Get32PixelPtr(BkgX, BkgY + J);
    P2 := ABitmap.Get32PixelPtr(ImgX, ImgY + J);
    if AAlpha >= 0 then
      for I := 0 to XMax do
      begin
        AlphaBlendRGB32Pixel(Byte(AAlpha), P1, P2, P1);
        Inc(P1);
        Inc(P2);
      end
    else
    begin
      alpha := ABitmap.Mask.Get8PixelPtr(ImgX, ImgY + J);
      for I := 0 to XMax do
      begin
        AlphaBlendRGB32Pixel(alpha^, P1, P2, P1);
        Inc(P1);
        Inc(P2);
        Inc(alpha);
      end;
    end;
  end;
end;


{ TRGB32BitmapCacheItem }

function TRGB32BitmapCacheItem.GetImage: TRGB32Bitmap;
begin
  Result := FImage;
end;

function TRGB32BitmapCacheItem.GetImageObject: TObject;
begin
  Result := FImage;
end;

constructor TRGB32BitmapCacheItem.Create(AStream: TStream);
var
  Reader: TFPCustomImageReader;
begin
  FImage := Nil;
  Reader := GetImageReader(AStream);
  if not Assigned(Reader) then
    raise EInvalidGraphic.Create('PNG/JPG expected.');
  try
    try
      FImage := TRGB32Bitmap.CreateFromStream(AStream, Reader);

      // Make sure that all tiles have the size defined by TileSize.
      StretchImageIfNeeded(FImage, TileSize.CX, TileSize.CY);
    except
      FreeAndNil(FImage);
    end;
  finally
    FreeAndNil(Reader);
  end;
end;

destructor TRGB32BitmapCacheItem.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

{ Scales the image to the new size if the original size is different.
  This is needed to have all tiles at the same size. }
procedure TRGB32BitmapCacheItem.StretchImageIfNeeded(var AImage: TRGB32Bitmap;
  ANewWidth, ANewHeight: Integer);
begin
  if Assigned(AImage) then
    if (AImage.Width <> ANewWidth) or (AImage.Height <> ANewHeight) then
      AImage.StretchTrunc(ANewWidth, ANewHeight);
end;

constructor TRGB32BitmapCacheItem.Create(ASource: TPictureCacheItem);
var
  src : TRGB32BitmapCacheItem absolute ASource;
begin
  inherited;
  if not (ASource is TRGB32BitmapCacheItem) then
    raise EMvCacheException.Create('Passed APictureCacheItem is not of type TRGB32BitmapCacheItem!');
  FImage := TRGB32Bitmap.Create(src.FImage.Width, src.FImage.Height);
  FImage.Assign(src.FImage);
end;

destructor TMvRGBGraphicsDrawingEngine.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TMvRGBGraphicsDrawingEngine.CreateBuffer(AWidth, AHeight: Integer);
begin
  FreeAndNil(FBuffer);
  FBuffer := TRGB32Bitmap.Create(AWidth, AHeight);
end;

procedure TMvRGBGraphicsDrawingEngine.DrawBitmap(X, Y: Integer;
  ABitmap: TCustomBitmap; UseAlphaChannel: Boolean);
var
  bmp32: TRGB32Bitmap;
begin
  bmp32 := TRGB32Bitmap.CreateFromBitmap(ABitmap);
  try
    if UseAlphaChannel then
      AlphaBlendImages(FBuffer, bmp32, X, Y)
    else
      FBuffer.Draw(X, Y, bmp32);
  finally
    bmp32.Free;
  end;
end;

procedure TMvRGBGraphicsDrawingEngine.SettleLineProc;
begin
  if 255 = FOpacityByte
    then FLineProc := @FBuffer.Canvas.Line
    else FLineProc := @LineDrawOpacity;
end;

procedure TMvRGBGraphicsDrawingEngine.LineDrawOpacity(X1, Y1, X2, Y2: Integer);
begin
  LineBresenham(X1, Y1, X2, Y2, @PixelDrawOpacity);
end;

procedure TMvRGBGraphicsDrawingEngine.PixelDrawOpacity(X, Y: Integer);
var
  P: PRGB32Pixel;
begin
  P := FBuffer.Get32PixelPtr(X, Y);
  if (P <> Nil)
    then AlphaBlendRGB32Pixel(FOpacityByte, P, @FPenColor, P)
    else FBuffer.Set32Pixel(X, Y, FPenColor)
end;

procedure TMvRGBGraphicsDrawingEngine.DrawBitmapOT(X, Y: Integer;
  ABitmap: TCustomBitmap; AOpaqueColor, ATransparentColor: TColor);
var
  bmp32: TRGB32Bitmap;
  BkgX, BkgY, XMax, YMax, I, J, ImgX, ImgY: Integer;
  P1, P2: PRGB32Pixel;
  alpha: PRGB8Pixel;
  transpPixel, opaquePixel, workPixel: TRGB32Pixel;
  gray, bkGray: Word;
begin
  BkgX := Max(0, X);
  BkgY := Max(0, Y);
  ImgX := Max(0, -X);
  ImgY := Max(0, -Y);
  XMax := Min(ABitmap.Width - ImgX, FBuffer.Width - BkgX) - 1;
  YMax := Min(ABitmap.Height - ImgY, FBuffer.Height - BkgY) - 1;
  opaquePixel := ColorToRGB32PixelInline(AOpaqueColor);
  transpPixel := ColorToRGB32PixelInline(ATransparentColor);
  bkGray := CalculateGray(transpPixel);

  bmp32 := TRGB32Bitmap.CreateFromBitmap(ABitmap);
  try
    for J := YMax downto 0 do
    begin
      P1 := FBuffer.Get32PixelPtr(BkgX, BkgY + J);
      P2 := bmp32.Get32PixelPtr(ImgX, ImgY + J);
      alpha := bmp32.Mask.Get8PixelPtr(ImgX, ImgY + J);
      for I := 0 to XMax do
      begin
        workPixel := P2^ and $00FFFFFF;
        if workPixel = transpPixel then
          alpha^ := 0
        else
        if workPixel = opaquePixel then
          alpha^ := 255
        else
        begin
          gray := CalculateGray(P2^);
          workPixel := opaquePixel;
          if gray > bkGray then
            alpha^ := gray - bkGray
          else
            alpha^ := bkGray - gray;
        end;
        AlphaBlendRGB32Pixel(alpha^, P1, @workPixel, P1);
        inc(P1);
        inc(P2);
        inc(alpha);
      end;
    end;
  finally
    bmp32.Free;
  end;
end;

procedure TMvRGBGraphicsDrawingEngine.DrawCacheItem(X, Y: Integer;
  AImg: TPictureCacheItem; ADrawMode: TItemDrawMode; AOpacity: Single);
var
  Item: TRGB32BitmapCacheItem;
begin
  Item := (AImg as TRGB32BitmapCacheItem);
  case ADrawMode of
    idmDraw:
      FBuffer.Draw(X, Y, Item.Image);
    idmUseOpacity:
      AlphaBlendImages(FBuffer, Item.Image, X, Y, round(AOpacity * 255));
    idmUseSourceAlpha:
      AlphaBlendImages(FBuffer, Item.Image, X, Y);
  end;
end;

procedure TMvRGBGraphicsDrawingEngine.DrawScaledCacheItem(DestRect,
  SrcRect: TRect; ASrcImg: TPictureCacheItem);
var
  srcBmp, SrcImg: TRGB32Bitmap;
  y, w, ww, h: Integer;
begin
  w := SrcRect.Right - SrcRect.Left;
  h := SrcRect.Bottom - SrcRect.Top;
  SrcImg := (ASrcImg as TRGB32BitmapCacheItem).Image;
  srcBmp := TRGB32Bitmap.Create(w, h);
  try
    ww := w * SizeOf(TRGB32Pixel);
    for y := 0 to h-1 do
      Move(SrcImg.Get32PixelPtr(SrcRect.Left, SrcRect.Top + y)^,
        srcBmp.Get32PixelPtr(0, y)^, ww);
    srcBmp.StretchTrunc(DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top);
    FBuffer.Draw(DestRect.Left, DestRect.Top, srcBmp);
  finally
    srcBmp.Free;
  end;
end;

procedure TMvRGBGraphicsDrawingEngine.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.Canvas.Ellipse(X1, Y1, X2, Y2);
end;

procedure TMvRGBGraphicsDrawingEngine.FillPixels(X1, Y1, X2, Y2: Integer;
  AColor: TColor);
var
  x, y: Integer;
  c: TRGB32Pixel;
begin
  if (X1 >= FBuffer.Width) or (X2 < 0) or (Y1 >= FBuffer.Height) or (Y2 < 0) then
    exit;

  if X1 < 0 then X1 := 0;
  if Y1 < 0 then Y1 := 0;
  if X2 >= FBuffer.Width then X2 := FBuffer.Width - 1;
  if Y2 >= FBuffer.Height then Y2 := FBuffer.Height - 1;

  c := ColorToRGB32Pixel(AColor);
  for y := Y1 to Y2 do
    for x := X1 to X2 do
      FBuffer.Set32Pixel(x, y, c);
end;

procedure TMvRGBGraphicsDrawingEngine.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.Canvas.FillRect(X1, Y1, X2, Y2);
end;

function TMvRGBGraphicsDrawingEngine.GetPenStyle: TPenStyle;
begin
  Result := FPenStyle;
end;

function TMvRGBGraphicsDrawingEngine.GetBrushColor: TColor;
begin
  Result := FBuffer.Canvas.FillColor;
end;

function TMvRGBGraphicsDrawingEngine.GetBrushStyle: TBrushStyle;
begin
  Result := FBrushStyle;
end;

function TMvRGBGraphicsDrawingEngine.GetFontColor: TColor;
begin
  Result := FFontColor;
end;

function TMvRGBGraphicsDrawingEngine.GetFontName: String;
begin
  Result := FFontName;
end;

function TMvRGBGraphicsDrawingEngine.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMvRGBGraphicsDrawingEngine.GetFontStyle: TFontStyles;
begin
  Result := FFontStyle;
end;

function TMvRGBGraphicsDrawingEngine.GetPenColor: TColor;
begin
  Result := FBuffer.Canvas.OutlineColor;
end;

function TMvRGBGraphicsDrawingEngine.GetPenWidth: Integer;
begin
  Result := FPenWidth;
end;

function TMvRGBGraphicsDrawingEngine.GetOpacity: Single;
begin
  Result := FOpacity;
end;

procedure TMvRGBGraphicsDrawingEngine.SetOpacity(AValue: Single);
begin
  if AValue = FOpacity then
    Exit;
  FOpacity := AValue;
  FOpacityByte := Round(255 * AValue);
end;

procedure TMvRGBGraphicsDrawingEngine.SetPenStyle(AValue: TPenStyle);
begin
  FPenStyle := AValue;
  SetDrawMode;
end;

procedure TMvRGBGraphicsDrawingEngine.Line(X1, Y1, X2, Y2: Integer);
var
  MY, MX: Double;
  PWX, PWY: Integer;
begin
  SettleLineProc;
  if FPenWidth = 1 then
    FLineProc(X1, Y1, X2, Y2)
  else
  begin
    if not OrthoVec(X1, Y1, X2, Y2, MX, MY) then
      Exit;
    PWX := Trunc(FPenWidth * MX * 0.5);
    PWY := Trunc(FPenWidth * MY * 0.5);
    DoScanFill([
      Point(X1 - PWX, Y1 - PWY), Point(X1 + PWX, Y1 + PWY),
      Point(X2 + PWX, Y2 + PWY), Point(X2 - PWX, Y2 - PWY)
    ], FLineProc);
  end;
end;

procedure TMvRGBGraphicsDrawingEngine.Polyline(const Points: array of TPoint);
var
  I: Integer;
begin
  for I := 1 to High(Points) do
    Line(Points[I - 1].X, Points[I - 1].Y, Points[I].X, Points[I].Y);
end;

procedure TMvRGBGraphicsDrawingEngine.Polygon(const Points: array of TPoint);
var
  OldColor: TColor;
  OldWidth: Integer;
  OldStyle: TPenStyle;
begin
  if BrushStyle <> bsClear then
  begin
    SettleLineProc;
    OldColor := PenColor;
    OldWidth := PenWidth;
    OldStyle := PenStyle;
    try
      PenColor := BrushColor;
      PenWidth := 1;
      PenStyle := psSolid;
      DoScanFill(Points, FLineProc);
    finally
      PenColor := OldColor;
      PenWidth := OldWidth;
      PenStyle := OldStyle;
    end;
  end;
  if PenStyle <> psClear then
  begin
    Polyline(Points);
    if Length(Points) > 1 then
      Line(Points[High(Points)].X, Points[High(Points)].Y, Points[0].X, Points[0].Y);
  end;
end;

procedure TMvRGBGraphicsDrawingEngine.PolyBezier(const Points: array of TPoint;
  Filled: Boolean; Continuous: Boolean);
var
  PtDyn: array of TPoint;
begin
  CalcBezier(Points, Continuous, PtDyn);
  if Filled
    then Polygon(PtDyn)
    else Polyline(PtDyn);
end;

procedure TMvRGBGraphicsDrawingEngine.PaintToCanvas(ACanvas: TCanvas;
  Origin: TPoint);
begin
  FBuffer.Canvas.DrawTo(ACanvas, Origin.X, Origin.Y);
end;

procedure TMvRGBGraphicsDrawingEngine.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.Canvas.Rectangle(X1, Y1, X2, Y2);
end;

function TMvRGBGraphicsDrawingEngine.SaveToImage(AClass: TRasterImageClass): TRasterImage;
begin
  Result := AClass.Create;
  Result.Width := FBuffer.Width;
  Result.Height := FBuffer.Height;
  Result.Canvas.FillRect(0, 0, FBuffer.Width, FBuffer.Height);
  FBuffer.Canvas.DrawTo(Result.Canvas, 0, 0);
end;

procedure TMvRGBGraphicsDrawingEngine.SetBrushColor(AValue: TColor);
begin
  FBuffer.Canvas.FillColor := AValue;
  FBrushColor := ColorToRGB32PixelInline(AValue);
end;

procedure TMvRGBGraphicsDrawingEngine.SetBrushStyle(AValue: TBrushStyle);
begin
  FBrushStyle := AValue;
  SetDrawMode;
  // No direct brush style support in RGB32Bitmap
end;

procedure TMvRGBGraphicsDrawingEngine.SetFontColor(AValue: TColor);
begin
  FFontColor := AValue;
end;

procedure TMvRGBGraphicsDrawingEngine.SetFontName(AValue: String);
begin
  FFontName := AValue;
end;

procedure TMvRGBGraphicsDrawingEngine.SetFontSize(AValue: Integer);
begin
  FFontSize := AValue;
end;

procedure TMvRGBGraphicsDrawingEngine.SetFontStyle(AValue: TFontStyles);
begin
  FFontStyle := AValue;
end;

procedure TMvRGBGraphicsDrawingEngine.SetPenColor(AValue: TColor);
begin
  FBuffer.Canvas.OutlineColor := AValue;
  FPenColor := ColorToRGB32PixelInline(AValue);
end;

procedure TMvRGBGraphicsDrawingEngine.SetPenWidth(AValue: Integer);
begin
  FPenWidth := AValue;
end;

procedure TMvRGBGraphicsDrawingEngine.SetDrawMode;
var
  B, P: Boolean;
begin
  B := FBrushStyle <> bsClear;
  P := FPenStyle <> psClear;
  if P and B then
    FBuffer.Canvas.DrawMode := dmFillAndOutline
  else if B then
    FBuffer.Canvas.DrawMode := dmFill
  else // if P then
    FBuffer.Canvas.DrawMode := dmOutline;
end;

constructor TMvRGBGraphicsDrawingEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Opacity := 1.0;
end;

function TMvRGBGraphicsDrawingEngine.TextExtent(const AText: String): TSize;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Name := FFontName;
    bmp.Canvas.Font.Size := FFontSize;
    bmp.Canvas.Font.Style := FFontStyle;
    Result := bmp.Canvas.TextExtent(AText);
  finally
    bmp.Free;
  end;
end;

procedure TMvRGBGraphicsDrawingEngine.TextOut(X, Y: Integer; const AText: String);
var
  bmp: TBitmap;
  ex: TSize;
begin
  if (AText = '') then
    exit;
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24Bit;    // Does not work with 32 bpp...
    bmp.Canvas.Font.Name := FFontName;
    bmp.Canvas.Font.Size := FFontSize;
    bmp.Canvas.Font.Style := FFontStyle;
    bmp.Canvas.Font.Color := FFontColor;
    ex := bmp.Canvas.TextExtent(AText);
    bmp.SetSize(ex.CX, ex.CY);
    if GetBrushStyle <> bsClear then begin
      bmp.Canvas.Brush.Color := GetBrushColor;
      bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
      bmp.Canvas.TextOut(0, 0, AText);
      DrawBitmap(X, Y, bmp, false);
    end else
    begin
      if FFontColor = clWhite then
        bmp.Canvas.Brush.Color := clBlack
      else
        bmp.Canvas.Brush.Color := clWhite;
      bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
      bmp.Canvas.TextOut(0, 0, AText);
      DrawBitmapOT(X, Y, bmp, FFontColor, bmp.Canvas.Brush.Color);
    end;
  finally
    bmp.Free;
  end;
end;

function TMvRGBGraphicsDrawingEngine.GetCacheItemClass: TPictureCacheItemClass;
begin
  Result := TRGB32BitmapCacheItem;
end;

end.

