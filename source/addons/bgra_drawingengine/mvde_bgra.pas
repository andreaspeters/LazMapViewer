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

interface

uses
  Classes, SysUtils, Types, Graphics,
  mvDrawingEngine, mvCache,
  BGRAGraphics, BGRABitmap;

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
    constructor Create(APictureCacheItem : TPictureCacheItem); override;
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    property Image: TBGRABitmap read GetImage;
  end;




  { TMvBGRADrawingEngine }

  TMvBGRADrawingEngine = class(TMvCustomDrawingEngine)
  private
    FBuffer: TBGRABitmap;
    FFontName: String;
    FFontColor: TColor;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
  protected
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
    function TextExtent(const AText: String): TSize; override;
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

{ TBGRABitmapCacheItem }

function TBGRABitmapCacheItem.GetImageObject: TObject;
begin
  Result := FImage;
end;

function TBGRABitmapCacheItem.GetImage: TBGRABitmap;
begin
  Result := FImage;
end;


constructor TBGRABitmapCacheItem.Create(AStream: TStream);
var
  Reader: TFPCustomImageReader;
begin
  FImage := Nil;
  Reader := GetImageReader(AStream);
  if not Assigned(Reader) then
    raise EInvalidGraphic.Create('PNG/JPG expected.');
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

constructor TBGRABitmapCacheItem.Create(APictureCacheItem: TPictureCacheItem);
var
  src : TBGRABitmapCacheItem absolute APictureCacheItem;
begin
  inherited;
  if not (APictureCacheItem is TBGRABitmapCacheItem) then
    raise EMvCacheException.Create('Passed APictureCacheItem is not of type TBGRABitmapCacheItem!');
  FImage := TBGRABitmap.Create(src.FImage.Height, src.FImage.Width);
  FImage.Assign(src.FImage);
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
var
  savedColor: TColor;
begin
  savedColor := FBuffer.CanvasBGRA.Brush.Color;
  FBuffer.CanvasBGRA.Brush.Color := AColor;
  FillRect(X1, Y1, X2, Y2);
  FBuffer.CanvasBGRA.Brush.Color := savedColor;
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
var
  A: Byte;
begin
  A := FBuffer.CanvasBGRA.Pen.BGRAColor.alpha;
  if 255 = A
    then Result := 1.0
    else Result := Round(A / 255);
end;

procedure TMvBGRADrawingEngine.SetOpacity(AValue: Single);
var
  A: Byte;
begin
  A := Round(255 * AValue);
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
  FBuffer.CanvasBGRA.Brush.Color := AValue;
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

function TMvBGRADrawingEngine.TextExtent(const AText: String): TSize;
begin
  Result := FBuffer.CanvasBGRA.TextExtent(AText);
end;

procedure TMvBGRADrawingEngine.TextOut(X, Y: Integer; const AText: String);
begin
  if (AText <> '') then
  begin
    FBuffer.CanvasBGRA.Font.Name := FFontName;
    FBuffer.CanvasBGRA.Font.Height := -Round(ScreenInfo.PixelsPerInchY / 72.0  * FFontSize);
    FBuffer.CanvasBGRA.Font.Style := FFontStyle;
    FBuffer.CanvasBGRA.Font.Color := FFontColor;
    FBuffer.CanvasBGRA.Font.Antialiasing := true;
    FBuffer.CanvasBGRA.TextOut(X, Y, AText);
  end;
end;

function TMvBGRADrawingEngine.GetCacheItemClass: TPictureCacheItemClass;
begin
  Result := TBGRABitmapCacheItem;
end;

end.

