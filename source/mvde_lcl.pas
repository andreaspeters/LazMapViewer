{ Default drawing engine based on LCL-only routines
  Copyright (C) 2019 Werner Pamler (user wpat Lazarus forum https://forum.lazarus.freepascal.org)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDE_LCL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, IntfGraphics, fpReadJPEG, fpReadPNG,
  mvDrawingEngine, mvCache;

type

  { TLCLCacheItem }

  TLCLCacheItem = class(TPictureCacheItem)
  private
    FImage: TCustomBitmap;
    function GetImage: TCustomBitmap;
  protected
    function GetImageObject: TObject; override;
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    property Image: TCustomBitmap read GetImage;
  end;


  { TMvLCLDrawingEngine }

  TMvLCLDrawingEngine = class(TMvCustomDrawingEngine)
    private
      FBuffer: TBitmap;
    protected
      function GetBrushColor: TColor; override;
      function GetBrushStyle: TBrushStyle; override;
      function GetFontColor: TColor; override;
      function GetFontName: String; override;
      function GetFontSize: Integer; override;
      function GetFontStyle: TFontStyles; override;
      function GetPenColor: TColor; override;
      function GetPenStyle: TPenstyle; override;
      function GetPenWidth: Integer; override;
      function GetOpacity: Single; override;
      procedure SetOpacity({%H-}AValue: Single); override;
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
        {%H-}ADrawMode: TItemDrawMode = idmDraw; {%H-}AOpacity: Single = 1.0 ); override;
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
  LCLType, FPImage, mvTypes;

procedure Register;
begin
  RegisterComponents(PALETTE_PAGE, [TMvLCLDrawingEngine]);
end;

{ TLCLCacheItem }

function TLCLCacheItem.GetImage: TCustomBitmap;
begin
  Result := FImage;
end;

function TLCLCacheItem.GetImageObject: TObject;
begin
  Result := FImage;
end;

constructor TLCLCacheItem.Create(AStream: TStream);
var
  reader: TFPCustomImageReader;
begin
  FImage := Nil;
  reader := GetImageReader(AStream);
  if reader is TFPReaderJPEG then
    FImage := TJpegImage.Create
  else
  if reader is TFPReaderPNG then
    FImage := TPortableNetworkGraphic.Create
  else
    raise EInvalidGraphic.Create('PNG/JPG expected.');
  try
    try
      FImage.LoadFromStream(AStream);
    except
      FreeAndNil(FImage);
    end;
  finally
    reader.Free;
  end;
end;

destructor TLCLCacheItem.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;


{ TMvLCLDrawingEngine }

destructor TMvLCLDrawingEngine.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TMvLCLDrawingEngine.CreateBuffer(AWidth, AHeight: Integer);
begin
  FBuffer.Free;
  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32Bit;
  FBuffer.SetSize(AWidth, AHeight);
end;

procedure TMvLCLDrawingEngine.DrawBitmap(X, Y: Integer; ABitmap: TCustomBitmap;
  UseAlphaChannel: Boolean);
begin
  FBuffer.Canvas.Draw(X, Y, ABitmap);
end;

procedure TMvLCLDrawingEngine.DrawCacheItem(X, Y: Integer;
  AImg: TPictureCacheItem; ADrawMode: TItemDrawMode; AOpacity: Single);
var
  item: TLCLCacheItem;
begin
  item := AImg as TLCLCacheItem;
  FBuffer.Canvas.Draw(X, Y, item.Image);
end;

procedure TMvLCLDrawingEngine.DrawScaledCacheItem(DestRect,
  SrcRect: TRect; ASrcImg: TPictureCacheItem);
var
  item: TLCLCacheItem;
begin
  item := ASrcImg as TLCLCacheItem;
  FBuffer.Canvas.CopyRect(DestRect, item.Image.Canvas, SrcRect);
end;

procedure TMvLCLDrawingEngine.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.Canvas.Ellipse(X1,Y1, X2, Y2);
end;

procedure TMvLCLDrawingEngine.FillPixels(X1, Y1, X2, Y2: Integer; AColor: TColor);
begin
  if (X1 >= FBuffer.Width) or (X2 < 0) or (Y1 >= FBuffer.Height) or (Y2 < 0) then
    exit;

  if X1 < 0 then X1 := 0;
  if Y1 < 0 then Y1 := 0;
  if X2 >= FBuffer.Width then X2 := FBuffer.Width - 1;
  if Y2 >= FBuffer.Height then Y2 := FBuffer.Height - 1;

  FBuffer.Canvas.Brush.Color := AColor;
  FBuffer.Canvas.FillRect(X1, Y1, X2, Y2);
end;

procedure TMvLCLDrawingEngine.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.Canvas.FillRect(X1,Y1, X2, Y2);
end;

function TMvLCLDrawingEngine.GetBrushColor: TColor;
begin
  Result := FBuffer.Canvas.Brush.Color;
end;

function TMvLCLDrawingEngine.GetBrushStyle: TBrushStyle;
begin
  Result := FBuffer.Canvas.Brush.Style
end;

function TMvLCLDrawingEngine.GetCacheItemClass: TPictureCacheItemClass;
begin
  Result := TLCLCacheItem;
end;

function TMvLCLDrawingEngine.GetFontColor: TColor;
begin
  Result := FBuffer.Canvas.Font.Color
end;

function TMvLCLDrawingEngine.GetFontName: String;
begin
  Result := FBuffer.Canvas.Font.Name;
end;

function TMvLCLDrawingEngine.GetFontSize: Integer;
begin
  Result := FBuffer.Canvas.Font.Size;
end;

function TMvLCLDrawingEngine.GetFontStyle: TFontStyles;
begin
  Result := FBuffer.Canvas.Font.Style;
end;

function TMvLCLDrawingEngine.GetPenColor: TColor;
begin
  Result := FBuffer.Canvas.Pen.Color;
end;

function TMvLCLDrawingEngine.GetPenStyle: TPenstyle;
begin
  Result := FBuffer.Canvas.Pen.Style;
end;

function TMvLCLDrawingEngine.GetPenWidth: Integer;
begin
  Result := FBuffer.Canvas.Pen.Width;
end;

function TMvLCLDrawingEngine.GetOpacity: Single;
begin
  Result := 1.0;
end;

procedure TMvLCLDrawingEngine.SetOpacity(AValue: Single);
begin
  ;// TODO
end;

procedure TMvLCLDrawingEngine.Line(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.Canvas.Line(X1, Y1, X2, Y2);
end;

procedure TMvLCLDrawingEngine.Polyline(const Points: array of TPoint);
begin
  FBuffer.Canvas.Polyline(Points);
end;

procedure TMvLCLDrawingEngine.Polygon(const Points: array of TPoint);
begin
  FBuffer.Canvas.Polygon(Points);
end;

procedure TMvLCLDrawingEngine.PolyBezier(const Points: array of TPoint;
  Filled: Boolean; Continuous: Boolean);
begin
  FBuffer.Canvas.PolyBezier(Points, Filled, Continuous);
end;

procedure TMvLCLDrawingEngine.PaintToCanvas(ACanvas: TCanvas; Origin: TPoint);
begin
  ACanvas.Draw(Origin.X, Origin.Y, FBuffer);
end;

procedure TMvLCLDrawingEngine.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  FBuffer.Canvas.Rectangle(X1,Y1, X2, Y2);
end;

function TMvLCLDrawingEngine.SaveToImage(AClass: TRasterImageClass): TRasterImage;
begin
  Result := AClass.Create;
  Result.Width := FBuffer.Width;
  Result.Height := FBuffer.Height;
  Result.Canvas.FillRect(0, 0, Result.Width, Result.Height);
  Result.Canvas.Draw(0, 0, FBuffer);
end;

procedure TMvLCLDrawingEngine.SetBrushColor(AValue: TColor);
begin
  FBuffer.Canvas.Brush.Color := AValue;
end;

procedure TMvLCLDrawingEngine.SetBrushStyle(AValue: TBrushStyle);
begin
  FBuffer.Canvas.Brush.Style := AValue;
end;

procedure TMvLCLDrawingEngine.SetFontColor(AValue: TColor);
begin
  FBuffer.Canvas.Font.Color := AValue;
end;

procedure TMvLCLDrawingEngine.SetFontName(AValue: String);
begin
  FBuffer.Canvas.Font.Name := AValue;
end;

procedure TMvLCLDrawingEngine.SetFontSize(AValue: Integer);
begin
  FBuffer.Canvas.Font.Size := AValue;
end;

procedure TMvLCLDrawingEngine.SetFontStyle(AValue: TFontStyles);
begin
  FBuffer.Canvas.Font.Style := AValue;
end;

procedure TMvLCLDrawingEngine.SetPenColor(AValue: TColor);
begin
  FBuffer.Canvas.Pen.Color := AValue;
end;

procedure TMvLCLDrawingEngine.SetPenStyle(AValue: TPenStyle);
begin
  FBuffer.Canvas.Pen.Style := AValue;
end;

procedure TMvLCLDrawingEngine.SetPenWidth(AValue: Integer);
begin
  FBuffer.Canvas.Pen.Width := AValue;
end;

function TMvLCLDrawingEngine.TextExtent(const AText: String): TSize;
begin
  Result := FBuffer.Canvas.TextExtent(AText)
end;

procedure TMvLCLDrawingEngine.TextOut(X, Y: Integer; const AText: String);
begin
  if (AText <> '') then
    FBuffer.Canvas.TextOut(X, Y, AText);
end;

end.

