{ Drawing engine based on Lazarus IntfGraphics routines
  (C) 2014 Werner Pamler (user wp at Lazarus forum https://forum.lazarus.freepascal.org)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDE_IntfGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, LclVersion,
  FPImage, FPCanvas, IntfGraphics, LazCanvas,
  mvDrawingEngine, mvCache;

type

  { TLazIntfImageCacheItem }

  TLazIntfImageCacheItem = class(TPictureCacheItem)
  private
    FImage: TLazIntfImage;
    function GetImage: TLazIntfImage;
  protected
    function GetImageObject: TObject; override;
    procedure StretchImageIfNeeded(var AImage: TLazIntfImage; ANewWidth, ANewHeight: Integer);
  public
    constructor Create(ASource : TPictureCacheItem); override;
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    property Image: TLazIntfImage read GetImage;
  end;


  { TMvIntfGraphicsDrawingEngine }

  TMvIntfGraphicsDrawingEngine = class(TMvCustomDrawingEngine)
  private
    FBuffer: TLazIntfImage;
    FCanvas: TFPCustomCanvas;
    FFontName: String;
    FFontColor: TColor;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FOpacity: Single;
    procedure CreateLazIntfImageAndCanvas(out ABuffer: TLazIntfImage;
      out ACanvas: TFPCustomCanvas; AWidth, AHeight: Integer);
    procedure AddAlphaToColors;
  protected
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateBuffer(AWidth, AHeight: Integer); override;
    procedure DrawBitmap(X, Y: Integer; ABitmap: TCustomBitmap;
      UseAlphaChannel: Boolean); override;
    procedure DrawCacheItem(X, Y: Integer; AImg: TPictureCacheItem;
      ADrawMode: TItemDrawMode = idmDraw; AOpacity: Single = 1.0 ); override;
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


implementation

uses
  LCLType, LCLIntf,
  FPImgCanv, GraphType,
  mvTypes, FPReadJPEG, Math;

function InRange(x, min, max: Integer): Boolean;
begin
  Result := (x >= min) and (x <= max);
end;


{$IF Lcl_FullVersion < 1090000}

function IfThen(ACondition: Boolean; a, b: Integer): Integer;
begin
  if ACondition then Result := a else Result := b;
end;

// Workaround for http://mantis.freepascal.org/view.php?id=27144
procedure CopyPixels(ASource, ADest: TLazIntfImage;
  XDst: Integer = 0; YDst: Integer = 0;
  AlphaMask: Boolean = False; AlphaTreshold: Word = 0);
var
  SrcHasMask, DstHasMask: Boolean;
  x, y, xStart, yStart, xStop, yStop: Integer;
  c: TFPColor;
  SrcRawImage, DestRawImage: TRawImage;
begin
  ASource.GetRawImage(SrcRawImage);
  ADest.GetRawImage(DestRawImage);

  if DestRawImage.Description.IsEqual(SrcRawImage.Description) and (XDst =  0) and (YDst = 0) then
  begin
    // same description -> copy
    if DestRawImage.Data <> nil then
      System.Move(SrcRawImage.Data^, DestRawImage.Data^, DestRawImage.DataSize);
    if DestRawImage.Mask <> nil then
      System.Move(SrcRawImage.Mask^, DestRawImage.Mask^, DestRawImage.MaskSize);
    Exit;
  end;

  // copy pixels
  XStart := IfThen(XDst < 0, -XDst, 0);
  YStart := IfThen(YDst < 0, -YDst, 0);
  XStop := IfThen(ADest.Width - XDst < ASource.Width, ADest.Width - XDst, ASource.Width) - 1;
  YStop := IfTHen(ADest.Height - YDst < ASource.Height, ADest.Height - YDst, ASource.Height) - 1;

  SrcHasMask := SrcRawImage.Description.MaskBitsPerPixel > 0;
  DstHasMask := DestRawImage.Description.MaskBitsPerPixel > 0;

  if DstHasMask then begin
    for y:= yStart to yStop do
      for x:=xStart to xStop do
        ADest.Masked[x+XDst,y+YDst] := SrcHasMask and ASource.Masked[x,y];
  end;

  for y:=yStart to yStop do
    for x:=xStart to xStop do
    begin
      c := ASource.Colors[x,y];
      if not DstHasMask and SrcHasMask and (c.alpha = $FFFF) then // copy mask to alpha channel
        if ASource.Masked[x,y] then
          c.alpha := 0;

      ADest.Colors[x+XDst,y+YDst] := c;
      if AlphaMask and (c.alpha < AlphaTreshold) then
        ADest.Masked[x+XDst,y+YDst] := True;
    end;
end;

{$IFEND}

{ TLazIntfImageCacheItem }

function TLazIntfImageCacheItem.GetImage: TLazIntfImage;
begin
  Result := FImage;
end;

function TLazIntfImageCacheItem.GetImageObject: TObject;
begin
  Result := FImage;
end;

constructor TLazIntfImageCacheItem.Create(AStream: TStream);
var
  reader: TFPCustomImageReader;
  rawImg: TRawImage;
begin
  FImage := Nil;
  Reader := GetImageReader(AStream);
  if not Assigned(Reader) then
    raise EInvalidGraphic.Create('PNG/JPG expected.');
  try
    rawImg.Init;
    rawImg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(0, 0);
    FImage := TLazIntfImage.Create(rawImg, True);
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

destructor TLazIntfImageCacheItem.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

{ Scales the image to the new size if the original size is different.
  This is needed to have all tiles at the same size. }
procedure TLazIntfImageCacheItem.StretchImageIfNeeded(var AImage: TLazIntfImage;
  ANewWidth, ANewHeight: Integer);
var
  img: TLazIntfImage;
  canv: TLazCanvas;
begin
  if AImage = nil then
    exit;

  if (AImage.Width <> ANewWidth) or (AImage.Height <> ANewHeight) then
  begin
    img := TLazIntfImage.CreateCompatible(AImage, ANewWidth, ANewHeight);
    canv := TLazCanvas.Create(img);
    try
      canv.Interpolation := TFPSharpInterpolation.Create;
      canv.StretchDraw(0, 0, ANewWidth, ANewHeight, AImage);
      AImage.Free;
      AImage := img;
    finally
      canv.Interpolation.Free;
      canv.Interpolation := nil;
      canv.Free;
    end;
  end;
end;

constructor TLazIntfImageCacheItem.Create(ASource: TPictureCacheItem);
var
  src : TLazIntfImageCacheItem absolute ASource;
begin
  inherited;
  if not (ASource is TLazIntfImageCacheItem) then
    raise EMvCacheException.Create('Passed APictureCacheItem is not of type TLazIntfImageCacheItem!');
  FImage := TLazIntfImage.CreateCompatible(src.FImage,src.FImage.Width,src.FImage.Height);
  FImage.Assign(src.FImage);
end;


{  TMvIntfGraphicsDrawingengine  }

destructor TMvIntfGraphicsDrawingEngine.Destroy;
begin
  FCanvas.Free;
  FBuffer.Free;
  inherited;
end;

procedure TMvIntfGraphicsDrawingEngine.CreateBuffer(AWidth, AHeight: Integer);
begin
  FCanvas.Free;
  FBuffer.Free;
  CreateLazIntfImageAndCanvas(FBuffer, FCanvas, AWidth, AHeight);
end;

procedure TMvIntfGraphicsDrawingEngine.CreateLazIntfImageAndCanvas(
  out ABuffer: TLazIntfImage;
  out ACanvas: TFPCustomCanvas; AWidth, AHeight: Integer);
var
  rawImg: TRawImage;
begin
  rawImg.Init;
  {$IFDEF DARWIN}
  rawImg.Description.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth, AHeight);
  {$ELSE}
  rawImg.Description.Init_BPP32_B8G8R8_BIO_TTB(AWidth, AHeight);
  // No alpha-channel for buffer image since it will be drawn on the MapView canvas.
  {$ENDIF}
  rawImg.CreateData(True);
  ABuffer := TLazIntfImage.Create(rawImg, true);
  ACanvas := TFPImageCanvas.Create(ABuffer);
  {ACanvas.Brush.FPColor}BrushColor := clWhite;
  ACanvas.FillRect(0, 0, AWidth, AHeight);
end;

procedure TMvIntfGraphicsDrawingEngine.AddAlphaToColors;
var
  A: Word;
begin
  if not Assigned(FCanvas) then
    Exit;
  with FCanvas do
    if FOpacity > 0.99 then
      DrawingMode := dmOpaque
    else
    begin
      A := Round($FFFF * FOpacity);
      Pen.FPColor := FPColor(Pen.FPColor.Red, Pen.FPColor.Green, Pen.FPColor.Blue, A);
      Brush.FPColor := FPColor(Brush.FPColor.Red, Brush.FPColor.Green, Brush.FPColor.Blue, A);
      //FFontColor := ;
      DrawingMode := dmAlphaBlend;
    end;
end;

function TMvIntfGraphicsDrawingEngine.GetPenStyle: TPenStyle;
begin
  if FCanvas <> Nil
    then Result := TPenStyle(FCanvas.Pen.Style)
    else Result := psClear;
end;

procedure TMvIntfGraphicsDrawingEngine.DrawBitmap(X, Y: Integer;
  ABitmap: TCustomBitmap; UseAlphaChannel: Boolean);
var
  intfImg: TLazIntfImage;
  i, j, iX, jY: Integer;
  cimg, cbuf: TFPColor;
begin
  intfImg := ABitmap.CreateIntfImage;
  try
    if UseAlphaChannel then
    begin
      for j := 0 to intfImg.Height - 1 do
      begin
        jY := j + Y;
        if InRange(jY, 0, FBuffer.Height - 1) then
          for i := 0 to intfImg.Width - 1 do
          begin
            iX := i + X;
            if InRange(iX, 0, FBuffer.Width-1) then
            begin
              cimg := intfImg.Colors[i, j];
              cbuf := FBuffer.Colors[iX, jY];
              FBuffer.Colors[iX, jY] := AlphaBlend(cbuf, cimg);
            end;
          end;
      end;
    end else
    begin
      for j := 0 to intfImg.Height - 1 do
      begin
        jY := j + Y;
        if InRange(jY, 0, FBuffer.Height - 1) then
          for i := 0 to intfImg.Width - 1 do
          begin
            ix := i + X;
            if InRange(iX, 0, FBuffer.Width-1) then
              FBuffer.Colors[iX, jY] := intfImg.Colors[i, j];
          end;
      end;
    end;
  finally
    intfimg.Free;
  end;
end;

function SameColor(AColor1, AColor2: TFPColor): Boolean;
begin
  Result := (AColor1.Red = AColor2.Red) and (AColor1.Green = AColor2.Green) and (AColor1.Blue = AColor2.Blue);
end;

{ Drawing a bitmap with a given opaque and transparent color }
procedure TMvIntfGraphicsDrawingEngine.DrawBitmapOT(X, Y: Integer;
  ABitmap: TCustomBitmap; AOpaqueColor, ATransparentColor: TColor);
var
  img: TLazIntfImage;
  i, j, iX, jY: Integer;
  col, opCol, trCol: TFPColor;
  gray: Word;
begin
  opCol := TColorToFPColor(AOpaqueColor);
  trCol := TColorToFPColor(ATransparentColor);

  img := ABitmap.CreateIntfImage;
  try
    for j := 0 to img.Height - 1 do
    begin
      jY := j + Y;
      if InRange(jY, 0, FBuffer.Height-1) then
        for i := 0 to img.Width - 1 do
        begin
          ix := i + X;
          if InRange(iX, 0, FBuffer.Width-1) then
          begin
            col := img.Colors[i, j];
            if SameColor(col, trCol) then
              col.Alpha := AlphaTransparent
            else
            if SameColor(col, opCol) then
              col.Alpha := AlphaOpaque
            else
            begin
              gray := CalculateGray(col);
              col := opCol;
              if SameColor(opCol, colWhite) then
                col.Alpha := gray
              else
                col.Alpha := $FFFF - gray;
            end;
            FBuffer.Colors[iX, jY] := AlphaBlend(FBuffer.Colors[iX, jY], col);
          end;
        end;
    end;
  finally
    img.Free;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.DrawCacheItem(X, Y: Integer;
  AImg: TPictureCacheItem; ADrawMode: TItemDrawMode; AOpacity: Single);
var
  Item: TLazIntfImageCacheItem;
  BufX, BufY, XMax, YMax, I, J, ImgX, ImgY: Integer;
  C1, C2: TFPColor;
  D1, D2: Single;
begin
  Item := AImg as TLazIntfImageCacheItem;
  if ADrawMode = idmDraw then
  begin
    {$IF Lcl_FullVersion < 1090000}
    { Workaround for //http://mantis.freepascal.org/view.php?id=27144 }
    CopyPixels(Item.Image, FBuffer, X, Y);
    {$ELSE}
    FBuffer.CopyPixels(Item.Image, X, Y);
    {$IFEND}
  end
  else
  begin
    BufX := Max(0, X);
    BufY := Max(0, Y);
    ImgX := Max(0, -X);
    ImgY := Max(0, -Y);
    XMax := Min(Item.Image.Width - ImgX, FBuffer.Width - BufX) - 1;
    YMax := Min(Item.Image.Height - ImgY, FBuffer.Height - BufY) - 1;
    for J := YMax downto 0 do
      for I := XMax downto 0 do
      begin
        C1 := FBuffer.Colors[BufX + I, BufY + J];
        C2 := Item.Image.Colors[ImgX + I, ImgY + J];
        if ADrawMode = idmUseSourceAlpha
          then D2 := C2.Alpha / 65535.0
          else D2 := AOpacity; // idmUseOpacity
        D1 := 1.0 - D2;
        C1.Red := Round(C1.Red * D1 + C2.Red * D2);
        C1.Green := Round(C1.Green * D1 + C2.Green * D2);
        C1.Blue := Round(C1.Blue * D1 + C2.Blue * D2);
        FBuffer.Colors[BufX + I, BufY + J] := C1;
      end;
  end;
end;

{ Scales the rectangle SrcRect of the specified source image (ASrcImg) such
  that it fits into the rectangle DestRect of the Buffer image. }
procedure TMvIntfGraphicsDrawingEngine.DrawScaledCacheItem(DestRect,
  SrcRect: TRect; ASrcImg: TPictureCacheItem);
var
  img, SrcImg: TLazIntfImage;
  w, h, x, y: Integer;
begin
  if FCanvas = nil then
    exit;

  w := SrcRect.Right - SrcRect.Left;
  h := SrcRect.Bottom - SrcRect.Top;

  SrcImg := (ASrcImg as TLazIntfImageCacheItem).Image;

  img := TLazIntfImage.Create(0, 0);
  try
    img.DataDescription := SrcImg.DataDescription;
    img.SetSize(w, h);
    for y := 0 to h-1 do
      for x := 0 to w-1 do
        img.Colors[x, y] := SrcImg.Colors[SrcRect.Left + x, SrcRect.Top + y];;
    FCanvas.Interpolation := TFPSharpInterpolation.Create;
    try
      FCanvas.StretchDraw(DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, img);
    finally
      FCanvas.Interpolation.Free;
      FCanvas.Interpolation := nil;
    end;
  finally
    img.Free;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  if FCanvas <> nil then
    FCanvas.Ellipse(X1,Y1, X2, Y2);
end;

procedure TMvIntfGraphicsDrawingEngine.FillPixels(X1, Y1, X2, Y2: Integer;
  AColor: TColor);
var
  c: TFPColor;
  x, y: Integer;
begin
  if (X1 >= FBuffer.Width) or (X2 < 0) or (Y1 >= FBuffer.Height) or (Y2 < 0) then
    exit;

  if X1 < 0 then X1 := 0;
  if Y1 < 0 then Y1 := 0;
  if X2 >= FBuffer.Width then X2 := FBuffer.Width - 1;
  if Y2 >= FBuffer.Height then Y2 := FBuffer.Height - 1;

  c := TColorToFPColor(ColorToRGB(AColor));
  for y := Y1 to Y2 do
    for x := X1 to X2 do
      FBuffer.Colors[x, y] := c;
end;

procedure TMvIntfGraphicsDrawingEngine.FillRect(X1, Y1, X2, Y2: Integer);
begin
  if FCanvas <> nil then
    FCanvas.FillRect(X1,Y1, X2, Y2);
end;

function TMvIntfGraphicsDrawingEngine.GetBrushColor: TColor;
begin
  if FCanvas <> nil then
    Result := FPColorToTColor(FCanvas.Brush.FPColor)
  else
    Result := 0;
end;

function TMvIntfGraphicsDrawingEngine.GetBrushStyle: TBrushStyle;
begin
  if FCanvas <> nil then
    Result := FCanvas.Brush.Style
  else
    Result := bsSolid;
end;

function TMvIntfGraphicsDrawingEngine.GetFontColor: TColor;
begin
  Result := FFontColor
end;

function TMvIntfGraphicsDrawingEngine.GetFontName: String;
begin
  Result := FFontName;
end;

function TMvIntfGraphicsDrawingEngine.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMvIntfGraphicsDrawingEngine.GetFontStyle: TFontStyles;
begin
  Result := FFontStyle;
end;

function TMvIntfGraphicsDrawingEngine.GetPenColor: TColor;
begin
  if FCanvas <> nil then
    Result := FPColorToTColor(FCanvas.Pen.FPColor)
  else
    Result := 0;
end;

function TMvIntfGraphicsDrawingEngine.GetPenWidth: Integer;
begin
  if FCanvas <> nil then
    Result := FCanvas.Pen.Width
  else
    Result := 0;
end;

function TMvIntfGraphicsDrawingEngine.GetOpacity: Single;
begin
  if Assigned(FCanvas) and (FCanvas.DrawingMode = dmAlphaBlend)
    then Result := FOpacity
    else Result := 1.0;
end;

procedure TMvIntfGraphicsDrawingEngine.SetOpacity(AValue: Single);
begin
  if not Assigned(FCanvas) or (AValue = FOpacity) then
    Exit;
  FOpacity := AValue;
  AddAlphaToColors;
end;

procedure TMvIntfGraphicsDrawingEngine.SetPenStyle(AValue: TPenStyle);
begin
  if FCanvas <> Nil then
    FCanvas.Pen.Style := TFPPenStyle(AValue);
end;

procedure TMvIntfGraphicsDrawingEngine.Line(X1, Y1, X2, Y2: Integer);
begin
  if FCanvas <> nil then
    FCanvas.Line(X1, Y1, X2, Y2);
end;

procedure TMvIntfGraphicsDrawingEngine.Polyline(const Points: array of TPoint);
begin
  FCanvas.Polyline(Points);
end;

procedure TMvIntfGraphicsDrawingEngine.Polygon(const Points: array of TPoint);
var
  OldColor: TColor;
  OldWidth: Integer;
  OldStyle: TPenStyle;
begin
  {$IF FPC_FullVersion >= 30203}
  FCanvas.Polygon(Points);
  {$ELSE}
  if BrushStyle <> bsClear then
  begin
    OldColor := PenColor;
    OldWidth := PenWidth;
    OldStyle := PenStyle;
    try
      PenColor := BrushColor;
      PenWidth := 1;
      PenStyle := psSolid;
      DoScanFill(Points, @FCanvas.Line);
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
  {$IFEND}
end;

procedure TMvIntfGraphicsDrawingEngine.PolyBezier(
  const Points: array of TPoint; Filled: Boolean; Continuous: Boolean);
var
  PtDyn: array of TPoint;
begin
  CalcBezier(Points, Continuous, PtDyn);
  if Filled
    then Polygon(PtDyn)
    else Polyline(PtDyn);
end;

procedure TMvIntfGraphicsDrawingEngine.PaintToCanvas(ACanvas: TCanvas;
  Origin: TPoint);
var
  bmp: TBitmap;
begin
  if ACanvas <> nil then begin
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32Bit;
      bmp.SetSize(FBuffer.Width, FBuffer.Height);
      bmp.LoadFromIntfImage(FBuffer);
      ACanvas.Draw(Origin.X, Origin.Y, bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  if FCanvas <> nil then
    FCanvas.Rectangle(X1,Y1, X2, Y2);
end;

function TMvIntfGraphicsDrawingEngine.SaveToImage(AClass: TRasterImageClass): TRasterImage;
begin
  Result := AClass.Create;
  Result.Width := FBuffer.Width;
  Result.Height := FBuffer.Height;
  Result.Canvas.FillRect(0, 0, Result.Width, Result.Height);
  Result.LoadFromIntfImage(FBuffer);
end;

procedure TMvIntfGraphicsDrawingEngine.SetBrushColor(AValue: TColor);
begin
  if FCanvas <> nil then
  begin
    FCanvas.Brush.FPColor := TColorToFPColor(AValue);
    AddAlphaToColors;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetBrushStyle(AValue: TBrushStyle);
begin
  if FCanvas <> nil then
    FCanvas.Brush.Style := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontColor(AValue: TColor);
begin
  FFontColor := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontName(AValue: String);
begin
  FFontName := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontSize(AValue: Integer);
begin
  FFontSize := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontStyle(AValue: TFontStyles);
begin
  FFontStyle := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetPenColor(AValue: TColor);
begin
  if FCanvas <> nil then
  begin
    FCanvas.Pen.FPColor := TColorToFPColor(AValue);
    AddAlphaToColors;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetPenWidth(AValue: Integer);
begin
  if FCanvas <> nil then
    FCanvas.Pen.Width := AValue;
end;

constructor TMvIntfGraphicsDrawingEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOpacity := 1.0;
end;

function TMvIntfGraphicsDrawingEngine.TextExtent(const AText: String): TSize;
var
  bmp: TBitmap;
  R: TRect;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Name := FFontName;
    bmp.Canvas.Font.Size := FFontSize;
    bmp.Canvas.Font.Style := FFontStyle;
    R := Rect(0, 0, DEFAULT_POI_TEXT_WIDTH, MaxInt);
    DrawText(bmp.Canvas.Handle, PChar(AText), Length(AText), R, DT_WORDBREAK + DT_CALCRECT);
    Result := Size(R.Width, R.Height);
  finally
    bmp.Free;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.TextOut(X, Y: Integer; const AText: String);
var
  bmp: TBitmap;
  R: TRect;
  txtFlags: Integer = DT_CENTER + DT_WORDBREAK;
begin
  if (FCanvas = nil) or (AText = '') then
    exit;

  bmp := TBitmap.Create;
  try
//    bmp.PixelFormat := pf32Bit;     // causes a crash on XUbuntu
    bmp.Canvas.Font.Name := FFontName;
    bmp.Canvas.Font.Size := FFontSize;
    bmp.Canvas.Font.Style := FFontStyle;
    bmp.Canvas.Font.Color := FFontColor;
    R := Rect(0, 0, 10000, 10000);
    DrawText(bmp.Canvas.Handle, PChar(AText), Length(AText), R, DT_CALCRECT + DT_WORDBREAK);
    bmp.SetSize(R.Right - R.Left, R.Bottom - R.Top);
    if GetBrushStyle <> bsClear then begin
      bmp.Canvas.Brush.Color := GetBrushColor;
      bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
      DrawText(bmp.Canvas.Handle, PChar(AText), Length(AText), R, txtFlags);
      DrawBitmap(X, Y, bmp, false);
    end else
    begin
      if FFontColor = clWhite then
        bmp.Canvas.Brush.Color := clBlack
      else
        bmp.Canvas.Brush.Color := clWhite;
      bmp.Canvas.FillRect(R);
      DrawText(bmp.Canvas.Handle, PChar(AText), Length(AText), R, txtFlags);
      DrawBitmapOT(X, Y, bmp, FFontColor, bmp.Canvas.Brush.Color);
    end;
  finally
    bmp.Free;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetCacheItemClass: TPictureCacheItemClass;
begin
  Result := TLazIntfImageCacheItem;
end;



end.

