{ This unit implement an MapViewer-Plugin to modify the tiles prior painting.
  It could be used to grayscale tiles, exchange a color or adjust brightness and contrast.
  It works on all three currently implemented Drawing-Engines.
  The implementation alter only the background tiles, not the tiles from any
  separate layer.
  This is helpful to display the partly transparent OpenRailwayMap-Tiles on top
  of a grayscale OpenStreetMap
}
unit mvtilemodifyplugin;

{$mode ObjFPC}{$H+}

interface

{ Activate USE_EPIKTIMER to include Epiktimer (https://wiki.lazarus.freepascal.org/EpikTimer)
  for a precise timing measurement.
  If not activated the timing measurement is much less precise.}
{$define USE_EPIKTIMER}

{ Activate USE_BGRA if your application uses the mvDE_BGRA drawing engine.
  If not activated, the plugin is not able to perform on the tiles of this drawinmg engine.
  If activated, but the drawing engine is not used, a lot of unused code is compiled into
  the target application.
}
{$define USE_BGRA}
{ Activate USE_RGB32 if your application uses the mvDE_RGBGraphics drawing engine.
  If not activated, the plugin is not able to perform on the tiles of this drawinmg engine.
  If activated, but the drawing engine is not used, a lot of unused code is compiled into
  the target application.
}
{$define USE_RGB32}

uses
  Classes, SysUtils,
  Graphics, Controls, LCLIntf, //LazLoggerBase,
  mvMapViewer, mvPluginCommon,  mvTypes,
  mvMapProvider, mvDrawingEngine, mvCache,
  FPImage, IntfGraphics, mvDE_IntfGraphics,
  uYCbCrTools
  {$ifdef USE_BGRA}, BGRABitmap, BGRABitmapTypes, mvDE_BGRA {$endif}
  {$ifdef USE_RGB32}, RGBGraphics, RGBTypes, mvDE_RGBGraphics {$endif}
  {$ifdef USE_EPIKTIMER}, epiktimer{$endif}
  ;
type

  { TTileModifyPluginPictureCache }

  TTileModifyPluginPictureCache = class(TPictureCache)
  public
    procedure ClearCache;
    procedure AddPureItem(MapProvider: TMapProvider; const TileId: TTileId; const Item: TPictureCacheItem);
  end;

  { TTileModifyPlugin }
  TTileModifyMode = (tmmNone, tmmGrayScale, tmmColorExchange, tmmBrightnessContrast);
  TTileModifyPlugin = class(TMvCustomPlugin)
  private
    FCurrentDrawingEngine : TMvCustomDrawingEngine;
    FInternalPictureCache : TTileModifyPluginPictureCache;
    FModifyMode : TTileModifyMode;
    FOrgColor : TColor;
    FExchangeColor : TColor;
    FSameColorRange : Single;
    FBrightness : Single;
    FContrast : Single;
    FMilliSecondsPerTile : Single;
    FAvgTileCount : Integer;
    FUseCache : Boolean;
    {$ifdef USE_EPIKTIMER}FEpikTimer : TEpikTimer;{$endif}

    function LimitToOne(Value : Single) : Single;
    procedure SetUseCache(Value : Boolean);
    procedure SetModifyMode(Value : TTileModifyMode);
    procedure SetSameColorRange(Value : Single);
    procedure SetBrightness(Value : Single);
    procedure SetContrast(Value : Single);
    procedure ProcessTileLazIntfImg(const ALazIntfImg: TLazIntfImage);
    {$ifdef USE_BGRA}procedure ProcessTileBRGA(const ABGRABitmap : TBGRABitmap);{$endif}
    {$ifdef USE_RGB32}procedure ProcessTileRGB32(const ARGB32Bitmap : TRGB32Bitmap);{$endif}

  protected
    procedure TileAfterGetFromCache(AMapView: TMapView; ATileLayer: TGPSTileLayerBase;
      AMapProvider: TMapProvider; ATileID: TTileID; ATileImg: TPictureCacheItem;
      var Handled: Boolean); override;
  published
    property ModifyMode : TTileModifyMode read FModifyMode write SetModifyMode;
    property MapView;
    // For the color exchange three values have to be set
    // The original color, the color to be painted instead
    // and the deviation (0.0 to 1.0) of the original color to be treated
    // as the same color.
    property OrgColor : TColor read FOrgColor write FOrgColor;
    property ExchangeColor : TColor read FExchangeColor write FExchangeColor;
    property SameColorRange : Single read FSameColorRange write SetSameColorRange;
    property Brightness : Single read FBrightness write SetBrightness;
    property Contrast : Single read FContrast write SetContrast;
    property UseCache : Boolean read FUseCache write SetUseCache;
  public
    property MilliSecondsPerTile : Single read FMilliSecondsPerTile;
    property AvgTileCount : Integer read FAvgTileCount write FAvgTileCount;
    procedure ResetMilliSecondsPerTile;
    constructor Create(AOwner: TComponent); override;
  end;

implementation
const
  TileCountMax = 128;

type
  TPluginRGBPixel = packed record
    blue, green, red, alpha: Byte;
  end;

function IsCloseToFPColor(const AColor, BColor : TFPColor; const ASameColorRange : Single) : Boolean;
var
  dw : Word;
begin
  dw := Trunc($FFFF * ASameColorRange);
  Result := (Abs(AColor.Red-BColor.Red) <= dw) and
            (Abs(AColor.Green-BColor.Green) <= dw) and
            (Abs(AColor.Blue-BColor.Blue) <= dw);
end;
function IsCloseToBGRAPixel(const AColor : TColor; const BColor : TPluginRGBPixel; const ASameColorRange : Single) : Boolean;
var
  dw : Word;
begin
  dw := Trunc($FF * ASameColorRange);
  Result := (Abs(Red(AColor)-BColor.Red) <= dw) and
            (Abs(Green(AColor)-BColor.Green) <= dw) and
            (Abs(Blue(AColor)-BColor.Blue) <= dw);
end;

{ TTileModifyPluginPictureCache }

procedure TTileModifyPluginPictureCache.ClearCache;
begin
  inherited ClearCache;
end;

procedure TTileModifyPluginPictureCache.AddPureItem(MapProvider: TMapProvider;
  const TileId: TTileId; const Item: TPictureCacheItem);
var
  lids : String;
begin
  if not Assigned(Item) then Exit;
  lids := GetFileName(MapProvider, TileId);
  AddItem(Item,lids);
end;

{ TTileModifyPlugin }

function TTileModifyPlugin.LimitToOne(Value: Single): Single;
begin
  if Value < 0.0 then
    Result := 0.0
  else if Value > 1.0 then
    Result := 1.0
  else
    Result := Value;
end;

procedure TTileModifyPlugin.SetUseCache(Value: Boolean);
begin
  if Value <> FUseCache then
  begin
    FUseCache := Value;
    FInternalPictureCache.ClearCache;
  end;
end;

procedure TTileModifyPlugin.SetModifyMode(Value: TTileModifyMode);
begin
  if Value <> FModifyMode then
  begin
    FModifyMode := Value;
    FInternalPictureCache.ClearCache;
  end;
end;

procedure TTileModifyPlugin.SetSameColorRange(Value: Single);
var
  newv : Single;
begin
  newv := LimitToOne(Value);
  if newv <> FSameColorRange then
  begin
    FSameColorRange := newv;
    FInternalPictureCache.ClearCache;
  end;
end;

procedure TTileModifyPlugin.SetBrightness(Value: Single);
var
  newv : Single;
begin
  newv := LimitToOne(Value);
  if newv <> FBrightness then
  begin
    FBrightness := newv;
    FInternalPictureCache.ClearCache;
  end;
end;

procedure TTileModifyPlugin.SetContrast(Value: Single);
var
  newv : Single;
begin
  newv := LimitToOne(Value);
  if newv <> FContrast then
  begin
    FContrast := newv;
    FInternalPictureCache.ClearCache;
  end;
end;

procedure TTileModifyPlugin.ProcessTileLazIntfImg(
  const ALazIntfImg: TLazIntfImage);
var
  x, y: Integer;
  lTmpFPClr: TFPColor;
  lTmpClr : TColor;
  Gray: Word;
  r,g,b : Double;
  yc,cr,cb : Double;
  rb, gb, bb : Byte;
  lOldClr, lNewClr : TFPColor;
begin
  case FModifyMode of
    tmmGrayScale :
      begin
        ALazIntfImg.BeginUpdate;
        try
          r := 0.30;
          g := 0.59;
          b := 0.11;
          for y := 0 to ALazIntfImg.Height - 1 do
            for x := 0 to ALazIntfImg.Width - 1 do
            begin
              lTmpFPClr := ALazIntfImg.Colors[x, y];

              Gray := Round(lTmpFPClr.Red * R + lTmpFPClr.Green * G + lTmpFPClr.Blue * B);
              lTmpFPClr.Red := Gray;
              lTmpFPClr.Green := Gray;
              lTmpFPClr.Blue := Gray;
              ALazIntfImg.Colors[x, y] := lTmpFPClr;
            end;
        finally
          ALazIntfImg.EndUpdate;
        end;
      end;
    tmmColorExchange :
      begin
        if FOrgColor = FExchangeColor then Exit;
        lOldClr := TColorToFPColor(FOrgColor);
        lNewClr := TColorToFPColor(FExchangeColor);
        ALazIntfImg.BeginUpdate;
        try
          for y := 0 to ALazIntfImg.Height - 1 do
            for x := 0 to ALazIntfImg.Width - 1 do
            begin
              lTmpFPClr := ALazIntfImg.Colors[x, y];
              if IsCloseToFPColor(lTmpFPClr,lOldClr,FSameColorRange) then
                lTmpFPClr := lNewClr;
              ALazIntfImg.Colors[x, y] := lTmpFPClr;
            end;
        finally
          ALazIntfImg.EndUpdate;
        end;
      end;
    tmmBrightnessContrast :
      begin
        if (FBrightness <> 0.5) or
           (FContrast <> 0.5) then
        begin
          ALazIntfImg.BeginUpdate;
          try
            for y := 0 to ALazIntfImg.Height - 1 do
              for x := 0 to ALazIntfImg.Width - 1 do
              begin
                lTmpFPClr := ALazIntfImg.Colors[x, y];
                lTmpClr := FPColorToTColor(lTmpFPClr);
                RGBToYCbCrInline(lTmpClr.Red,lTmpClr.Green,lTmpClr.Blue,yc,cr,cb);
                YCbCrContrastBrightness(yc,cr,cb,FContrast,FBrightness);
                YCbCrToRGBInline(yc,cr,cb,rb,gb,bb);
                lTmpClr := RGB(rb,gb,bb);
                lTmpFPClr := TColorToFPColor(lTmpClr);
                ALazIntfImg.Colors[x, y] := lTmpFPClr;
              end;
          finally
            ALazIntfImg.EndUpdate;
          end;
        end;
      end;
  else
    // tmmNone :;
  end;
end;
{$ifdef USE_BGRA}
procedure TTileModifyPlugin.ProcessTileBRGA(const ABGRABitmap: TBGRABitmap);
var
  plBGRAPixel: PBGRAPixel;
  lNewBGRAPixelClr : TPluginRGBPixel;
  n: integer;
  yc,cr,cb : Double;
begin
  case FModifyMode of
    tmmGrayScale :
      begin
        ABGRABitmap.InplaceGrayscale();
      end;
    tmmColorExchange :
      begin
        if FOrgColor = FExchangeColor then Exit;
        plBGRAPixel := ABGRABitmap.Data;
        for n := ABGRABitmap.NbPixels-1 downto 0 do
        begin
          lNewBGRAPixelClr.red:= plBGRAPixel^.red;
          lNewBGRAPixelClr.green:= plBGRAPixel^.green;
          lNewBGRAPixelClr.blue:= plBGRAPixel^.blue;
          if IsCloseToBGRAPixel(FOrgColor,lNewBGRAPixelClr,FSameColorRange) then
          begin
            plBGRAPixel^.red := Red(FExchangeColor);
            plBGRAPixel^.green := Green(FExchangeColor);
            plBGRAPixel^.blue := Blue(FExchangeColor);
          end;
          Inc(plBGRAPixel);
        end;
        ABGRABitmap.InvalidateBitmap;   // note that we have accessed pixels directly
      end;
    tmmBrightnessContrast :
      begin
        if (FBrightness <> 0.5) or
           (FContrast <> 0.5) then
        begin
          plBGRAPixel := ABGRABitmap.Data;
          for n := ABGRABitmap.NbPixels-1 downto 0 do
          begin
            RGBToYCbCrInline(plBGRAPixel^.red,plBGRAPixel^.green,plBGRAPixel^.blue,yc,cr,cb);
            YCbCrContrastBrightness(yc,cr,cb,FContrast,FBrightness);
            YCbCrToRGBInline(yc,cr,cb,plBGRAPixel^.red,plBGRAPixel^.green,plBGRAPixel^.blue);
            Inc(plBGRAPixel);
          end;
          ABGRABitmap.InvalidateBitmap;   // note that we have accessed pixels directly
        end;
      end;
  else
    // tmmNone :;
  end;
end;
{$endif}
{$ifdef USE_RGB32}
procedure TTileModifyPlugin.ProcessTileRGB32(const ARGB32Bitmap: TRGB32Bitmap);
var
  plBGRAPixel: PRGB32Pixel;
  lPixelClr : TPluginRGBPixel;
  lNewBGRAPixelClr : TPluginRGBPixel;
  n: integer;
  cnt : Integer;
  clr : TColor;
  yc,cr,cb : Double;
begin
  case FModifyMode of
    tmmGrayScale :
      begin
        ARGB32Bitmap.Grayscale;
      end;
    tmmColorExchange :
      begin
        if FOrgColor = FExchangeColor then Exit;
        lNewBGRAPixelClr.red := Red(FExchangeColor);
        lNewBGRAPixelClr.green := Green(FExchangeColor);
        lNewBGRAPixelClr.blue := Blue(FExchangeColor);

        plBGRAPixel := PRGB32Pixel(ARGB32Bitmap.Pixels);
        cnt := ARGB32Bitmap.Height * ARGB32Bitmap.RowPixelStride; // * lRGB32Bitmap.SizeOfPixel;
        for n := 0 to cnt-1 do
        begin
          clr := RGB32PixelToColor(plBGRAPixel^);
          lPixelClr.red := Red(clr);
          lPixelClr.green := Green(clr);
          lPixelClr.blue := Blue(clr);
          if IsCloseToBGRAPixel(FOrgColor,lPixelClr,FSameColorRange) then
          begin
            clr := RGB(lNewBGRAPixelClr.red,lNewBGRAPixelClr.green,lNewBGRAPixelClr.blue);
            plBGRAPixel^ := ColorToRGB32Pixel(clr);
          end;
          Inc(plBGRAPixel);
        end;
      end;
    tmmBrightnessContrast :
      begin
        if (FBrightness <> 0.5) or
           (FContrast <> 0.5) then
        begin
          plBGRAPixel := PRGB32Pixel(ARGB32Bitmap.Pixels);
          cnt := ARGB32Bitmap.Height * ARGB32Bitmap.RowPixelStride; // * lRGB32Bitmap.SizeOfPixel;
          for n := 0 to cnt-1 do
          begin
            clr := RGB32PixelToColor(plBGRAPixel^);
            lPixelClr.red := Red(clr);
            lPixelClr.green := Green(clr);
            lPixelClr.blue := Blue(clr);
            RGBToYCbCrInline(lPixelClr.red,lPixelClr.green,lPixelClr.blue,yc,cr,cb);
            YCbCrContrastBrightness(yc,cr,cb,FContrast,FBrightness);
            YCbCrToRGBInline(yc,cr,cb,lPixelClr.red,lPixelClr.green,lPixelClr.blue);
            clr := RGB(lPixelClr.red,lPixelClr.green,lPixelClr.blue);
            plBGRAPixel^ := ColorToRGB32Pixel(clr);
            Inc(plBGRAPixel);
          end;
        end;
      end;
  else
    // tmmNone :;
  end;
end;
{$endif}

procedure TTileModifyPlugin.TileAfterGetFromCache(AMapView: TMapView;
  ATileLayer: TGPSTileLayerBase; AMapProvider: TMapProvider; ATileID: TTileID;
  ATileImg: TPictureCacheItem; var Handled: Boolean);

// const
//  OSM = 'OPENSTREETMAP';

var
  {$ifndef USE_EPIKTIMER}
    t0, t1 : Int64;
  {$endif}
  ms : Double;
  partd : Double;
  picitem : TPictureCacheItem;
begin
  Unused(AMapView);
  Unused(ATileID,Handled);

  if Assigned(ATileLayer) then Exit;
  if FModifyMode = tmmNone then Exit;

  {$ifdef USE_EPIKTIMER}
    FEpikTimer.Clear;
    FEpikTimer.Start;
  {$else}
    t0 := GetTickCount;
  {$endif}

  if FCurrentDrawingEngine <> MapView.DrawingEngine then
  begin
    FInternalPictureCache.ClearCache;
    FCurrentDrawingEngine := MapView.DrawingEngine;
  end;

  if FUseCache and FInternalPictureCache.InCache(AMapProvider,ATileID) then
  begin
    FInternalPictureCache.GetFromCache(AMapProvider,ATileID,picitem);
    if ATileImg is TLazIntfImageCacheItem then
      TLazIntfImageCacheItem(ATileImg).Image.Assign(TLazIntfImageCacheItem(picitem).Image)
    {$ifdef USE_BGRA}
    else if ATileImg is TBGRABitmapCacheItem then
      TBGRABitmapCacheItem(ATileImg).Image.Assign(TBGRABitmapCacheItem(picitem).Image)
    {$endif}
    {$ifdef USE_RGB32}
    else if ATileImg is TRGB32BitmapCacheItem then
      TRGB32BitmapCacheItem(ATileImg).Image.Assign(TRGB32BitmapCacheItem(picitem).Image)
    {$endif}
    ;
  end
  else
  begin
    picitem := Nil;
    if ATileImg is TLazIntfImageCacheItem then
    begin
      ProcessTileLazIntfImg(TLazIntfImageCacheItem(ATileImg).Image);
      if FUseCache then
        picitem := TLazIntfImageCacheItem.Create(ATileImg);
    end
    {$ifdef USE_BGRA}
    else if ATileImg is TBGRABitmapCacheItem then
    begin
      ProcessTileBRGA(TBGRABitmapCacheItem(ATileImg).Image);
      if FUseCache then
        picitem := TBGRABitmapCacheItem.Create(ATileImg);
    end
    {$endif}
    {$ifdef USE_RGB32}
    else if ATileImg is TRGB32BitmapCacheItem then
    begin
      ProcessTileRGB32(TRGB32BitmapCacheItem(ATileImg).Image);
      if FUseCache then
        picitem := TRGB32BitmapCacheItem.Create(ATileImg);
    end
    {$endif}
    ;
    if Assigned(picitem) then
    begin
      FInternalPictureCache.AddPureItem(AMapProvider,ATileID,picitem);
      FInternalPictureCache.CheckCacheSize(Nil);
    end;
  end;

  {$ifdef USE_EPIKTIMER}
    FEpikTimer.Stop;
    ms := FEpikTimer.Elapsed*1000.0;
  {$else}
    t1 := GetTickCount;
    ms := t1-t0;
  {$endif}

  partd := ((TileCountMax-FAvgTileCount)/TileCountMax);
  FMilliSecondsPerTile := (FMilliSecondsPerTile*(1.0-partd))+
                          (ms*partd);
  if FAvgTileCount < TileCountMax-1 then
    Inc(FAvgTileCount);
end;

procedure TTileModifyPlugin.ResetMilliSecondsPerTile;
begin
  FAvgTileCount := 0;
  FMilliSecondsPerTile := 0.0;
end;

constructor TTileModifyPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FInternalPictureCache := TTileModifyPluginPictureCache.Create(Self);
  FInternalPictureCache.UseDisk:= False;

  {$ifdef USE_EPIKTIMER}FEpikTimer := TEpikTimer.Create(Self);{$endif}
end;

initialization
  // RegisterPluginClass(TTileModifyPlugin, 'Tile Modify');
  // This plugin cannot be registered because it would pull other packages into LazMapViewer.

end.

