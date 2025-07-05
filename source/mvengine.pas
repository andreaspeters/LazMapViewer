{
  (c) 2014 ti_dic@hotmail.com

  Parts of this component are based on :
    Map Viewer Copyright (C) 2011 Maciej Kaczkowski / keit.co

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}


unit mvEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, Controls, Math, GraphType, FPImage,
  mvGeoMath, mvTypes, mvJobQueue, mvMapProvider, mvDownloadEngine, mvCache, mvDragObj;

type
  TDrawTileEvent = procedure (const TileId: TTileId; X,Y: integer;
    TileImg: TPictureCacheItem) of object;

  TDrawStretchedTileEvent = procedure (const TileId: TTileId; X,Y: Integer;
    TileImg: TPictureCacheItem; const R: TRect) of object;

  TTileAfterGetFromCacheEvent = procedure (const AMapProvider: TMapProvider;
                                           const ATileId: TTileId;
                                           const ATileImg: TPictureCacheItem) of object;

  TEraseBackgroundEvent = procedure (const R: TRect) of Object;

  TTileDownloadedEvent = procedure (const TileId: TTileId) of object;

  TCenterMovingEvent = procedure (Sender: TObject; var NewCenter: TRealPoint; var Allow: Boolean) of object;
  TZoomChangingEvent = procedure (Sender: TObject; NewZoom: Integer; var Allow: Boolean) of object;

  TTileIdArray = Array of TTileId;

  { TMapWindow }

  TMapWindow = Record
    MapProvider: TMapProvider;
    X: Int64;
    Y: Int64;
    Center: TRealPoint;
    Zoom: integer;
    ZoomCenter: TRealPoint;
    ZoomOffset: TPoint;
    Height: integer;
    Width: integer;
  end;


  { TMapViewerEngine }

  TMapViewerEngine = Class(TComponent)
    private
      FDragObj : TDragObj;
      Cache : TPictureCache;
      FActive: boolean;
      FBkColor: TFPColor;
      FCyclic: Boolean;
      FDownloadEngine: TMvCustomDownloadEngine;
      FDrawPreviewTiles: Boolean;
      FDrawTitleInGuiThread: boolean;
      FInDrag: Boolean;
      FOnCenterMove: TNotifyEvent;
      FOnCenterMoving: TCenterMovingEvent;
      FOnChange: TNotifyEvent;
      FOnDrawTile: TDrawTileEvent;
      FOnDrawStretchedTile: TDrawStretchedTileEvent;
      FOnEraseBackground: TEraseBackgroundEvent;
      FOnTileDownloaded: TTileDownloadedEvent;
      FOnZoomChange: TNotifyEvent;
      FOnZoomChanging: TZoomChangingEvent;
      FCreateTempTileCopy : Boolean;
      FTileAfterGetFromCacheEvent : TTileAfterGetFromCacheEvent;
      FZoomMax: Integer;
      FZoomMin: Integer;
      lstProvider : TStringList;
      Queue : TJobQueue;
      MapWin : TMapWindow;
      FZoomToCursor: Boolean;
      function GetCacheItemClass: TPictureCacheItemClass;
      function GetCacheMaxAge: Integer;
      function GetCacheOnDisk: Boolean;
      function GetCachePath: String;
      function GetCacheMemMaxItemCount : Integer;
      function GetCacheMemMaxItemCountDefault : Integer;
      function GetCenter: TRealPoint;
      function GetHeight: integer;
      class function GetProjectionType(const aWin: TMapWindow): TProjectionType;
      function GetMapProjectionType: TProjectionType;
      function GetMapProvider: String;
      function GetUseThreads: Boolean;
      function GetWidth: integer;
      function GetZoom: integer;
      function IsValidTile(const aWin: TMapWindow; const aTile: TTIleId): boolean;
      procedure MoveMapCenter(Sender: TDragObj);
      procedure SetActive(AValue: boolean);
      procedure SetBkColor(AValue: TFPColor);
      procedure SetCacheItemClass(AValue: TPictureCacheItemClass);
      procedure SetCacheMaxAge(AValue: Integer);
      procedure SetCacheOnDisk(AValue: Boolean);
      procedure SetCachePath(AValue: String);
      procedure SetCacheMemMaxItemCount(AValue : Integer);
      procedure SetCenter(ACenter: TRealPoint);
      procedure SetCyclic(AValue: Boolean);
      procedure SetDownloadEngine(AValue: TMvCustomDownloadEngine);
      procedure SetHeight(AValue: integer);
      procedure SetMapProvider(AValue: String);
      procedure SetUseThreads(AValue: Boolean);
      procedure SetWidth(AValue: integer);
      procedure SetZoom(AValue: Integer); overload;
      procedure SetZoom(AValue: integer; AZoomToCursor: Boolean); overload;
      function DegreesToMapPixels(const AWin: TMapWindow; APt: TRealPoint): TPoint;
      function MapPixelsToDegrees(const AWin: TMapWindow; APoint: TPoint): TRealPoint; overload; inline;
      function MapPixelsToDegrees(const AWin: TMapWindow; AX, AY: Double): TRealPoint; overload;
      function PixelsToDegreesEPSG3395(AX, AY: Double; Zoom: Integer): TRealPoint;
      function PixelsToDegreesEPSG3857(AX, AY: Double; Zoom: Integer): TRealPoint;
      procedure CalculateWin(var AWin: TMapWindow);
      function DegreesToPixelsEPSG3395(const AWin: TMapWindow; APt: TRealPoint): TPoint;
      function DegreesToPixelsEPSG3857(const AWin: TMapWindow; APt: TRealPoint): TPoint;
      procedure Redraw(const aWin: TMapWindow; const paintOnly: Boolean = False);
      function CalculateVisibleTiles(const aWin: TMapWindow; out Area: TArea): Boolean;
      function IsCurrentWin(const aWin: TMapWindow) : boolean;
    protected
      procedure AdjustZoomCenter(var AWin: TMapWindow);
      procedure ConstraintZoom(var aWin: TMapWindow);
      function GetTileName(const Id: TTileId): String;
      procedure evDownload(Data: TObject; Job: TJob);
      procedure TileDownloaded(Data: PtrInt);
      procedure EraseBackground(const R: TRect);
      procedure DrawTileFromCache(constref ATile: TTileId; constref AWin: TMapWindow);
      procedure DrawStretchedTile(const TileId: TTileID; X, Y: Integer; TileImg: TPictureCacheItem; const R: TRect);
      Procedure DrawTile(const TileId: TTileId; X,Y: integer; TileImg: TPictureCacheItem);
      Procedure DoDrag(Sender: TDragObj);
      Procedure DoEndDrag(Sender: TDragObj);
      Function DoCenterMoving(var ANewPoint: TRealPoint): Boolean;
      Function DoZoomChanging(ANewZoom: Integer): Boolean;

    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      function AddMapProvider(OpeName: String; ProjectionType: TProjectionType; Url: String;
        MinZoom, MaxZoom, NbSvr: integer; GetSvrStr: TGetSvrStr = nil;
        GetXStr: TGetValStr = nil; GetYStr: TGetValStr = nil;
        GetZStr: TGetValStr = nil): TMapProvider;
      procedure CancelCurrentDrawing;
      procedure ClearCache;
      procedure ClearMapProviders;
      function CrossesDateline: Boolean;
      procedure GetMapProviders(AList: TStrings);
      function MapProviderByName(AName: String): TMapProvider;
      function LatLonToScreen(APt: TRealPoint): TPoint;
      function LonLatToScreen(APt: TRealPoint): TPoint; deprecated 'Use LatLonToScreen';
      function LatLonToWorldScreen(APt: TRealPoint): TPoint;
      function LonLatToWorldScreen(APt: TRealPoint): TPoint; deprecated 'Use LatLonToWorldScreen';
      procedure PrepareCache(AMapProvider: TMapProvider);
      function ReadProvidersFromXML(AFileName: String; out AMsg: String): Boolean;
      procedure Redraw;
      Procedure RegisterProviders;
      function ScreenRectToRealArea(ARect: TRect): TRealArea;
      function ScreenToLatLon(aPt: TPoint): TRealPoint;
      function ScreenToLonLat(aPt: TPoint): TRealPoint; deprecated 'Use ScreenToLatLon';
      procedure SetSize(aWidth, aHeight: integer);
      function ValidProvider(const AProvider: String): Boolean;
      function WorldScreenToLatLon(aPt: TPoint): TRealPoint;
      function WorldScreenToLonLat(aPt: TPoint): TRealPoint; deprecated 'Use WorldScreenToLatLon';
      procedure WriteProvidersToXML(AFileName: String);

      procedure DblClick(Sender: TObject);
      procedure MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
        {%H-}Shift: TShiftState; X, Y: Integer);
      procedure MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
        X, Y: Integer);
      procedure MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
        {%H-}Shift: TShiftState; X, Y: Integer);
      procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
        WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
      procedure ZoomOnArea(const aArea: TRealArea);
      procedure CopyMapWindowFrom(AEngine: TMapViewerEngine);

      property MapLeft: Int64 read MapWin.X;
      property MapTop: Int64 read MapWin.Y;
      property MapProjectionType: TProjectionType read GetMapProjectionType;

      property BkColor: TFPColor read FBkColor write SetBkColor;
      property Center: TRealPoint read GetCenter write SetCenter;
      property DrawPreviewTiles : Boolean read FDrawPreviewTiles write FDrawPreviewTiles;
      property InDrag: Boolean read FInDrag;
      property DragObj: TDragObj read FDragObj;
      property CacheMaxAge: Integer read GetCacheMaxAge write SetCacheMaxAge;
      property ZoomMin: Integer read FZoomMax write FZoomMin;
      property ZoomMax: Integer read FZoomMax write FZoomMax;
      property CacheMemMaxItemCountDefault : Integer read GetCacheMemMaxItemCountDefault;
    published
      property Active: Boolean read FActive write SetActive default false;
      property CacheOnDisk: Boolean read GetCacheOnDisk write SetCacheOnDisk;
      property CachePath: String read GetCachePath write SetCachePath;
      property CacheMemMaxItemCount : Integer read GetCacheMemMaxItemCount write SetCacheMemMaxItemCount;
      property Cyclic: Boolean read FCyclic write SetCyclic default false;
      property DownloadEngine: TMvCustomDownloadEngine
        read FDownloadEngine write SetDownloadEngine;
      property DrawTitleInGuiThread: boolean
        read FDrawTitleInGuiThread write FDrawTitleInGuiThread;
      property Height: integer read GetHeight write SetHeight;
      property JobQueue: TJobQueue read Queue;
      property MapProvider: String read GetMapProvider write SetMapProvider;
      property UseThreads: Boolean read GetUseThreads write SetUseThreads;
      property Width: integer read GetWidth write SetWidth;
      property Zoom: integer read GetZoom write SetZoom;
      property ZoomToCursor: Boolean read FZoomToCursor write FZoomToCursor default True;
      property CacheItemClass: TPictureCacheItemClass read GetCacheItemClass write SetCacheItemClass;
      { CreateTempTileCopy creates a local copy of the tile prior further processing }
      property CreateTempTileCopy : Boolean read FCreateTempTileCopy write FCreateTempTileCopy;
      property OnCenterMove: TNotifyEvent read FOnCenterMove write FOnCenterMove;
      property OnCenterMoving: TCenterMovingEvent read FOnCenterMoving write FOnCenterMoving;
      property OnChange: TNotifyEvent Read FOnChange write FOnchange; //called when visiable area change
      property OnDrawStretchedTile: TDrawStretchedTileEvent read FOnDrawStretchedTile write FOnDrawStretchedTile;
      property OnDrawTile: TDrawTileEvent read FOnDrawTile write FOnDrawTile;
      property OnEraseBackground: TEraseBackgroundEvent read FOnEraseBackground write FOnEraseBackground;
      property OnTileDownloaded: TTileDownloadedEvent read FOnTileDownloaded write FOnTileDownloaded;
      property OnZoomChange: TNotifyEvent read FOnZoomChange write FOnZoomChange;
      property OnZoomChanging: TZoomChangingEvent read FOnZoomChanging write FOnZoomChanging;
      property OnTileAfterGetFromCache: TTileAfterGetFromCacheEvent read FTileAfterGetFromCacheEvent write FTileAfterGetFromCacheEvent;
  end;

// The following functions have been moved to mvGeoMath and will be removed here sooner or later...
function DMSToDeg(Deg, Min: Word; Sec: Double): Double; deprecated 'Use function in unit mvGeoMath';
function GPSToDMS(Angle: Double): string; deprecated 'Use function in unit mvGeoMath';
function GPSToDMS(Angle: Double; AFormatSettings: TFormatSettings): string; deprecated 'Use function in unit mvGeoMath';

function LatToStr(ALatitude: Double; DMS: Boolean): String; deprecated 'Use function in unit mvGeoMath';
function LatToStr(ALatitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String; deprecated 'Use function in unit mvGeoMath';
function LonToStr(ALongitude: Double; DMS: Boolean): String; deprecated 'Use function in unit mvGeoMath';
function LonToStr(ALongitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String; deprecated 'Use function in unit mvGeoMath';
function TryStrToGps(const AValue: String; out ADeg: Double): Boolean; deprecated 'Use function in unit mvGeoMath';
function TryStrDMSToDeg(const AValue: String; out ADeg: Double): Boolean; deprecated 'Use function in unit mvGeoMath';

function ZoomFactor(AZoomLevel: Integer): Int64; deprecated 'Use function in unit mvGeoMath';

var
  HERE_AppID: String = '';
  HERE_AppCode: String = '';
  OpenWeatherMap_ApiKey: String = '';
  ThunderForest_ApiKey: String = '';


implementation

uses
  Forms, TypInfo, laz2_xmlread, laz2_xmlwrite, laz2_dom,
  mvJobs, mvGpsObj;

type

  { TEnvTile }

  TEnvTile = Class(TBaseTile)
  private
    Tile: TTileId;
    Win: TMapWindow;
  public
    constructor Create(const aTile: TTileId; const aWin: TMapWindow);reintroduce;
  end;


  { TMemObj }

  TMemObj = Class
  private
    FWin: TMapWindow;
  public
    constructor Create(const aWin: TMapWindow);
  end;

  constructor TMemObj.Create(const aWin: TMapWindow);
  begin
    FWin := aWin;
  end;


{ TEnvTile }

constructor TEnvTile.Create(const aTile: TTileId; const aWin: TMapWindow);
begin
  inherited Create(aWin.MapProvider);
  Tile := aTile;
  Win := aWin;
end;


{ TMapViewerEngine }

constructor TMapViewerEngine.Create(aOwner: TComponent);
begin
  DrawTitleInGuiThread := true;
  DrawPreviewTiles := true;
  FDragObj := TDragObj.Create;
  FDragObj.OnDrag := @DoDrag;
  FDragObj.OnEndDrag := @DoEndDrag;
  Cache := TPictureCache.Create(self);
  lstProvider := TStringList.Create;
  FBkColor := colWhite;
  RegisterProviders;
  Queue := TJobQueue.Create(8);

  inherited Create(aOwner);

  FZoomMin := 1;
  FZoomMax := 19;
  FZoomToCursor := true;
  ConstraintZoom(MapWin);
  CalculateWin(mapWin);
end;

destructor TMapViewerEngine.Destroy;
begin
  Queue.CancelAllJob(Nil);
  Queue.RemoveAsyncCalls(Self);
  FreeAndNil(Queue);
  ClearMapProviders;
  FreeAndNil(FDragObj);
  FreeAndNil(lstProvider);
  FreeAndNil(Cache);
  inherited Destroy;
end;

function TMapViewerEngine.AddMapProvider(OpeName: String; ProjectionType: TProjectionType;
  Url: String; MinZoom, MaxZoom, NbSvr: integer; GetSvrStr: TGetSvrStr;
  GetXStr: TGetValStr; GetYStr: TGetValStr; GetZStr: TGetValStr): TMapProvider;
var
  idx :integer;
Begin
  idx := lstProvider.IndexOf(OpeName);
  if idx = -1 then
  begin
    Result := TMapProvider.Create(OpeName);
    lstProvider.AddObject(OpeName, Result);
  end
  else
    Result := TMapProvider(lstProvider.Objects[idx]);
  Result.AddUrl(Url, ProjectionType, NbSvr, MinZoom, MaxZoom, GetSvrStr, GetXStr, GetYStr, GetZStr);
end;

procedure TMapViewerEngine.AdjustZoomCenter(var AWin: TMapWindow);
var
  ptMouseCursor: TPoint;
  rPtAdjustedCenter: TRealPoint;
begin
  ptMouseCursor := LatLonToScreen(AWin.ZoomCenter);
  rPtAdjustedCenter := ScreenToLatLon(ptMouseCursor.Add(AWin.ZoomOffset));
  AWin.Center := rPtAdjustedCenter;
  CalculateWin(AWin);
end;

// Returns whether the view area is not covered fully with a tiles
function TMapViewerEngine.CalculateVisibleTiles(const aWin: TMapWindow; out
  Area: TArea): Boolean;
var
  MaxX, MaxY, startX, startY: int64;
  WorldMax: Int64;
begin
  Area := Default(TArea);
  Result := (aWin.X <= 0) and (aWin.Y <= 0);
  WorldMax := Int64(1) shl AWin.Zoom - 1;
  MaxX := Int64(aWin.Width) div TileSize.CX + 1;
  MaxY := Int64(aWin.Height) div TileSize.CY + 1;
  if (MaxX > WorldMax) or (MaxY > WorldMax) then
  begin
    Result := False;
    MaxX := Min(WorldMax, MaxX);
    MaxY := Min(WorldMax, MaxY);
  end;
  startX := -aWin.X div TileSize.CX;
  startY := -aWin.Y div TileSize.CY;
  if (startX < 0) or (startY < 0) then
  begin
    startX := Max(0, startX);
    startY := Max(0, startY);
    Result := False;
  end;
  Area.Left := startX;
  Area.Right := startX + MaxX;
  Area.Top := startY;
  Area.Bottom := startY + MaxY;
end;

procedure TMapViewerEngine.CalculateWin(var AWin: TMapWindow);
var
  PixelLocation: TPoint; // review: coth: Should it use Int64?
  PType: TProjectionType;
begin
  PType := GetProjectionType(AWin);
  case PType of
    ptEPSG3857: PixelLocation := DegreesToPixelsEPSG3857(AWin, AWin.Center);
    ptEPSG3395: PixelLocation := DegreesToPixelsEPSG3395(AWin, AWin.Center);
    else PixelLocation := DegreesToPixelsEPSG3857(AWin, AWin.Center);
  end;

  AWin.X := Int64(AWin.Width div 2) - PixelLocation.x;
  AWin.Y := Int64(AWin.Height div 2) - PixelLocation.y;
end;

procedure TMapViewerEngine.CancelCurrentDrawing;
var
  Jobs: TJobArray;
begin
  Jobs := Queue.CancelAllJob(self);
  Queue.WaitForTerminate(Jobs);
end;

procedure TMapViewerEngine.ClearCache;
begin
  Cache.ClearCache;
end;

procedure TMapViewerEngine.ClearMapProviders;
var
  i: Integer;
begin
  for i:=0 to lstProvider.Count-1 do
    TObject(lstProvider.Objects[i]).Free;
  lstProvider.Clear;
end;

procedure TMapViewerEngine.ConstraintZoom(var aWin: TMapWindow);
var
  zMin, zMax: integer;
begin
  if Assigned(aWin.MapProvider) then
  begin
    aWin.MapProvider.GetZoomInfos(zMin, zMax);
    if aWin.Zoom < zMin then
      aWin.Zoom := zMin;
    if aWin.Zoom > zMax then
      aWin.Zoom := zMax;
  end;
end;

{ Returns true when the visible window crosses the date line, i.e. the
  longitudes at the left of the window are greater than those at the right. }
function TMapViewerEngine.CrossesDateline: Boolean;
var
  visArea: TRealArea;
  mapWidth: Int64;
begin
  // A non-cyclic map cannot cross the date line.
  if not FCyclic then
    exit(false);

  // Catch the case, that the screen is wider than the whole world
  mapWidth := mvGeoMath.ZoomFactor(MapWin.Zoom) * TileSize.CX;
  Result := (MapWin.Width > mapWidth);
  if not Result then
  begin
    // Or: date line is visible when the longitude of the map's left side is
    // larger than the longitude of the right side.
    visArea.TopLeft := ScreenToLatLon(Point(0, 0));
    visArea.BottomRight := ScreenToLatLon(Point(Width, Height));
    Result := visArea.TopLeft.Lon > visArea.BottomRight.Lon;
  end;
end;

procedure TMapViewerEngine.DblClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt.X := FDragObj.MouseX;
  pt.Y := FDragObj.MouseY;
  SetCenter(ScreenToLatLon(pt));
end;

procedure TMapViewerEngine.DoDrag(Sender: TDragObj);
begin
  if Sender.DragSrc = self then
  begin
    FInDrag := True;
    MoveMapCenter(Sender);
  end;
end;

procedure TMapViewerEngine.DoEndDrag(Sender: TDragObj);
begin
  FInDrag := False;
end;

function TMapViewerEngine.DoCenterMoving(var ANewPoint: TRealPoint): Boolean;
begin
  Result := true;
  if Assigned(FOnCenterMoving) then
    FOnCenterMoving(Self, ANewPoint, Result);
end;

function TMapViewerEngine.DoZoomChanging(ANewZoom: Integer): Boolean;
begin
  Result := true;
  if Assigned(FOnZoomChanging) then
    FOnZoomChanging(Self, ANewZoom, Result);
end;

procedure TMapViewerEngine.DrawStretchedTile(const TileId: TTileID; X,
  Y: Integer; TileImg: TPictureCacheItem; const R: TRect);
begin
  if Assigned(FOnDrawStretchedTile) then
    FOnDrawStretchedTile(TileId, X, Y, TileImg, R);
end;

procedure TMapViewerEngine.DrawTile(const TileId: TTileId; X, Y: integer;
  TileImg: TPictureCacheItem);
begin
  if Assigned(FOnDrawTile) then
    FOnDrawTile(TileId, X, Y, TileImg);
end;

procedure TMapViewerEngine.evDownload(Data: TObject; Job: TJob);
var
  Id: TTileId;
  Url: String;
  Env: TEnvTile;
  MapO: TMapProvider;
  lStream: TMemoryStream;
begin
  Env := TEnvTile(Data);
  Id := Env.Tile;
  MapO := Env.Win.MapProvider;
  if Assigned(MapO) and Assigned(Cache) then
  begin
    if not Cache.InCache(MapO, Id) then
    begin
      if Assigned(FDownloadEngine) then
      begin
        Url := MapO.GetUrlForTile(Id);
        if Url <> '' then
        begin
          lStream := TMemoryStream.Create;
          try
            try
              FDownloadEngine.DownloadFile(Url, lStream);
              if (lStream.Size = 0) then
                Job.Cancel
              else
              if Assigned(Cache) then
                Cache.Add(MapO, Id, lStream);
            except
            end;
          finally
            FreeAndNil(lStream);
          end;
        end;
      end;
    end;
  end;

  if Job.Cancelled then
    Exit;

  if DrawTitleInGuiThread then
    Queue.QueueAsyncCall(@TileDownloaded, PtrInt(Env))
  else
    TileDownloaded(PtrInt(Env));
end;

function TMapViewerEngine.GetCacheOnDisk: Boolean;
begin
  Result := Cache.UseDisk;
end;

function TMapViewerEngine.GetCacheItemClass: TPictureCacheItemClass;
begin
  Result := Cache.CacheItemClass;
end;

function TMapViewerEngine.GetCacheMaxAge: Integer;
begin
  Result := Cache.MaxAge;
end;

function TMapViewerEngine.GetCachePath: String;
begin
  Result := Cache.BasePath;
end;

function TMapViewerEngine.GetCacheMemMaxItemCount: Integer;
begin
  Result := Cache.MemMaxItemCount;
end;

function TMapViewerEngine.GetCacheMemMaxItemCountDefault: Integer;
begin
  Result := Cache.CacheMemMaxItemCountDefault;
end;

function TMapViewerEngine.GetCenter: TRealPoint;
begin
  Result := MapWin.Center;
end;

function TMapViewerEngine.GetHeight: integer;
begin
  Result := MapWin.Height
end;

class function TMapViewerEngine.GetProjectionType(const aWin: TMapWindow
  ): TProjectionType;
begin
  if Assigned(AWin.MapProvider)
    then Result := AWin.MapProvider.ProjectionType
    else Result := ptEPSG3857; // Default
end;

function TMapViewerEngine.GetMapProjectionType: TProjectionType;
begin
  Result := GetProjectionType(MapWin);
end;

function TMapViewerEngine.GetMapProvider: String;
begin
  if Assigned(MapWin.MapProvider) then
    Result := MapWin.MapProvider.Name
  else
    Result := '';
end;

procedure TMapViewerEngine.GetMapProviders(AList: TStrings);
begin
  AList.Assign(lstProvider);
end;

function TMapViewerEngine.MapProviderByName(AName: String): TMapProvider;
var
  I: Integer;
begin
  I := lstProvider.IndexOf(AName);
  if I <> -1
    then Result := TMapProvider(lstProvider.Objects[I])
    else Result := Nil;
end;

function TMapViewerEngine.GetTileName(const Id: TTileId): String;
begin
  Result := IntToStr(Id.X) + '.' + IntToStr(Id.Y) + '.' + IntToStr(Id.Z);
end;

function TMapViewerEngine.GetUseThreads: Boolean;
begin
  Result := Queue.UseThreads;
end;

function TMapViewerEngine.GetWidth: integer;
begin
  Result := MapWin.Width;
end;

function TMapViewerEngine.GetZoom: integer;
begin
  Result := MapWin.Zoom;
end;

function TMapViewerEngine.IsCurrentWin(const aWin: TMapWindow): boolean;
begin
  Result := (aWin.Zoom = MapWin.Zoom) and
            (aWin.Center.Lat = MapWin.Center.Lat) and
            (aWin.Center.Lon = MapWin.Center.Lon) and
            (aWin.Width = MapWin.Width) and
            (aWin.Height = MapWin.Height);
end;

function TMapViewerEngine.IsValidTile(const aWin: TMapWindow;
  const aTile: TTIleId): boolean;
var
  tiles: int64;
begin
  tiles := 1 shl aWin.Zoom;
  Result := (aTile.X >= 0) and (aTile.X <= tiles-1) and
            (aTile.Y >= 0) and (aTile.Y <= tiles-1);
end;

function TMapViewerEngine.DegreesToMapPixels(const AWin: TMapWindow;
  APt: TRealPoint): TPoint;
var
  pixelLocation: TPoint;
  mapWidth: Int64;
  PType: TProjectionType;
begin
  PType := GetProjectionType(AWin);
  case PType of
    ptEPSG3395: pixelLocation := DegreesToPixelsEPSG3395(AWin, APt);
    ptEPSG3857: pixelLocation := DegreesToPixelsEPSG3857(AWin, APt);
    else pixelLocation := DegreesToPixelsEPSG3857(AWin, APt);
  end;
  Result.X := pixelLocation.x + AWin.X;
  if FCyclic and CrossesDateline then
  begin
    mapWidth := mvGeoMath.ZoomFactor(AWin.Zoom) * TileSize.CX;
    while (Result.X < 0) do
      Result.X := Result.X + mapWidth;
    while (Result.X > AWin.Width) do
      Result.X := Result.X - mapWidth;
  end;
  Result.Y := pixelLocation.y + AWin.Y;
end;

// review: coth: Should it use Int64?
function TMapViewerEngine.DegreesToPixelsEPSG3857(const AWin: TMapWindow;
  APt: TRealPoint): TPoint;
const
  MIN_LATITUDE = -85.05112878;
  MAX_LATITUDE = 85.05112878;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
  TWO_PI = 2.0 * pi;
var
  factorX, factorY, px, py: Extended;
  pt: TRealPoint;
begin
  // https://epsg.io/3857
  // https://pubs.usgs.gov/pp/1395/report.pdf, page 41
  // https://en.wikipedia.org/wiki/Web_Mercator_projection
  pt.Lat := Math.EnsureRange(APt.Lat, MIN_LATITUDE, MAX_LATITUDE);
  pt.Lon := Math.EnsureRange(APt.Lon, MIN_LONGITUDE, MAX_LONGITUDE);

  factorX := TileSize.CX / TWO_PI * mvGeoMath.ZoomFactor(AWin.Zoom);
  factorY := TileSize.CY / TWO_PI * mvGeoMath.ZoomFactor(AWin.Zoom);
  px := factorX * (pt.LonRad + pi);
  py := factorY * (pi - ln( tan(pi/4 + pt.LatRad/2) ));

  Result.x := Round(px);
  Result.y := Round(py);
end;

// review: coth: Should it use Int64?
function TMapViewerEngine.DegreesToPixelsEPSG3395(const AWin: TMapWindow;
  APt: TRealPoint): TPoint;
const
  MIN_LATITUDE = -80;
  MAX_LATITUDE = 84;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
var
  px, py, lny, sny: Extended;
  pt: TRealPoint;
  cfmpx, cfmpm: Extended;
  Z: Integer;
  zoomfac: Extended;  // 2**Z
begin
  // https://epsg.io/3395
  // https://pubs.usgs.gov/pp/1395/report.pdf, page 44
  pt.Lat := Math.EnsureRange(APt.Lat, MIN_LATITUDE, MAX_LATITUDE);
  pt.Lon := Math.EnsureRange(APt.Lon, MIN_LONGITUDE, MAX_LONGITUDE);

  Z := 23 - AWin.Zoom;
  zoomfac := mvGeomath.ZoomFactor(Z);
  cfmpx := IntPower(2, 31);
  cfmpm := cfmpx / EARTH_CIRCUMFERENCE;
  px := (EARTH_CIRCUMFERENCE/2 + EARTH_EQUATORIAL_RADIUS * pt.LonRad) * cfmpm / zoomfac;

  sny := EARTH_ECCENTRICITY * sin(pt.LatRad);
  lny := tan(pi/4 + pt.LatRad/2) * power((1-sny)/(1+sny), EARTH_ECCENTRICITY/2);
  py := (EARTH_CIRCUMFERENCE/2 - EARTH_EQUATORIAL_RADIUS * ln(lny)) * cfmpm / zoomfac;

  Result.x := Round(px);
  Result.y := Round(py);
end;

function TMapViewerEngine.LatLonToScreen(APt: TRealPoint): TPoint;
begin
  Result := DegreesToMapPixels(MapWin, APt);
end;

function TMapViewerEngine.LonLatToScreen(APt: TRealPoint): TPoint;
Begin
  Result := DegreesToMapPixels(MapWin, APt);
end;

function TMapViewerEngine.LatLonToWorldScreen(APt: TRealPoint): TPoint;
begin
  Result := LatLonToScreen(APt);
  Result.X := Result.X + MapWin.X;
  Result.Y := Result.Y + MapWin.Y;
end;

function TMapViewerEngine.LonLatToWorldScreen(APt: TRealPoint): TPoint;
begin
  Result := LatLonToScreen(APt);
end;

function TMapViewerEngine.MapPixelsToDegrees(const AWin: TMapWindow;
  APoint: TPoint): TRealPoint;
begin
  Result := MapPixelsToDegrees(AWin, APoint.X, APoint.Y);
end;

function TMapViewerEngine.MapPixelsToDegrees(const AWin: TMapWindow; AX,
  AY: Double): TRealPoint;
var
  mapWidth: Double;
  mPointX, mPointY : Double;
  PType: TProjectionType;
begin
  mapWidth := mvGeoMath.ZoomFactor(AWin.Zoom) * TileSize.CX;

  if FCyclic then
  begin
    AX := AX - AWin.X;
    mPointX := AX - mapWidth * Int(AX / mapWidth); // FPC 3.0.x compatible FMod()
    while mPointX < 0 do
      mPointX := mPointX + mapWidth;
    while mPointX >= mapWidth do
      mPointX := mPointX - mapWidth;
  end else
    mPointX := EnsureRange(AX - AWin.X, 0, mapWidth);
  mPointY := EnsureRange(AY - AWin.Y, 0, mapWidth);

  PType := GetProjectionType(AWin);
  case PType of
    ptEPSG3857: Result := PixelsToDegreesEPSG3857(mPointX, mPointY, AWin.Zoom);
    ptEPSG3395: Result := PixelsToDegreesEPSG3395(mPointX, mPointY, AWin.Zoom);
    else        Result := PixelsToDegreesEPSG3857(mPointX, mPointY, AWin.Zoom);
  end;
end;

function TMapViewerEngine.PixelsToDegreesEPSG3857(AX, AY: Double; Zoom: Integer
  ): TRealPoint;
const
  MIN_LATITUDE = -85.05112878;
  MAX_LATITUDE = 85.05112878;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
var
  zoomfac: Int64;
begin
  // https://epsg.io/3857
  // https://pubs.usgs.gov/pp/1395/report.pdf, page 41

  // note: coth: ** for better readability, but breaking OmniPascal in VSCode
  // Result.LonRad := ( APoints.X / (( TILE_SIZE / (2*pi)) * 2**Zoom) ) - pi;
  // Result.LatRad := arctan( sinh(pi - (APoints.Y/TILE_SIZE) / 2**Zoom * pi*2) );
  zoomFac := mvGeoMath.ZoomFactor(Zoom);
  Result.LonRad := ( AX / (( TileSize.CX / (2*pi)) * zoomFac) ) - pi;
  Result.LatRad := arctan( sinh(pi - (AY / TileSize.CY) / zoomFac * pi*2) );

  Result.Lat := Math.EnsureRange(Result.Lat, MIN_LATITUDE, MAX_LATITUDE);
  Result.Lon := Math.EnsureRange(Result.Lon, MIN_LONGITUDE, MAX_LONGITUDE);
end;

function TMapViewerEngine.PixelsToDegreesEPSG3395(AX, AY: Double; Zoom: Integer
  ): TRealPoint;

  function PhiIteration(y, phi: Extended): Extended;
  var
    t: Extended;
    sin_phi: Extended;
    arg: Extended;
  begin
    t := exp(y/EARTH_EQUATORIAL_RADIUS);
    sin_phi := sin(phi);
    arg := (1 - EARTH_ECCENTRICITY * sin_phi) / (1 + EARTH_ECCENTRICITY * sin_phi);
    Result := pi/2 - 2*arctan( t * Math.power(arg, EARTH_ECCENTRICITY/2) );
  end;

const
  MIN_LATITUDE = -80;
  MAX_LATITUDE = 84;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
  EPS = 1e-8;
var
  LonRad, LatRad: Extended;
  WorldSize: Int64;
  Cpm: Extended;
  Z: Integer;
  t, phi:  Extended;
  zoomFac: Int64;
  i: Integer;
begin
  // https://epsg.io/3395
  // https://pubs.usgs.gov/pp/1395/report.pdf, page 44

  Z := 23 - Zoom;
  zoomFac := mvGeoMath.ZoomFactor(Z);
  WorldSize := mvGeoMath.ZoomFactor(31);
  Cpm :=  WorldSize / EARTH_CIRCUMFERENCE;

  LonRad := (AX / (Cpm/zoomFac) - EARTH_CIRCUMFERENCE/2) / EARTH_EQUATORIAL_RADIUS;
  LatRad := (AY / (Cpm/zoomFac) - EARTH_CIRCUMFERENCE/2);

  t := pi/2 - 2*arctan(exp(-LatRad/EARTH_EQUATORIAL_RADIUS));

  i := 0;
  repeat
    phi := t;
    t := PhiIteration(LatRad, phi);
    inc(i);
    if i>10 then
      Break;
      //raise Exception.Create('Phi iteration takes too long.');
  until (abs(phi - t) < EPS);

  LatRad := t;

  Result.LonRad := LonRad;
  Result.LatRad := LatRad;

  Result.Lat := Math.EnsureRange(Result.Lat, MIN_LATITUDE, MAX_LATITUDE);
  Result.Lon := Math.EnsureRange(Result.Lon, MIN_LONGITUDE, MAX_LONGITUDE);
end;

procedure TMapViewerEngine.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragObj.MouseDown(self,X,Y);
end;

procedure TMapViewerEngine.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  FDragObj.MouseMove(X,Y);
end;

procedure TMapViewerEngine.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragObj.MouseUp(X,Y);
end;

procedure TMapViewerEngine.MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Val: Integer;
  nZoom: integer;
  bZoomToCursor: Boolean;
begin
  bZoomToCursor := False;
  Val := 0;
  if WheelDelta > 0 then
    Val := 1;
  if WheelDelta < 0 then
    Val := -1;
  nZoom := EnsureRange(Zoom + Val, FZoomMin, FZoomMax);
  if ZoomToCursor then
  begin
    MapWin.ZoomCenter := ScreenToLatLon(MousePos);
    MapWin.ZoomOffset := LatLonToScreen(Center).Subtract(MousePos);
    bZoomToCursor := True;
  end;
  SetZoom(nZoom, bZoomToCursor);
  Handled := true;
end;

procedure TMapViewerEngine.MoveMapCenter(Sender: TDragObj);
var
  old: TMemObj;
  nCenter: TRealPoint;
  aPt: TPoint;
Begin
  if Sender.LnkObj = nil then
    Sender.LnkObj := TMemObj.Create(MapWin);
  old := TMemObj(Sender.LnkObj);
  aPt.X := old.FWin.Width DIV 2-Sender.OfsX;
  aPt.Y := old.FWin.Height DIV 2-Sender.OfsY;
  nCenter := MapPixelsToDegrees(old.FWin,aPt);
  SetCenter(nCenter);
end;

procedure TMapViewerEngine.PrepareCache(AMapProvider: TMapProvider);
begin
  Cache.Prepare(AMapProvider);
end;

function TMapViewerEngine.ReadProvidersFromXML(AFileName: String;
  out AMsg: String): Boolean;

  function GetSvrStr(AName: String): TGetSvrStr;
  var
    lcName: String;
  begin
    lcName := LowerCase(AName);
    if lcName = LowerCase(SVR_LETTER) then
      Result := @GetSvrLetter
    else if lcName = LowerCase(SVR_BASE1) then
      Result := @GetSvrBase1
    else
      Result := nil;
  end;

  function GetValStr(AName: String): TGetValStr;
  var
    lcName: String;
  begin
    lcName := Lowercase(AName);
    if lcName = LowerCase(STR_QUADKEY) then
      Result := @GetStrQuadKey
    else if lcName = LowerCase(STR_YAHOOY) then
      Result := @GetStrYahooY
    else if lcName = LowerCase(STR_YAHOOZ) then
      Result := @GetStrYahooZ
    else
      Result := nil;
  end;

  function GetAttrValue(ANode: TDOMNode; AttrName: String): String;
  var
    node: TDOMNode;
  begin
    Result := '';
    if ANode.HasAttributes then begin
      node := ANode.Attributes.GetNamedItem(AttrName);
      if Assigned(node) then Result := node.NodeValue;
    end;
  end;

var
  stream: TFileStream;
  doc: TXMLDocument = nil;
  node, layerNode: TDOMNode;
  providerName: String;
  projectionType: TProjectionType;
  url: String;
  minZoom: Integer;
  maxZoom: Integer;
  svrCount: Integer;
  s: String;
  svrProc: String;
  xProc: String;
  yProc: String;
  zProc: String;
  first: Boolean;
begin
  Result := false;
  AMsg := '';
  stream := TFileStream.Create(AFileName, fmOpenread or fmShareDenyWrite);
  try
    ReadXMLFile(doc, stream, [xrfAllowSpecialCharsInAttributeValue, xrfAllowLowerThanInAttributeValue]);
    node := doc.FindNode('map_providers');
    if node = nil then begin
      AMsg := 'No map providers in file.';
      exit;
    end;

    first := true;
    node := node.FirstChild;
    while node <> nil do begin
      providerName := GetAttrValue(node, 'name');
      layerNode := node.FirstChild;
      while layerNode <> nil do begin
        url := GetAttrValue(layerNode, 'url');
        if url = '' then
          continue;
        s := GetAttrValue(layerNode, 'minZom');
        if s = '' then minZoom := 0
          else minZoom := StrToInt(s);
        s := GetAttrValue(layerNode, 'maxZoom');
        if s = '' then maxzoom := 9
          else maxZoom := StrToInt(s);
        s := GetAttrValue(layerNode, 'serverCount');
        if s = '' then svrCount := 1
          else svrCount := StrToInt(s);
        s := Concat('pt', GetAttrValue(layerNode, 'projection'));
        projectionType := TProjectionType(GetEnumValue(TypeInfo(TProjectionType), s)); //-1 will default to ptEPSG3857
        svrProc := GetAttrValue(layerNode, 'serverProc');
        xProc := GetAttrValue(layerNode, 'xProc');
        yProc := GetAttrValue(layerNode, 'yProc');
        zProc := GetAttrValue(layerNode, 'zProc');
        layerNode := layerNode.NextSibling;
      end;
      if first then begin
        ClearMapProviders;
        first := false;
      end;
      AddMapProvider(providerName, projectionType,
        url, minZoom, maxZoom, svrCount,
        GetSvrStr(svrProc), GetValStr(xProc), GetValStr(yProc), GetValStr(zProc)
      );
      node := node.NextSibling;
    end;
    Result := true;
  finally
    stream.Free;
    doc.Free;
  end;
end;

procedure TMapViewerEngine.Redraw;
begin
  Redraw(MapWin, FDragObj.InDrag);
end;

procedure TMapViewerEngine.Redraw(const aWin: TMapWindow;
  const paintOnly: Boolean);
var
  TilesVis: TArea;
  x, y, px, py: Integer;
  iTile, numTiles, XShift: Integer;
  Tiles: TTileIdArray = nil;
  tile: TTileID;
  previewDrawn: Boolean;
  previewImg: TPictureCacheItem;
  tmpImg : TPictureCacheItem;
  R: TRect;

  procedure AddJob;
  var
    lTile: TEnvTile;
    lJob: TEventJob;
  begin
    lTile:=TEnvTile.Create(Tiles[iTile], aWin);
    lJob := TEventJob.Create(@evDownload, lTile, False,                                    // owns data
      GetTileName(Tiles[iTile]));
    if not Queue.AddUniqueJob(lJob, Self) then
    begin
      FreeAndNil(lJob);
      FreeAndNil(lTile);
    end;
  end;

  procedure EraseAround;
  var
    T, L, B, R: Integer;
  begin
    T := -AWin.Y div TileSize.CY - Max(0, Sign(AWin.Y));
    B := T + AWin.Height div TileSize.CY + 1;
    L := -AWin.X div TileSize.CX - Max(0, Sign(AWin.X));
    R := L + AWin.Width div TileSize.CX + 1;
    if T < TilesVis.top then  // Erase above top
      EraseBackground(Rect(0, 0, AWin.Width, AWin.Y + TilesVis.top * TileSize.CY));
    if L < TilesVis.left then // Erase on the left
      EraseBackground(Rect(
        0,
        AWin.Y + TilesVis.top * TileSize.CY,
        AWin.X + TilesVis.left * TileSize.CX,
        AWin.Y + (TilesVis.bottom + 1) * TileSize.CY)
      );
    if R > TilesVis.right then // Erase on the right
      EraseBackground(Rect(
        AWin.X + (TilesVis.right + 1) * TileSize.CX,
        AWin.Y + TilesVis.top * TileSize.CY,
        AWin.Width,
        AWin.Y + (TilesVis.bottom + 1) * TileSize.CY)
      );
    if B > TilesVis.Bottom then // Erase below
      EraseBackground(Rect(
        0,
        AWin.Y + (TilesVis.bottom + 1) * TileSize.CY,
        AWin.Width,
        AWin.Height)
      );
  end;

begin
  if not(Active) then
    Exit;
  if not Assigned(AWin.MapProvider) then
  begin
    EraseBackground(Rect(0, 0, AWin.Width, AWin.Height));
    Exit;
  end;

  if not CalculateVisibleTiles(AWin, TilesVis) then
    EraseAround;
  SetLength(Tiles, (TilesVis.Bottom - TilesVis.Top + 1) * (TilesVis.Right - TilesVis.Left + 1));
  iTile := Low(Tiles);
  numTiles := 1 shl AWin.Zoom;
  XShift := IfThen(aWin.X > 0, numTiles - aWin.X div TileSize.CX - 1, 0);
  for y := TilesVis.Top to TilesVis.Bottom do
    for X := TilesVis.Left to TilesVis.Right do
    begin
      if FCyclic then
      begin // 0,1,2,3,4,5 --> 15,16,>0<,1,2,3
        Tiles[iTile].X := (X + XShift) mod numTiles;
        if Tiles[iTile].X < 0 then  //
          Tiles[iTile].X := Tiles[iTile].X + numTiles;
      end
      else
        Tiles[iTile].X := X;
      Tiles[iTile].Y := Y;
      Tiles[iTile].Z := AWin.Zoom;

      if Cache.InCache(AWin.MapProvider, Tiles[iTile]) then
      begin
        DrawTileFromCache(Tiles[iTile], AWin);
      end
      else
      // Avoid tiling artefacts when a tile does not exist (lowest zoom) or
      // is not valid
      begin
        previewdrawn := False;
        py := AWin.Y {%H-}+ Y * TileSize.CY;
        px := AWin.X {%H-}+ X * TileSize.CX;
        if FDrawPreviewTiles then
        begin
          if IsValidTile(AWin, Tiles[iTile]) then  // Invalid tiles probably will not be found in the cache
          begin
            tile := Tiles[iTile];
            if Cache.GetPreviewFromCache(AWin.MapProvider, tile, R) then
            begin
              Cache.GetFromCache(AWin.MapProvider, tile, previewImg);
              tmpImg := nil;
              try
                if FCreateTempTileCopy then
                begin
                  // If a local copy is needed, create one
                  tmpImg := Cache.CacheItemClass.Create(previewImg);
                  // replace the pointer to the original image by the copy
                  previewImg := tmpImg;
                end;
                if Assigned(FTileAfterGetFromCacheEvent) then
                  FTileAfterGetFromCacheEvent(AWin.MapProvider,tile,previewImg);
                DrawStretchedTile(Tiles[iTile], px, py, previewImg, R);
              finally
                // free the copy if
                if Assigned(tmpImg) then
                  tmpImg.Free;
              end;

              previewDrawn := true;
            end;
          end;
        end;
        if not previewDrawn then
          DrawTile(Tiles[iTile], px, py, nil);  // Draw blank tile if preview cannot be generated

        if not paintOnly and IsValidTile(AWin, Tiles[iTile]) then
        begin
          AddJob;
          inc(iTile);
        end;
      end;
    end;
  SetLength(Tiles, iTile);
end;

procedure TMapViewerEngine.RegisterProviders;
var
  HERE1, HERE2: String;
begin
  {$I mvengine_mapreg.inc}
  MapWin.MapProvider := Nil; // Undo the OSM Mapnik default selection
end;

function TMapViewerEngine.ScreenRectToRealArea(ARect: TRect): TRealArea;
begin
  Result.TopLeft := MapPixelsToDegrees(MapWin, ARect.TopLeft);
  // Extend the area to include the BottomRight row/column
  Result.BottomRight := MapPixelsToDegrees(MapWin,
    ARect.BottomRight.X + 0.9999999, ARect.BottomRight.Y + 0.9999999);
end;

function TMapViewerEngine.ScreenToLatLon(aPt: TPoint): TRealPoint;
begin
  Result := MapPixelsToDegrees(MapWin, aPt);
end;

function TMapViewerEngine.ScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  Result := MapPixelsToDegrees(MapWin, aPt);
end;

procedure TMapViewerEngine.SetActive(AValue: boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;

  // One problem at designtime is that the control is creating cache directories
  // at unexpected places when Cache.BasePath is relative
  if csDesigning in ComponentState then
    exit;

  if not FActive then
    Queue.CancelAllJob(self)
  else begin
    if Cache.UseDisk then ForceDirectories(Cache.BasePath);
    Redraw(MapWin);
  end;
end;

procedure TMapViewerEngine.SetBkColor(AValue: TFPColor);
begin
  if FBkColor = AValue then Exit;
  FBkColor := AValue;
  Redraw(MapWin);
end;

procedure TMapViewerEngine.SetCacheItemClass(AValue: TPictureCacheItemClass);
begin
  if Cache.CacheItemClass = AValue then
    Exit;
  Cache.CacheItemClass := AValue;
end;

procedure TMapViewerEngine.SetCacheMaxAge(AValue: Integer);
begin
  if Cache.MaxAge = AValue then
    Exit;
  Cache.MaxAge := AValue;
end;

procedure TMapViewerEngine.SetCacheOnDisk(AValue: Boolean);
begin
  if Cache.UseDisk = AValue then Exit;
  Cache.UseDisk := AValue;
end;

procedure TMapViewerEngine.SetCachePath(AValue: String);
begin
  Cache.BasePath := SetDirSeparators(aValue);
end;

procedure TMapViewerEngine.SetCacheMemMaxItemCount(AValue: Integer);
begin
  Cache.MemMaxItemCount := AValue;
end;

procedure TMapViewerEngine.SetCenter(ACenter: TRealPoint);
begin
  if (MapWin.Center.Lon = aCenter.Lon) and (MapWin.Center.Lat = aCenter.Lat) then
    exit;
  if not DoCenterMoving(ACenter) then
    exit;
  Mapwin.Center := aCenter;
  CalculateWin(MapWin);
  if Assigned(OnCenterMove) then
    OnCenterMove(Self);
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TMapViewerEngine.SetCyclic(AValue: Boolean);
begin
  if FCyclic = AValue then exit;
  FCyclic := AValue;
  if Assigned(MapWin.MapProvider) and CrossesDateLine then
    Redraw;
end;

procedure TMapViewerEngine.SetDownloadEngine(AValue: TMvCustomDownloadEngine);
begin
  if FDownloadEngine = AValue then Exit;
  FDownloadEngine := AValue;
  if Assigned(FDownloadEngine) then
    FDownloadEngine.FreeNotification(self);
end;

procedure TMapViewerEngine.SetHeight(AValue: integer);
begin
  if MapWin.Height = AValue then Exit;
  MapWin.Height := AValue;
  CalculateWin(MapWin);
  Redraw(MapWin);
end;

procedure TMapViewerEngine.SetMapProvider(AValue: String);
var
  Provider: TMapProvider;
begin
  Provider := MapProviderByName(AValue);
  if not ((aValue = '') or Assigned(Provider)) then
    raise Exception.Create('Unknow Provider: ' + aValue);
  if Assigned(MapWin.MapProvider) and (MapWin.MapProvider.Name = AValue) then Exit;
  MapWin.MapProvider := Provider;
  if Assigned(Provider) then
  begin
    PrepareCache(Provider);
    ConstraintZoom(MapWin);
    CalculateWin(MapWin);
    Redraw(MapWin);
  end;
end;

procedure TMapViewerEngine.SetSize(aWidth, aHeight: integer);
begin
  if (MapWin.Width = aWidth) and (MapWin.Height = aHeight) then Exit;
  CancelCurrentDrawing;
  MapWin.Width := aWidth;
  MapWin.Height := aHeight;
  CalculateWin(MapWin);
  Redraw(MapWin);
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TMapViewerEngine.SetUseThreads(AValue: Boolean);
begin
  if Queue.UseThreads = AValue then Exit;
  Queue.UseThreads := AValue;
  Cache.UseThreads := AValue;
end;

procedure TMapViewerEngine.SetWidth(AValue: integer);
begin
  if MapWin.Width = AValue then Exit;
  MapWin.Width := AValue;
  CalculateWin(MapWin);
  Redraw(MapWin);
end;

procedure TMapViewerEngine.SetZoom(AValue: Integer);
begin
  SetZoom(AValue, false);
end;

procedure TMapViewerEngine.SetZoom(AValue: integer; AZoomToCursor: Boolean);
begin
  if MapWin.Zoom = AValue then Exit;

  if not DoZoomChanging(AValue) then
    Exit;

  MapWin.Zoom := AValue;
  ConstraintZoom(MapWin);
  CalculateWin(MapWin);
  if AZoomToCursor then
    AdjustZoomCenter(MapWin);
  Redraw(MapWin, True);
  if Assigned(OnZoomChange) then
    OnZoomChange(Self);
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TMapViewerEngine.TileDownloaded(Data: PtrInt);
var
  EnvTile: TEnvTile;
begin
  EnvTile := TEnvTile(Data);
  try
    if Assigned(FOnTileDownloaded) then
      FOnTileDownloaded(EnvTile.Tile);
  finally
    FreeAndNil(EnvTile);
  end;
end;

procedure TMapViewerEngine.EraseBackground(const R: TRect);
begin
  if Assigned(FOnEraseBackground) then
      FOnEraseBackground(R);
end;

procedure TMapViewerEngine.DrawTileFromCache(constref ATile: TTileId; constref
  AWin: TMapWindow);
var
  img, tmpImg: TPictureCacheItem;
  X, Y: integer;
  worldWidth : Integer;
  numTiles : Integer;
  baseX : Integer;
begin
  if IsCurrentWin(AWin)then
  begin
     Cache.GetFromCache(AWin.MapProvider, ATile, img);
     tmpImg := Nil;
     try
       // if a local copy is needed, create one
       if FCreateTempTileCopy then
       begin
         tmpImg := Cache.CacheItemClass.Create(img);
         img := tmpImg; // Replace the pointer to the cached image by the copy
       end;
       if Assigned(FTileAfterGetFromCacheEvent) then
         FTileAfterGetFromCacheEvent(AWin.MapProvider,ATile,img);
       Y := AWin.Y + ATile.Y * TileSize.CY; // begin of Y
       if Cyclic then
       begin
         baseX := AWin.X + ATile.X * TileSize.CX; // begin of X
         numTiles := 1 shl AWin.Zoom;
         worldWidth := numTiles * TileSize.CX;
         // From the center to the left (western) hemisphere
         X := baseX;
         while (X + TileSize.CX >= 0) do
         begin
           DrawTile(ATile, X, Y, img);
           X := X - worldWidth;
         end;
         // From the center to the right (eastern) hemisphere
         X := baseX + worldWidth;
         while ((X - TileSize.CX) <= AWin.Width) do
         begin
           DrawTile(ATile, X, Y, img);
           X := X + worldWidth;
         end;
       end else
       begin
         X := AWin.X + ATile.X * TileSize.CX; // begin of X
         DrawTile(ATile, X, Y, img);
       end;
     finally
       if Assigned(tmpImg) then
         tmpImg.Free;
     end;
  end;
end;

function TMapViewerEngine.ValidProvider(const AProvider: String): Boolean;
begin
  Result := lstProvider.IndexOf(AProvider) > -1;
end;

function TMapViewerEngine.WorldScreenToLatLon(aPt: TPoint): TRealPoint;
begin
  aPt.X := aPt.X - MapWin.X;
  aPt.Y := aPt.Y - MapWin.Y;
  Result := ScreenToLatLon(aPt);
end;

function TMapViewerEngine.WorldScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  Result := WorldScreenToLatLon(aPt);
end;

procedure TMapViewerEngine.WriteProvidersToXML(AFileName: String);
var
  doc: TXMLDocument;
  root: TDOMNode;
  i: Integer;
  prov: TMapProvider;
begin
  doc := TXMLDocument.Create;
  try
    root := doc.CreateElement('map_providers');
    doc.AppendChild(root);
    for i := 0 to lstProvider.Count - 1 do begin
      prov := TMapProvider(lstProvider.Objects[i]);
      prov.ToXML(doc, root);
    end;
    WriteXMLFile(doc, AFileName);
  finally
    doc.Free;
  end;
end;

procedure TMapViewerEngine.ZoomOnArea(const aArea: TRealArea);
var
  tmpWin: TMapWindow;
  visArea: TRealArea;
  TopLeft, BottomRight: TPoint;
begin
  tmpWin := MapWin;
  tmpWin.Center.Lon := (aArea.TopLeft.Lon + aArea.BottomRight.Lon) / 2;
  tmpWin.Center.Lat := (aArea.TopLeft.Lat + aArea.BottomRight.Lat) / 2;
  tmpWin.Zoom := FZoomMax;
  TopLeft.X := 0;
  TopLeft.Y := 0;
  BottomRight.X := tmpWin.Width;
  BottomRight.Y := tmpWin.Height;
  Repeat
    CalculateWin(tmpWin);
    visArea.TopLeft := MapPixelsToDegrees(tmpWin, TopLeft);
    visArea.BottomRight := MapPixelsToDegrees(tmpWin, BottomRight);
    if AreaInsideArea(aArea, visArea) then
      break;
    dec(tmpWin.Zoom);
  until (tmpWin.Zoom = 2);
  MapWin := tmpWin;
  Redraw(MapWin);
end;

procedure TMapViewerEngine.CopyMapWindowFrom(AEngine: TMapViewerEngine);
var
  MP: TMapProvider;
begin
  MP := MapWin.MapProvider;
  MapWin := AEngine.MapWin;
  MapWin.MapProvider := MP; // Restore the old Map Provider
end;

{------------------------------------------------------------------------------}

function DMSToDeg(Deg, Min: Word; Sec: Double): Double;
begin
  Result := mvGeoMath.DMSToDeg(Deg, Min, Sec);
end;

function GPSToDMS(Angle: Double): string;
begin
  Result := mvGeoMath.GPSToDMS(Angle);
end;

function GPSToDMS(Angle: Double; AFormatSettings: TFormatSettings): string;
begin
  Result := mvGeoMath.GPSToDMS(Angle, AFormatSettings);
end;

function LatToStr(ALatitude: Double; DMS: Boolean): String;
begin
  Result := mvGeoMath.LatToStr(ALatitude, DMS);
end;

function LatToStr(ALatitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
begin
  Result := mvGeoMath.LatToStr(ALatitude, DMS, AFormatSettings);
end;

function LonToStr(ALongitude: Double; DMS: Boolean): String;
begin
  Result := mvGeoMath.LonToStr(ALongitude, DMS);
end;

function LonToStr(ALongitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
begin
  Result := mvGeoMath.LonToStr(ALongitude, DMS, AFormatSettings);
end;

function TryStrToGps(const AValue: String; out ADeg: Double): Boolean;
begin
  Result := mvGeoMath.TryStrToGps(AValue, ADeg);
end;

function TryStrDMSToDeg(const AValue: String; out ADeg: Double): Boolean;
begin
  Result := mvGeoMath.TryStrDMSToDeg(AValue, ADeg);
end;

function ZoomFactor(AZoomLevel: Integer): Int64;
begin
  Result := mvGeoMath.ZoomFactor(AZoomLevel);
end;

end.

