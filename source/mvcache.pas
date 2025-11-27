{
  Picture cache manager
    (C) 2014 ti_dic@hotmail.com

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, IntfGraphics, syncObjs, FileUtil, LazFileUtils,
  mvMapProvider, mvTypes, FPImage;

Type
   EMvCacheException = class(EMapViewerException);

   { TPictureCacheItem }

   TPictureCacheItem = class(TObject)
   protected
     function GetImageObject: TObject; virtual;
     class function GetImageReader({%H-}AStream: TStream): TFPCustomImageReader;
   public
     constructor Create({%H-}AStream: TStream); virtual;
     destructor Destroy; override;
   end;

   TPictureCacheItemClass = class of TPictureCacheItem;

   { TPictureCache }

   TPictureCache = Class(TComponent)
   private
     FCacheItemClass: TPictureCacheItemClass;
     FMaxAge: Integer;
     FMemMaxItemCount: Integer;
     FCacheObjectList : TFPObjectList;
     FCacheIDs: TStringList;
     FLock: TCriticalSection;
     FBasePath: String;
     FUseDisk: Boolean;
     FUseThreads: Boolean;
     procedure SetMemMaxItemCount(Value: Integer);
     procedure SetCacheItemClass(AValue: TPictureCacheItemClass);
     procedure SetUseThreads(AValue: Boolean);
     procedure EnterLock;
     procedure LeaveLock;
     function GetCacheMemMaxItemCountDefault: Integer;
   protected
     procedure AddItem(const Item: TPictureCacheItem; const AIDString: String);
     procedure DeleteItem(const AItemIndex : Integer);
     function DiskCached(const aFileName: String): Boolean;
     function GetFileName(MapProvider: TMapProvider; const TileId: TTileId): String;
     procedure LoadFromDisk(const aFileName: String; out Item: TPictureCacheItem);
     function MapProvider2FileName(MapProvider: TMapProvider): String;
   public
     constructor Create(aOwner: TComponent); override;
     destructor Destroy; override;
     procedure Add(const MapProvider: TMapProvider; const TileId: TTileId; const Stream: TMemoryStream);
     procedure CheckCacheSize;
     procedure CheckCacheSize(Sender: TObject); deprecated 'Use CheckCacheSize without parameters!';
     procedure ClearCache;
     procedure DeleteCacheFiles(const MapProvider: TMapProvider);
     procedure GetFromCache(const MapProvider: TMapProvider; const TileId: TTileId; out Item: TPictureCacheItem);
     function GetPreviewFromCache(const MapProvider: TMapProvider; var TileId: TTileId; out ARect: TRect): boolean;
     function InCache(const MapProvider: TMapProvider; const TileId: TTileId): Boolean;
     procedure Prepare(const MapProvider: TMapProvider);

     property BasePath: String read FBasePath write FBasePath;
     property CacheItemClass: TPictureCacheItemClass read FCacheItemClass write SetCacheItemClass;
     property CacheMemMaxItemCountDefault: Integer read GetCacheMemMaxItemCountDefault;
     property MaxAge: Integer read FMaxAge write FMaxAge; // in days
     property MemMaxItemCount: Integer read FMemMaxItemCount write SetMemMaxItemCount;
     property UseDisk: Boolean read FUseDisk write FUseDisk;
     property UseThreads: Boolean read FUseThreads write SetUseThreads;
   end;


implementation

uses
  GraphType, DateUtils, FPReadJPEG;

const
  // Tiles kept in memory
  // One tile has approx 256*256*4 Bytes = 256KBytes, 128 Tiles in Memory will consume 32MB of Memory
  MEMCACHE_MIN = 16;
  MEMCACHE_DEFAULT = 128; //64;
  MEMCACHE_MAX = 1024;
  MEMCACHE_SWEEP_CNT = 10; // Max tiles to be swept at once
  FLAT_CACHE = false;      // all cache files in flat folder, or grouped by provider and zoom

function IsValidPNG(AStream: TStream): Boolean;
var
  s: string = '';
  y: Int64;
begin
  if Assigned(AStream) then
  begin
    SetLength(s, 3);
    y := AStream.Position;
    AStream.Position := 1;
    AStream.Read(s[1], 3);
    AStream.Position := y;
    Result := (s = 'PNG');
  end
  else
    Result := false;
end;

function IsValidJPEG(AStream: TStream): Boolean;
var
  s: string = '';
  y: Int64;
begin
  if Assigned(AStream) then
  begin
    SetLength(s, 4);
    y := AStream.Position;
    AStream.Position := 6;
    AStream.Read(s[1], 4);
    AStream.Position := y;
    Result := (s = 'JFIF') or (s = 'Exif');
  end
  else
    Result := false;
end;

{ TPictureCacheItem }

constructor TPictureCacheItem.Create(AStream: TStream);
begin
  {empty}
end;

destructor TPictureCacheItem.Destroy;
begin
  inherited Destroy;
end;

function TPictureCacheItem.GetImageObject: TObject;
begin
  Result := Nil;
end;

class function TPictureCacheItem.GetImageReader(AStream: TStream): TFPCustomImageReader;
begin
  Result := Nil;
  if not Assigned(AStream) then
     Exit;
  if IsValidJPEG(AStream) then
    Result := TFPReaderJPEG.Create
  else if IsValidPNG(AStream) then
    Result := TLazReaderPNG.Create;
end;

{ TPictureCache

  Some explanation about the internal cache memory.

  There are two lists.
  - The first one is the FCacheObjectList (type TFPObjectList) which contains
    the stored objects (cache items).
  - The second one is the FCacheIDs (type TStringList), which contains the
    IDString (corresponding to the filename) and in the objects property the
    reference to the object stored in the FCacheObjectList.

  This stringlist is sorted to speed up the access to the cache items.

  The FCacheObjectList contains the oldest objects in the lower indices,
  the newer ones at the end. If an item is retrieved and located in the lower half
  of the indices, it is placed again at the end. This keeps often-used items in the
  cache.

  If the cache is full, the list is reduced to FMemMaxItemCount-MEMCACHE_SWEEP_CNT,
  so that tiles do not have to be deleted on every added tile.
}
constructor TPictureCache.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCacheItemClass := TPictureCacheItem;
  FMemMaxItemCount := MEMCACHE_DEFAULT;
  FMaxAge := MaxInt;
  FCacheObjectList := TFPObjectList.Create(True);  // Owns the objects
  FCacheIDs := TStringList.Create;
  FCacheIDs.Sorted := True;
  FCacheIDs.Duplicates := dupAccept;
end;

destructor TPictureCache.Destroy;
begin
  ClearCache;
  FreeAndNil(FCacheObjectList);
  FreeAndNil(FCacheIDs);
  FreeAndNil(FLock);
  inherited;
end;

procedure TPictureCache.SetUseThreads(AValue: Boolean);
begin
  if FUseThreads = AValue then Exit;
  FUseThreads := AValue;
  if aValue then
    FLock := TCriticalSection.Create
  else
    FreeAndNil(FLock);
end;

procedure TPictureCache.SetMemMaxItemCount(Value: Integer);
var
  newcnt : Integer;
begin
  if Value < MEMCACHE_MIN then
    newcnt := MEMCACHE_MIN
  else if Value > MEMCACHE_MAX then
    newcnt := MEMCACHE_MAX
  else
    newcnt := Value;
  if FMemMaxItemCount <> newcnt then
  begin
    FMemMaxItemCount := newcnt;
    CheckCacheSize;
  end;
end;

procedure TPictureCache.SetCacheItemClass(AValue: TPictureCacheItemClass);
begin
  if FCacheItemClass = AValue then Exit;
  FCacheItemClass := AValue;
  ClearCache;
end;

procedure TPictureCache.EnterLock;
begin
  if Assigned(FLock) then
    FLock.Enter;
end;

procedure TPictureCache.LeaveLock;
begin
  if Assigned(FLock) then
    FLock.Leave;
end;

function TPictureCache.GetCacheMemMaxItemCountDefault: Integer;
begin
  Result := MEMCACHE_DEFAULT;
end;

procedure TPictureCache.ClearCache;
begin
  EnterLock;
  try
    FCacheIDs.Clear; // Delete all ID strings
    FCacheObjectList.Clear;
  finally
    LeaveLock;
  end;
end;

procedure TPictureCache.DeleteCacheFiles(const MapProvider: TMapProvider);
var
  prov: string;

  procedure DoDeleteFiles(ACacheDir: String);
  begin
    if DeleteDirectory(ACacheDir, false) then
      RemoveDirUTF8(ACacheDir);
  end;

begin
  if MapProvider = nil then
    exit;
  prov := MapProvider2FileName(MapProvider);
  DoDeleteFiles(FBasePath + prov);
  if MapProvider.HasRetinaTiles then
    DoDeleteFiles(FBasePath + prov + '-512');
end;

function TPictureCache.MapProvider2FileName(MapProvider: TMapProvider): String;
var
  i: integer;
begin
  Result := '';
  if Assigned(MapProvider) then
  begin
    Result := MapProvider.Name;
    for i := 1 to Length(Result) do
      if not (Result[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.']) then
        Result[i] := '-';
  end;
end;

function TPictureCache.DiskCached(const aFileName: String): Boolean;
var
  FullFileName: string;
  Age: TDateTime;
begin
  if UseDisk then
  begin
    FullFileName := BasePath + aFileName;
    Result := FileAge(fullFileName, Age) and (DaysBetween(Now, Age) <= FMaxAge);
  end
  else
    Result := False;
end;

procedure TPictureCache.LoadFromDisk(const aFileName: String; 
  out Item: TPictureCacheItem);
var
  FullFileName: String;
  lStream: TFileStream;
begin
  Item := nil;
  if DiskCached(aFileName) then
  begin
    FullFileName := BasePath + aFileName;
    lStream := TFileStream.Create(FullFileName, fmOpenRead or fmShareDenyWrite);
    try
      try
        Item := FCacheItemClass.Create(lStream);
      except
        FreeAndNil(Item);
      end;
      if Assigned(Item) then
        AddItem(Item, aFileName);
    finally
      lStream.Free;
    end;
  end;
end;

function TPictureCache.GetFileName(MapProvider: TMapProvider;
  const  TileId: TTileId): String;
var
  prov: String;
  retina: String = '';
begin
  prov := MapProvider2FileName(MapProvider);
  if MapProvider.RequestRetinaTiles and MapProvider.HasRetinaTiles then
    retina := '-512';

  if FLAT_CACHE then
    {%H-}Result := Format('%s%s_%d_%d_%d', [prov, retina, TileId.X, TileId.Y, TileId.Z])
  else
    // "provider/zoom/x/y" or "provider_512/zoom/x/y"
    Result := SetDirSeparators(Format('%s%s/%d/%d/%d', [prov, retina, TileID.Z, TileID.X, TileID.Y]));
end;

{ AddItem allows the insertion of an existing TPictureCacheItem.
  CAUTION: This will not create any file on disk! }
procedure TPictureCache.AddItem(const Item: TPictureCacheItem;
  const AIDString: String);
var
  pci, pci0 : TPictureCacheItem;
  ndx, ndxi : Integer;
begin
  EnterLock;
  try
    // First check if an item with this ID is in the list
    ndx := FCacheIDs.IndexOf(AIDString);
    if ndx >= 0 then
    begin  // Delete the Item
      ndxi := FCacheObjectList.IndexOf(FCacheIDs.Objects[ndx]);
      if ndxi >= 0 then
        DeleteItem(ndxi);
    end;
    pci := Item;
    try
      try
        FCacheObjectList.Add(Item);
        pci0 := pci; // from here the Item is in the object list
        pci := Nil; // so nil
        FCacheIDs.AddObject(AIDString,pci0);
        CheckCacheSize();
      except
      end;
    finally
      if Assigned(pci) then
        pci.Free; // assigned, that means the add to the objectlist failed. Free item
    end;
  finally
    LeaveLock;
  end;
end;

procedure TPictureCache.DeleteItem(const AItemIndex: Integer);
var
  i: Integer;
  pci : TPictureCacheItem;
  cnt : Integer;
begin
  EnterLock;
  try
    cnt := FCacheObjectList.Count;
    if (AItemIndex < 0) or (AItemIndex >= cnt) then Exit; // Out of Index, exut
    // Extract the item
    pci := TPictureCacheItem(FCacheObjectList.Extract(FCacheObjectList.Items[AItemIndex]));
    if Assigned(pci) then // should be always assigned
    try
      for i := 0 to FCacheIDs.Count-1 do
      begin
        if (FCacheIDs.Objects[i] = pci) then // String fit, object also?
        begin
          FCacheIDs.Delete(i); // Delete the entry
          Break;
        end;
      end;
    finally
      pci.Free; // always free the extracted item
    end;
  finally
    LeaveLock;
  end;
end;

procedure TPictureCache.Add(const MapProvider: TMapProvider;
  const TileId: TTileId; const Stream: TMemoryStream);
var
  FileName: String;
  item: TPictureCacheItem;
  lFile: TFileStream;
begin
  FileName := GetFileName(MapProvider, TileId);
  EnterLock;
  try
    item := FCacheItemClass.Create(Stream);
    AddItem(Item, FileName);
  finally
    LeaveLock;
  end;

  if UseDisk then
  begin
    if Assigned(item) then
    begin
      FileName := BasePath + FileName;
      ForceDirectories(ExtractFileDir(FileName));    // <--- to be removed !!!
      lFile := TFileStream.Create(FileName, fmCreate);
      try
        Stream.Position := 0;
        lFile.CopyFrom(Stream, 0);
      finally
        FreeAndNil(lFile);
      end;
    end;
  end;
end;

procedure TPictureCache.CheckCacheSize;
var
  cnt: Integer;
begin
  EnterLock;
  try
    cnt := FCacheObjectList.Count;
    if cnt < FMemMaxItemCount then Exit;
    repeat
      cnt := FCacheObjectList.Count;
      if cnt < (FMemMaxItemCount-MEMCACHE_SWEEP_CNT) then Break;
      DeleteItem(0);
    until False;
  finally
    LeaveLock;
  end;
end;

procedure TPictureCache.CheckCacheSize(Sender: TObject);
begin
  CheckCacheSize();
end;

procedure TPictureCache.GetFromCache(const MapProvider: TMapProvider;
  const TileId: TTileId; out Item: TPictureCacheItem);
var
  FileName: String;
  ndx: integer;
begin
  Item := nil;
  FileName := GetFileName(MapProvider, TileId);
  EnterLock;
  try
    ndx := FCacheIDs.IndexOf(FileName);
    if ndx >= 0 then
    begin
      Item := TPictureCacheItem(FCacheIDs.Objects[ndx]);
      if Assigned(Item) then
      begin
        ndx := FCacheObjectList.IndexOf(Item);
        if ndx > (FMemMaxItemCount div 2) then
        begin
          FCacheObjectList.Extract(Item);
          try
            FCacheObjectList.Add(Item);
          except
            Item.Free;
          end;
        end;
      end;
    end;

  finally
    LeaveLock;
  end;
  if ndx < 0 then
  begin
    if UseDisk then
      LoadFromDisk(FileName, Item);
  end;
end;

{ When TileId is not yet in the cache, the function decreases zoom level and
  returns the TileID of a tile which already is in the cache, and in ARect
  the rectangle coordinates to get an upscaled preview of the originally
  requested tile. The function returns true in this case.
  If the requested tile already is in the cache, or no containing tile is found
  the function returns false indicating that no preview image must be
  generated. }
function TPictureCache.GetPreviewFromCache(const MapProvider: TMapProvider;
  var TileId: TTileId; out ARect: TRect): boolean;
var
  ltid: TTileId;
  xfrac, yfrac: Double;
  lDeltaZoom: Integer;
  w, px, py: Integer;
begin
  Result := false;
  ARect := Rect(0, 0, 0, 0);

  if (TileId.Z < 0) or
     (TileId.X < 0) or
     (TileId.Y < 0) then exit;

  if InCache(MapProvider, TileID) then
    exit;

  if TileId.Z <= 0 then
    exit; // The whole earth as a preview, is simply the earth

  // The "preview" is the part of the containing tile that covers the location of the wanted tile
  // Every decrement of Zoom reduces the tile area by 4 (half of x and y direction)
  // So incrementing Z and dividing X and Y in the Id will lead us to the containing tile
  // The fraction of the division points to the location of the preview
  // e.g 0.5 = right or lower half of the tile, when divided by 2
  ltid := TileId;
  lDeltaZoom := 1;
  w := TileSize.CX;
  repeat
    w := w shr 1;
    dec(ltid.Z);
    lDeltaZoom := lDeltaZoom shl 1;
    xfrac := TileId.X / lDeltaZoom; // xfrac, yfrac contains the tile number
    yfrac := TileId.Y / lDeltaZoom;
    ltid.X := Trunc(xfrac);
    ltid.Y := Trunc(yfrac);
    if InCache(MapProvider, ltid) then
    begin // We found a tile in the cache that contains the preview
      xfrac := xfrac - ltid.X; //xfrac and yfrac calculated for the position in the tile from the cache
      yfrac := yfrac - ltid.Y;
      px := Trunc(xfrac * TileSize.CX); //x and y are the percentage of the tile width
      py := Trunc(yfrac * TileSize.CY);
      ARect := Rect(px, py, px+w, py+w);
      TileID := ltid;
      Result := true;
      exit;
    end;
  until (w <= 1) or (ltid.Z <= 0);
end;

function TPictureCache.InCache(const MapProvider: TMapProvider;
  const TileId: TTileId): Boolean;
var
  FileName: String;
  ndx: integer;
begin
  FileName := GetFileName(MapProvider, TileId);
  EnterLock;
  try
    ndx := FCacheIDs.IndexOf(FileNAme);
  finally
    LeaveLock;
  end;
  if ndx >= 0 then
    Result := True
  else
    Result := DiskCached(FileName);
end;

{ Makes sure that the subfolders needed by the given map provider exist in
  the cache directory. }
procedure TPictureCache.Prepare(const MapProvider: TMapProvider);
var
  prov, dir: String;
  zoom, zoomMin, zoomMax: Integer;
begin
  if (not FLAT_CACHE) or (MapProvider = nil) then
    exit;
  prov := MapProvider2FileName(MapProvider);
  dir := BasePath + prov;
  ForceDirectories(dir);
  MapProvider.GetZoomInfos(zoomMin, zoomMax);
  for zoom := zoomMin to zoomMax do
  begin
    if not DirectoryExists(dir + IntToStr(zoom)) then
      CreateDir(dir + IntToStr(zoom));
  end;
end;

end.

