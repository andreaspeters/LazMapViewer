{ Map Viewer - basic gps object
  (C) 2014 ti_dic@hotmail.com

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvGpsObj;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

uses
  Classes, SysUtils, Graphics, fgl, contnrs, syncobjs,
  mvTypes, mvGeoMath;

const
  NO_ELEVATION = -10000000;
  NO_ELE       = NO_ELEVATION;  // deprecated: use NO_ELEVATION
  NO_DATE      = 0;

type
  TIdArray = Array of integer;
  TGPSObj = class;
  TGPSObjClass = class of TGPSObj;

  TGPSObjList_ = specialize TFPGObjectList<TGPSObj>;

  { TGPSObjList }

  TGPSObjList = class(TGPSObjList_)
  private
    FRef: TObject;
  public
    destructor Destroy; override;
  end;

  { TGPSObjEnumerator }

  TGPSObjEnumerator = class
  private
    function GetCurrent: TGPSObj; virtual; abstract;
  public
    function GetEnumerator: TGPSObjEnumerator;
    function MoveNext: Boolean; virtual; abstract;
    property Current: TGPSObj read GetCurrent;
  end;

  { TGPSObjDrawEvent }

  TGPSObjDrawEvent = procedure(Sender: TObject; AGPSObj: TGPSObj;
    AArea: TRealArea) of object;

  { TGPSObj }

  TGPSObj = class
  private
    //BBoxSet: Boolean;
    FExtraData: TObject;
    FName: String;
    FIdOwner: integer;
    FOnDrawObj: TGPSObjDrawEvent;
    FVisible: Boolean;
    FZOrder: Integer;
    function GetBoundingBox: TRealArea;
    function GetAllObjs: TGPSObjEnumerator; virtual;
    procedure SetExtraData(AValue: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AObj: TGPSObj); virtual;
    procedure GetArea(out Area: TRealArea); virtual; abstract;
    procedure Draw(AView: TObject; Area: TRealArea); virtual; abstract;
    property Name: String read FName write FName;
    property ExtraData: TObject read FExtraData write SetExtraData;
    property IdOwner: Integer read FIdOwner;
    property BoundingBox: TRealArea read GetBoundingBox;
    property ZOrder: Integer read FZOrder;
    property Visible: Boolean read FVisible write FVisible;
    property AllObjs: TGPSObjEnumerator read GetAllObjs;
    property OnDrawObj: TGPSObjDrawEvent read FOnDrawObj write FOnDrawObj;
  end;

  TGPSObjarray = Array of TGPSObj;

  { TGPSPoint }

  TGPSPoint = class(TGPSObj)
  private
    FRealPt: TRealPoint;
    FElevation: Double;
    FDateTime: TDateTime;
    FTextDistance: Integer;
    function GetLat: Double;
    function GetLon: Double;
    procedure SetLat(AValue: Double);
    procedure SetLon(AValue: Double);
  public
    constructor Create(ALon,ALat: double; AElevation: double = NO_ELEVATION;
      ADateTime: TDateTime = NO_DATE);
    constructor CreateFrom(aPt: TRealPoint; AElevation: Double = NO_ELEVATION;
      ADateTime: TDateTime = NO_DATE);

    procedure Assign(AObj: TGPSObj); override;
    procedure GetArea(out Area: TRealArea);override;
    procedure Draw({%H-}AView: TObject; {%H-}Area: TRealArea); override;
    function HasElevation: boolean;
    function HasDateTime: Boolean;
    procedure Init; virtual;
    function DistanceInKmFrom(OtherPt: TGPSPoint; UseElevation: boolean=true): double;
    procedure MoveTo(ALon, ALat: Double; AElevation: double = NO_ELE;
      ADateTime: TDateTime = NO_DATE);

    property Lon: Double read GetLon write SetLon;
    property Lat: Double read GetLat write SetLat;
    property Elevation: double read FElevation write FElevation;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property RealPoint: TRealPoint read FRealPt;
    property TextDistance: Integer read FTextDistance write FTextDistance default 5;
  end;

  TGPSPointList = specialize TFPGObjectList<TGPSPoint>;

  { TGPSPointOfInterest }

  TGPSPointOfInterest = class(TGPSPoint)
  private
    FImageAnchorX: Integer;
    FImageAnchorY: Integer;
    FImageIndex: Integer;
    FTextPositionHor: TTextPositionHor;
    FTextPositionVert: TTextPositionVert;
    FCaptionRect: TRect;
    FImageRect: TRect;
  public
    procedure Draw({%H-}AView: TObject; {%H-}Area: TRealArea); override;
    procedure Init; override;
    property CaptionRect: TRect read FCaptionRect write FCaptionRect;
    property ImageRect: TRect read FImageRect write FImageRect;
    property ImageAnchorX: Integer read FImageAnchorX write FImageAnchorX default 50;  // Percentage!
    property ImageAnchorY: Integer read FImageAnchorY write FImageAnchorY default 100; // Percentage!
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property TextPositionHor: TTextPositionHor read FTextPositionHor write FTextPositionHor default tphCenter;
    property TextPositionVert: TTextPositionVert read FTextPositionVert write FTextPositionVert default tpvBelow;
  end;

  { TGPSPolyLine }

  TGPSPolyLine = class(TGPSObj)
  private
    FPoints: TGPSPointList;
    function GetAllObjs: TGPSObjEnumerator; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetArea(out Area: TRealArea); override;
    property Points: TGPSPointList read FPoints;
  end;

  { TGPSTrack }

  TGPSTrack = class(TGPSPolyLine)
  private
    FConnectColor: TColor;
    FConnectWidth: Double;
    FDateTime: TDateTime;
    FLineWidth: Double;  // Line width in mm
    FLineColor: TColor;
    FOpacity: Single;
    function GetDateTime: TDateTime;
  public
    constructor Create;

    procedure Draw({%H-}AView: TObject; {%H-}Area: TRealArea); override;
    function TrackLengthInKm(UseEle: Boolean=true): double;

    property DateTime: TDateTime read GetDateTime write FDateTime;
    property LineColor: TColor read FLineColor write FLineColor;
    property LineWidth: Double read FLineWidth write FLineWidth;
    property ConnectColor: TColor read FConnectColor write FConnectColor;
    property ConnectWidth: Double read FConnectWidth write FConnectWidth;
    property Opacity: Single read FOpacity write FOpacity;
  end;

  { TGPSArea }

  TGPSArea = class(TGPSPolyLine)
  private
    FFillColor: TColor;
    FLineColor: TColor;
    FLineWidth: Double;
    FOpacity: Single;
  public
    constructor Create;
    procedure Draw({%H-}AView: TObject; {%H-}Area: TRealArea); override;
    property FillColor: TColor read FFillColor write FFillColor;
    property LineColor: TColor read FLineColor write FLineColor;
    property LineWidth: Double read FLineWidth write FLineWidth;
    property Opacity: Single read FOpacity write FOpacity;
  end;

  { TGPSObjectList }

  TModifiedEvent = procedure (Sender: TObject; objs: TGPSObjList;
    Adding: boolean) of object;

  TGPSObjectList = class(TGPSObj)
  private
    Crit: TCriticalSection;
    FPending: TObjectList;
    FRefCount: integer;
    FOnModified: TModifiedEvent;
    FUpdating: integer;
    FItems: TGPSObjList;
    function GetCount: integer;
    function GetItem(AIndex: Integer): TGpsObj;
    function GetAllObjs: TGPSObjEnumerator; override;
  protected
    procedure _Delete(Idx: Integer; var DelLst: TGPSObjList);
    procedure FreePending;
    procedure DecRef;
    procedure CallModified(lst: TGPSObjList; Adding: boolean);
    procedure IdsToObj(const Ids: TIdArray; out objs: TGPSObjArray; AIdOwner: integer);
  public
    constructor Create;
    destructor Destroy; override;
    Procedure ClearAll;
    Procedure Clear(OwnedBy: integer);
    procedure ClearExcept(OwnedBy: integer; const ExceptLst: TIdArray;
      out Notfound: TIdArray);
    procedure GetArea(out Area: TRealArea); override;
    procedure Draw(AView: TObject; Area: TRealArea); override;
    function GetObjectsInArea(const Area: TRealArea; AClass: TGPSObjClass = nil): TGPSObjList;
    function GetPointsInArea(const Area: TRealArea; APointTypes: TMvPointTypes): TGPSObjList;
    function GetIDsArea(const IDs: TIdArray; AIdOwner: integer): TRealArea; deprecated 'Use GetAreaOfIDs';
    function GetAreaOfIDs(const IDs: TIdArray; AIdOwner: Integer): TRealArea;

    function Add(aItem: TGpsObj; AIdOwner: Integer; AZOrder: Integer = 0): Integer;
    function Delete(AItem: TGPSObj): Boolean;
    function IndexOf(AItem: TGPSObj): Integer;
    function ChangeZOrder(AItem: TGPSObj; ANewZOrder: Integer): Integer;
    function MoveToBack(AItem: TGPSObj): Integer;
    function MoveToFront(AItem: TGPSObj): Integer;
    procedure DeleteById(const Ids: Array of integer);
    function FindTrackByID(const id: Integer): TGpsTrack;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Lock;
    procedure UnLock;

    property Count: integer read GetCount;
    property Items[AIndex: Integer]: TGpsObj read GetItem; default;
    property OnModified: TModifiedEvent read FOnModified write FOnModified;
  end;

function HasIntersectArea(const Area1: TRealArea; const Area2: TRealArea): boolean;
function IntersectArea(const Area1: TRealArea; const Area2: TRealArea): TRealArea;
function PtInsideArea(const aPoint: TRealPoint; const Area: TRealArea): boolean;
function AreaInsideArea(const AreaIn: TRealArea; const AreaOut: TRealArea): boolean;
procedure ExtendArea(var AreaToExtend: TRealArea; const Area: TRealArea);
function GetAreaOf(objs: TGPSObjList): TRealArea;
function GoingEast(Lon1, Lon2: Double): Boolean;


implementation

uses
  mvExtraData, mvMapViewer, Math;

type
  { TGPSObjEnumeratorFrom }

  generic TGPSObjEnumeratorFrom<T: Class> = class(TGPSObjEnumerator)
  private
    type
      TItemClass = T;
      TOwnerListClass = specialize TFPGObjectList<T>;
  private
    FList: TOwnerListClass;
    FCurrent: TItemClass;
    FIndex: Integer;
    function GetCurrent: TGPSObj; override;
  public
    constructor Create(AList: TOwnerListClass);
    function MoveNext: Boolean; override;
  end;

function GoingEast(Lon1, Lon2: Double): Boolean;
begin
  // Assume the shortest path (<180 deg)
  Result := ((Lon1 < Lon2) and (Lon2 - Lon1 < 180.0))
    or ((Lon1 > 0) and (Lon2 < 0) and (Lon1 - Lon2 > 180.0));
end;

function hasIntersectArea(const Area1: TRealArea; const Area2: TRealArea): boolean;
begin
  Result := Area1.Intersects(Area2);
end;

function IntersectArea(const Area1: TRealArea; const Area2: TRealArea): TRealArea;
begin
  Result := Area1.Intersection(Area2);
end;

function PtInsideArea(const aPoint: TRealPoint; const Area: TRealArea): boolean;
begin
  Result := Area.ContainsPoint(aPoint);
end;

function AreaInsideArea(const AreaIn: TRealArea; const AreaOut: TRealArea): boolean;
begin
  Result := AreaIn.Equal(AreaIn.Intersection(AreaOut));
end;

procedure ExtendArea(var AreaToExtend: TRealArea; const Area: TRealArea);
begin
  AreaToExtend := AreaToExtend.Union(Area);
end;

function GetAreaOf(objs: TGPSObjList): TRealArea;
var
  i: integer;
begin
  Result.Init(0, 0, 0, 0);
  if Objs.Count > 0 then
  begin
    Result := Objs[0].BoundingBox;
    for i:=1 to pred(Objs.Count) do
      ExtendArea(Result, Objs[i].BoundingBox);
  end;
end;

{ TGPSObjEnumeratorFrom }

function TGPSObjEnumeratorFrom.GetCurrent: TGPSObj;
begin
  Result := FCurrent;
end;

constructor TGPSObjEnumeratorFrom.Create(AList: TOwnerListClass);
begin
  FList := AList;
  FIndex := -1;
end;

function TGPSObjEnumeratorFrom.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
  if Result then
    FCurrent := FList[FIndex];
end;

{ TGPSObjEnumerator }

function TGPSObjEnumerator.GetEnumerator: TGPSObjEnumerator;
begin
  Result := Self;
end;

{ TGPSArea }

constructor TGPSArea.Create;
begin
  inherited;
  FFillColor := clNone;
  FLineColor := clDefault;  // --> use MapView.DefaultTrackColor
  FLineWidth := -1;         // --> use MapView.DefaultTrackWidth
  FOpacity := 1.0;
end;

procedure TGPSArea.Draw(AView: TObject; Area: TRealArea);
begin
  if Assigned(FOnDrawObj)
    then FOnDrawObj(AView, Self, Area)
    else TMapView(AView).DrawArea(Area, Self);
end;

{ TGPSPolyLine }

function TGPSPolyLine.GetAllObjs: TGPSObjEnumerator;
begin
  Result := specialize TGPSObjEnumeratorFrom<TGPSPoint>.Create(FPoints);
end;

constructor TGPSPolyLine.Create;
begin
  inherited;
  FPoints := TGPSPointList.Create(true);
end;

destructor TGPSPolyLine.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPoints);
end;

procedure TGPSPolyLine.GetArea(out Area: TRealArea);
var
  i: integer;
  ptArea: TRealArea;
  pt1, pt2: TRealPoint;
begin
  Area.Init(0, 0, 0, 0);
  if FPoints.Count > 0 then
  begin
    pt1 := FPoints[0].RealPoint;
    Area := FPoints[0].BoundingBox;
    for i:=1 to pred(FPoints.Count) do
    begin
      pt2 := FPoints[I].RealPoint;
      if GoingEast(pt1.Lon, pt2.Lon)
        then ptArea.Init(pt1.Lon, Max(pt1.Lat, pt2.Lat), pt2.Lon, Min(pt1.Lat, pt2.Lat))
        else ptArea.Init(pt2.Lon, Max(pt1.Lat, pt2.Lat), pt1.Lon, Min(pt1.Lat, pt2.Lat));
      ExtendArea(Area, ptArea);
      pt1 := pt2;
    end;
  end;
end;

{ TGPSObjList }

destructor TGPSObjList.Destroy;
begin
  if Assigned(FRef) then
    TGPSObjectList(FRef).DecRef;
  inherited Destroy;
end;

{ TGPSObj }

procedure TGPSObj.Assign(AObj: TGPSObj);
begin
  FName := AObj.Name;
end;

procedure TGPSObj.SetExtraData(AValue: TObject);
begin
  if FExtraData=AValue then Exit;
  if Assigned(FExtraData) then
    FreeAndNil(FExtraData);
  FExtraData := AValue;
end;

constructor TGPSObj.Create;
begin
  FVisible := True;
end;

function TGPSObj.GetBoundingBox: TRealArea;
var
  A: TRealArea;
begin
  GetArea(A);
  Result := A;
end;

function TGPSObj.GetAllObjs: TGPSObjEnumerator;
begin
  Result := Nil;
end;

destructor TGPSObj.Destroy;
begin
  FreeAndNil(FExtraData);
  inherited Destroy;
end;

{ TGPSObjectList }

function TGPSObjectList.GetCount: integer;
begin
  Result := FItems.Count
end;

function TGPSObjectList.GetItem(AIndex: Integer): TGpsObj;
begin
  Result := FItems[AIndex];
end;

procedure TGPSObjectList._Delete(Idx: Integer; var DelLst: TGPSObjList);   // wp: was "out"
var
  Item: TGpsObj;
begin
  Lock;
  try
    if not(Assigned(DelLst)) then
    begin
      DelLst := TGpsObjList.Create(False);
      DelLst.FRef := Self;
      inc(FRefCount);
    end;
    if not Assigned(FPending) then
      FPending := TObjectList.Create(true);
    Item := FItems.Extract(FItems[Idx]);
    FPending.Add(Item);
  finally
    UnLock;
  end;
  DelLst.Add(Item);
end;

procedure TGPSObjectList.FreePending;
begin
  if Assigned(FPending) then
  begin
    Lock;
    try
      FreeAndNil(FPending);
    finally
      UnLock;
    end;
  end;
end;

procedure TGPSObjectList.DecRef;
begin
  FRefCount-=1;
  if FRefCount=0 then
    FreePending;
end;

procedure TGPSObjectList.Lock;
begin
  if Assigned(Crit) then
    Crit.Enter;
end;

procedure TGPSObjectList.UnLock;
begin
  if Assigned(Crit) then
    Crit.Leave;
end;

procedure TGPSObjectList.CallModified(lst: TGPSObjList; Adding: boolean);
begin
  if (FUpdating=0) and Assigned(FOnModified) then
    FOnModified(self, lst, Adding)
end;

procedure TGPSObjectList.IdsToObj(const Ids: TIdArray; out objs: TGPSObjArray;
  AIdOwner: integer);

  function ToSelect(aId: integer): boolean;
  var
    i: integer;
  begin
    result := false;
    for i:=low(Ids) to high(Ids) do
      if Ids[i]=aId then
      begin
        result := true;
        break;
      end;
  end;

var
  i,nb : integer;
begin
  objs := nil;
  SetLength(objs, Length(Ids));
  nb := 0;
  Lock;
  try
    for i:=0 to pred(FItems.Count) do
    begin
      if (AIdOwner = 0) or (AIdOwner = FItems[i].FIdOwner) then
        if Assigned(FItems[i].ExtraData) and FItems[i].ExtraData.InheritsFrom(TDrawingExtraData) then
        begin
          if ToSelect(TDrawingExtraData(FItems[i].ExtraData).Id) then
          begin
            objs[nb] := FItems[i];
            nb+=1;
          end;
        end;
    end;
  finally
    Unlock;
  end;
  SetLength(objs, nb);
end;

function TGPSObjectList.GetAllObjs: TGPSObjEnumerator;
begin
  Result := specialize TGPSObjEnumeratorFrom<TGPSObj>.Create(FItems);
end;

procedure TGPSObjectList.GetArea(out Area: TRealArea);
var
  i: integer;
  ptArea: TRealArea;
begin
  Area.Init(0, 0, 0, 0);
  Lock;
  try
    if Count > 0 then
    begin
      Area := Items[0].BoundingBox;
      for i:=1 to pred(Count) do
      begin
        ptArea := Items[i].BoundingBox;
        ExtendArea(Area, ptArea);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TGPSObjectList.Draw(AView: TObject; Area: TRealArea);
var
  I: Integer;
  obj: TGPSObj;
begin
  Lock;
  try
    for I := 0 to Pred(Count) do
    begin
      obj := Items[I];
      obj.Draw(AView, Area);
    end;
  finally
    Unlock;
  end;
end;

function TGPSObjectList.GetObjectsInArea(const Area: TRealArea;
  AClass: TGPSObjClass = Nil): TGPSObjList;
var
  I: integer;
  ItemArea: TRealArea;
begin
  Result := TGPSObjList.Create(false);
  Lock;
  try
    Inc(FRefCount);
    for I := 0 to Pred(Count) do
    begin
      ItemArea := Items[I].BoundingBox;
      if (not Assigned(AClass) or (Items[I] is AClass)) and
        hasIntersectArea(Area,ItemArea)
      then
        Result.Add(Items[I]);
    end;
    if Result.Count > 0 then
      Result.FRef := Self
    else
      Dec(FRefCount);
  finally
    Unlock;
  end;
end;

function TGPSObjectlist.GetPointsInArea(const Area: TRealArea;
  APointTypes: TMvPointTypes): TGPSObjList;
var
  i, j: Integer;
  itemArea: TRealArea;
  gpsArea: TGPSArea;
  gpsTrack: TGPSTrack;
  obj: TGPSObj;
begin
  Result := TGPSObjList.Create(false);
  Lock;
  try
    inc(FRefCount);
    for i := 0 to Pred(Count) do
    begin
      obj := Items[i];
      itemArea := obj.BoundingBox;
      if Area.Intersects(itemArea) then
      begin
        if (ptGpsPointOfInterest in APointTypes) and (obj is TGPSPointOfInterest) then
          Result.Add(obj);

        if (ptGpsTrackPoint in APointTypes) and (obj is TGPSTrack) then
        begin
          gpsTrack := TGPSTrack(obj);
          for j := 0 to Pred(gpsTrack.Points.Count) do
            if Area.ContainsPoint(gpsTrack.Points[j].RealPoint) then
              Result.Add(gpsTrack.Points[j]);
        end;

        if (ptGpsAreaPoint in APointTypes) and (obj is TGPSArea) then
        begin
          gpsArea := TGPSArea(obj);
          for j := 0 to Pred(gpsArea.Points.Count) do
            if Area.ContainsPoint(gpsArea.Points[j].RealPoint) then
              Result.Add(gpsArea.Points[j]);
        end;
      end;
    end;
    if Result.Count > 0 then
      Result.FRef := Self
    else
      Dec(FRefCount);
  finally
    Unlock;
  end;
end;

constructor TGPSObjectList.Create;
begin
  inherited;
  Crit := TCriticalSection.Create;
  FItems := TGPSObjList.Create(true);
end;

destructor TGPSObjectList.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FItems);
  FreeAndNil(FPending);
  FreeAndNil(Crit);
end;

procedure TGPSObjectList.ClearAll;
begin
  Lock;
  try
    FItems.Clear;
  finally
    Unlock;
  end;
end;

procedure TGPSObjectList.Clear(OwnedBy: integer);
var
  i: integer;
  DelObj: TGPSObjList;
begin
  DelObj := nil;
  Lock;
  try
    for i:=pred(FItems.Count) downto 0 do
      if (OwnedBy = 0) or (FItems[i].FIdOwner = OwnedBy) then
        _Delete(i,DelObj);
  finally
    Unlock;
  end;
  if Assigned(DelObj) then
  begin
    CallModified(DelObj, false);
    DelObj.Free;
  end;
end;

procedure TGPSObjectList.ClearExcept(OwnedBy: integer;
  const ExceptLst: TIdArray; out Notfound: TIdArray);

var
  Found: TIdArray;

  function ToDel(aIt: TGPsObj): boolean;
  var
    i,Id: integer;
  begin
    if (aIt.ExtraData=nil) or not(aIt.ExtraData.InheritsFrom(TDrawingExtraData)) then
      Result := true
    else
    Begin
      Result := true;
      Id := TDrawingExtraData(aIt.ExtraData).Id;
      for i := Low(ExceptLst) to High(ExceptLst) do
       if Id = ExceptLst[i] then
       begin
         Result := false;
         SetLength(Found, Length(Found)+1);
         Found[high(Found)] := Id;
         exit;
       end;
    end;
  end;

var
  i,j: integer;
  IsFound: boolean;
  DelLst: TGPSObjList;
begin
  DelLst := nil;
  SetLength(NotFound{%H-}, 0);
  SetLength(Found, 0);
  Lock;
  try
    for i := pred(FItems.Count) downto 0 do
    begin
      if (FItems[i].FIdOwner = OwnedBy) or (OwnedBy = 0) then
      Begin
         if ToDel(FItems[i]) then
           _Delete(i,DelLst);
      end;
    end;
  finally
    Unlock;
  end;
  for i:=low(ExceptLst) to high(ExceptLst) do
  begin
    IsFound := false;
    for j:=Low(Found) to High(Found) do
      if Found[j] = ExceptLst[i] then
      begin
        IsFound := true;
        break;
      end;
    if not IsFound then
    begin
      SetLength(NotFound, Length(NotFound)+1);
      NotFound[high(NotFound)] := ExceptLst[i];
    end;
  end;
  if Assigned(DelLst) then
  begin
    CallModified(DelLst, false);
    DelLst.Free;
  end;
end;

function TGPSObjectList.GetIdsArea(const Ids: TIdArray; AIdOwner: integer): TRealArea;
begin
  Result := GetAreaOfIDs(IDs, AIDOwner);
end;

function TGPSObjectList.GetAreaOfIDs(const IDs: TIdArray; AIdOwner: Integer): TRealArea;
var
  Objs: TGPSObjarray;
  i: integer;
begin
  Result.Init(0, 0, 0, 0);
  Lock;
  try
    IdsToObj(IDs, Objs, AIdOwner);
    if Length(Objs) > 0 then
    begin
      Result := Objs[0].BoundingBox;
      for i:=succ(Low(Objs)) to High(Objs) do
        ExtendArea(Result, Objs[i].BoundingBox);
    end;
  finally
    Unlock;
  end;
end;

function TGPSObjectList.Add(aItem: TGpsObj; AIdOwner: Integer; AZOrder: Integer
  ): Integer;
var
  mList: TGPSObjList;

  // Returns index _after_ the rightmost less or equal
  function FindLeftmost: Integer;
  var
    L, M: Integer;
  begin
    L := 0; Result := FItems.Count;
    while L < Result do
    begin
      M := (L + Result) div 2;
      if FItems[M].ZOrder > AZOrder
        then Result := M
        else L := Succ(M);
    end;
  end;

begin
  aItem.FIdOwner := AIdOwner;
  aItem.FZOrder := AZOrder;
  Lock;
  try
    Result := FindLeftmost;
    FItems.Insert(Result, aItem);
    mList := TGPSObjList.Create(false);
    mList.Add(aItem);
    inc(FRefCount);
    mList.FRef := Self;
  finally
    Unlock;
  end;
  CallModified(mList, true);
  mList.Free;
end;

function TGPSObjectList.IndexOf(AItem: TGPSObj): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

function TGPSObjectList.Delete(AItem: TGPSObj): Boolean;
var
  DelLst: TGPSObjList;
  I: Integer;
begin
  DelLst := Nil;
  Lock;
  try
    I := FItems.IndexOf(AItem);
    Result := -1 < I;
    if Result then
      _Delete(I, DelLst);
  finally
    Unlock;
  end;
  if Assigned(DelLst) then
  begin
    CallModified(DelLst, False);
    DelLst.Free;
  end;
end;

function TGPSObjectList.ChangeZOrder(AItem: TGPSObj; ANewZOrder: Integer
  ): Integer;
var
  Item: TGPSObj;
begin
  Lock;
  try
    Result := -1;
    Item := FItems.Extract(AItem);
    if Assigned(Item) then
    begin
      Result := Add(AItem, AItem.IdOwner, ANewZOrder);
      CallModified(Nil, True); // Like EndUpdate?
    end;
  finally
    Unlock;
  end;
end;

function TGPSObjectList.MoveToBack(AItem: TGPSObj): Integer;
begin
  Result := FItems.IndexOf(AItem);
  if Result > 0 then
    Result := ChangeZOrder(AItem, Pred(FItems.First.ZOrder))
end;

function TGPSObjectList.MoveToFront(AItem: TGPSObj): Integer;
begin
  Result := FItems.IndexOf(AItem);
  if (Result >= 0) and (Result < Pred(FItems.Count)) then
    Result := ChangeZOrder(AItem, FItems.Last.ZOrder);
end;

procedure TGPSObjectList.DeleteById(const Ids: array of integer);

  function ToDelete(const AId: integer): Boolean;
  var
    i: integer;
  begin
    result := false;
    For i:=Low(Ids) to High(Ids) do
      if Ids[i] = AId then
      begin
        result := true;
        exit;
      end;
  end;

var
  Extr: TDrawingExtraData;
  i: integer;
  DelLst: TGPSObjList;
begin
  DelLst := nil;
  Lock;
  try
    for i:=pred(Count) downto 0 do
    begin
      if Assigned(Items[i].ExtraData) then
      begin
        if Items[i].ExtraData.InheritsFrom(TDrawingExtraData) then
        begin
          Extr := TDrawingExtraData(Items[i].ExtraData);
          if ToDelete(Extr.Id) then
            _Delete(i, DelLst);
        end;
      end;
    end;
  finally
    Unlock;
  end;
  if Assigned(DelLst) then
  begin
    CallModified(DelLst, false);
    DelLst.Free;
  end;
end;

procedure TGPSObjectList.BeginUpdate;
begin
  inc(FUpdating);
end;

procedure TGPSObjectList.EndUpdate;
begin
  if FUpdating > 0 then
  begin
    Dec(FUpdating);
    if FUpdating = 0 then
      CallModified(nil, true);
  end;
end;

function TGPSObjectList.FindTrackByID(const id: Integer): TGpsTrack;
var
  i: Integer;
begin
  for i:=0 to pred(FItems.Count) do
    if (ID = FItems[i].IdOwner) and (FItems[i] is TGpsTrack) then
    begin
      Result := TGpsTrack(FItems[i]);
      exit;
    end;
  Result := nil;
end;


{ TGPSTrack }

function TGPSTrack.GetDateTime: TDateTime;
begin
  if FDateTime = 0 then
  Begin
    if FPoints.Count > 0 then
      FDateTime := FPoints[0].DateTime;
  end;
  Result := FDateTime;
end;

constructor TGPSTrack.Create;
begin
  inherited;
  FLineColor := clDefault;  // --> use MapView.DefaultTrackColor
  FLineWidth := -1;         // --> use MapView.DefaultTrackWidth
  FConnectColor := clNone;  // --> None, clDefault for LineColor
  FConnectWidth := -1;      // --> use LineWidth
  FOpacity := 1.0;
end;


procedure TGPSTrack.Draw(AView: TObject; Area: TRealArea);
begin
  if Assigned(FOnDrawObj)
    then FOnDrawObj(AView, Self, Area)
    else TMapView(AView).DrawTrack(Area, Self);
end;

function TGPSTrack.TrackLengthInKm(UseEle: Boolean): double;
var
  i: integer;
begin
  Result := 0;
  for i:=1 to pred(FPoints.Count) do
    result += FPoints[i].DistanceInKmFrom(FPoints[pred(i)], UseEle);
end;


{ TGPSPoint }

constructor TGPSPoint.Create(ALon, ALat: double; AElevation: double;
  ADateTime: TDateTime);
begin
  inherited Create;
  MoveTo(ALon, ALat, AElevation, ADateTime);
  Init;
end;

constructor TGPSPoint.CreateFrom(aPt: TRealPoint;
  AElevation: Double = NO_ELEVATION; ADateTime: TDateTime = NO_DATE);
begin
  Create(aPt.Lon, aPt.Lat, AElevation, ADateTime);
end;

procedure TGPSPoint.Assign(AObj: TGPSObj);
begin
  if (AObj is TGPSPoint) then
  begin
    inherited Assign(AObj);
    FRealPt := TGPSPoint(AObj).RealPoint;
    FElevation := TGPSPoint(AObj).Elevation;
    FDateTime := TGPSPoint(AObj).DateTime;
  end;
end;

procedure TGPSPoint.Init;
begin
  FTextDistance := 5;
end;

function TGPSPoint.GetLat: Double;
begin
  result := FRealPt.Lat;
end;

function TGPSPoint.GetLon: Double;
begin
  result := FRealPt.Lon;
end;

procedure TGPSPoint.SetLat(AValue: Double);
begin
  FRealPt.Lat := AValue;
end;

procedure TGPSPoint.SetLon(AValue: Double);
begin
  FRealPt.Lon := AValue;
end;

procedure TGPSPoint.GetArea(out Area: TRealArea);
begin
  Area.TopLeft := FRealPt;
  Area.BottomRight := FRealPt;
end;

procedure TGPSPoint.Draw(AView: TObject; Area: TRealArea);
begin
  TMapView(AView).DrawPt(Area, Self);
end;

function TGPSPoint.HasElevation: boolean;
begin
  Result := FElevation <> NO_ELEVATION;
end;

function TGPSPoint.HasDateTime: Boolean;
begin
  Result := FDateTime <> NO_DATE;
end;

function TGPSPoint.DistanceInKmFrom(OtherPt: TGPSPoint;
  UseElevation: Boolean = true): Double;
var
  dElev: Double;
begin
  Result := CalcGeoDistance(Lat, Lon, OtherPt.Lat, OtherPt.Lon, duKilometers, esEllipsoid);
  if UseElevation and HasElevation and OtherPt.HasElevation and (FElevation <> OtherPt.Elevation) then
  begin
    dElev := (FElevation - OtherPt.Elevation) * 0.001;  // Elevation is given in meters
    Result := sqrt(sqr(dElev) + sqr(Result));
  end;
end;

(*
function TGPSPoint.DistanceInKmFrom(OtherPt: TGPSPoint;
  UseElevation: boolean = true): double;
var
  a: double;
  lat1, lat2, lon1, lon2, t1, t2, t3, t4, t5, rad_dist: double;
  DiffEle: Double;
begin
  a := PI / 180;
  lat1 := lat * a;
  lat2 := OtherPt.lat * a;
  lon1 := lon * a;
  lon2 := OtherPt.lon * a;

  t1 := sin(lat1) * sin(lat2);
  t2 := cos(lat1) * cos(lat2);
  t3 := cos(lon1 - lon2);
  t4 := t2 * t3;
  t5 := t1 + t4;
  if t5 >= 1.0 then
    rad_dist := 0.0
  else
  if t5 <= 1.0 then
    rad_dist := pi
  else
    rad_dist := arctan(-t5/sqrt(-t5 * t5 + 1)) + 2 * arctan(1);
  result := (rad_dist * 3437.74677 * 1.1508) * 1.6093470878864446;
  if UseElevation and (FElevation <> OtherPt.FElevation) then
    if (HasElevation) and (OtherPt.HasElevation) then
    begin
      //FElevation is assumed in meters
      DiffEle := (FElevation - OtherPt.Elevation) / 1000;
      Result := sqrt(DiffEle*DiffEle + result*result);
    end;
end;
*)

procedure TGPSPoint.MoveTo(ALon, ALat: Double; AElevation: double = NO_ELEVATION;
  ADateTime: TDateTime = NO_DATE);
begin
  FRealPt.Lon := ALon;
  FRealPt.Lat := ALat;
  FElevation := AElevation;
  FDateTime := ADateTime;
end;


{ TGPSPointOfInterest }

procedure TGPSPointOfInterest.Draw(AView: TObject; Area: TRealArea);
begin
  if Assigned(FOnDrawObj)
    then FOnDrawObj(AView, Self, Area)
    else TMapView(AView).DrawPointOfInterest(Area, Self);
end;

procedure TGPSPointOfInterest.Init; // called by the constructor
begin
  inherited;
  FImageAnchorX := 50;    // These are percentages!
  FImageAnchorY := 100;
  FImageIndex := -1;
  FTextPositionHor := tphCenter;
  FTextPositionVert := tpvBelow;
end;

end.

