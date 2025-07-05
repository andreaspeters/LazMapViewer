unit mvPluginCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Contnrs, Math, LazLoggerBase,
  Graphics, Controls, Dialogs,
  mvMapViewer, mvTypes, mvGpsObj, mvClassRegistration, mvDrawingEngine,
  mvMapProvider, mvCache;

type
  TMvCustomPlugin = class;
  TMvPluginManager = class;

  EMvPluginException = class(EMapViewerException);


  { TMvIndexedComponent }
  
  TMvIndexedComponent = class(TComponent)
  strict protected
    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(AValue: Integer); virtual; abstract;
  public
    procedure ChangeNamePrefix(const AOld, ANew: String; var AFailed: String);
    property Index: Integer read GetIndex write SetIndex;
  end;


  { TMvIndexedComponentList }

  TMvIndexedComponentList = class(TFPList)
  public
    procedure ChangeNamePrefix(const AOld, ANew: String);
  end;


  { TMvCustomPlugin }

  TMvCustomPlugin = class(TMvIndexedComponent)
  private
    FPluginManager: TMvPluginManager;
    FMapView: TMapView;
    FEnabled: Boolean;
    procedure SetPluginManager(AValue: TMvPluginManager);
  protected
    function GetIndex: Integer; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
    procedure SetEnabled(AValue: Boolean); virtual;
    procedure SetIndex(AValue: Integer); override;
    procedure SetMapView(AValue: TMapView); virtual;
    procedure SetParentComponent(AParent: TComponent); override;
    procedure Update; virtual;
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure AfterDrawTile(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      ATileID: TTileID; ARect: TRect; var Handled: Boolean); virtual;
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure CenterMoving(AMapView: TMapView; var NewCenter: TRealPoint;
      var Allow, Handled: Boolean); virtual;
    procedure DrawGPSPoint(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      APoint: TGPSPoint; var Handled: Boolean); virtual;
    procedure DrawMissingTile(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      ATileID: TTileID; ARect: TRect; var Handled: Boolean); virtual;
    procedure TileAfterGetFromCache(AMapView: TMapView; ATileLayer: TGPSTileLayerBase;
      AMapProvider: TMapProvider; ATileID: TTileID; ATileImg: TPictureCacheItem;
      var Handled: Boolean); virtual;
    procedure GPSItemsModified(AMapView: TMapView; ModifiedList: TGPSObjectList;
      ActualObjs: TGPSObjList; Adding: Boolean; var Handled: Boolean); virtual;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure MouseEnter(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure MouseLeave(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); virtual;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer; var Handled: Boolean); virtual;
    procedure MouseWheel(AMapView: TMapView; AShift: TShiftState;
      AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean); virtual;
    procedure Resize(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure ZoomChanging(AMapView: TMapView; NewZoom: Integer; var Allow, Handled: Boolean); virtual;
  protected
    property MapView: TMapView read FMapView write SetMapView;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    property PluginManager: TMvPluginManager read FPluginManager write SetPluginManager;
  published
  end;


  { TMvPlugin }

  TMvPlugin = class(TMvCustomPlugin)
  published
    property Enabled;
    property MapView;
  end;

  { TMvDrawPlugin - common ancestor of all plugins drawing something in the map }

  TMvDrawPlugin = class(TMvPlugin)
  private
    const
      DEFAULT_OPACITY = 0.55;
      DEFAULT_BACKGROUND_COLOR = clWhite;
  private
    FBackgroundColor: TColor;
    FBackgroundOpacity: Single;
    FFont: TFont;
    FPen: TPen;
    function IsOpacityStored: Boolean;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBackgroundOpacity(AValue: Single);
    procedure SetFont(AValue: TFont);
    procedure SetPen(AValue: TPen);
  protected
    procedure Changed(Sender: TObject);
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default DEFAULT_BACKGROUND_COLOR;
    property BackgroundOpacity: Single read FBackgroundOpacity write SetBackgroundOpacity stored IsOpacityStored;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  end;
  

  { TMvMultiMapsPluginData }
const
  DefaultMultiMapsMapViewEnabled = True;

type
  TMvMultiMapsPluginData = class(TObject)
  private
    FMapView : TMapView;
    FEnabled : Boolean;
    FData : array of Byte;
    function GetDataSize : Integer;
  public
    property Enabled : Boolean read FEnabled write FEnabled;
    property DataSize : Integer read GetDataSize;
    procedure SetData(const AData; const ADataSize : Integer);
    function GetData(out AData; const AMaxDataSize : Integer) : Integer;
    function GetDataPtr : Pointer;
    property MapView : TMapView read FMapView write FMapView;
    constructor Create;
  end;


  { TMvMultiMapsPlugin }

  TMvMultiMapsPlugin = class(TMvCustomPlugin)
  private
    FMapDataList : TObjectList;
    function GetMapViewDataIndex(Value : TMapView) : Integer;
    function GetMapViewDataSize(Value : TMapView) : Integer;
    function GetMapViewDataItem(Value : TMapView) : TMvMultiMapsPluginData;
    function GetMapViewDataPtr(Value : TMapView) : Pointer;
    function GetMapViewEnabled(Value : TMapView) : Boolean;
    procedure SetMapViewEnabled(AIndex : TMapView; Value : Boolean);

  protected
    function CreateMultiMapsPluginData : TMvMultiMapsPluginData;virtual;
    property MapDataList : TObjectList read FMapDataList;
  public
    property MapViewEnabled[AIndex : TMapView] : Boolean read GetMapViewEnabled write SetMapViewEnabled;
    property MapViewDataIndex[AIndex : TMapView] : Integer read GetMapViewDataIndex;
    property MapViewDataSize[AIndex : TMapView] : Integer read GetMapViewDataSize;
    property MapViewDataItem[AIndex : TMapView] : TMvMultiMapsPluginData read GetMapViewDataItem;

    property MapViewDataPtr[AIndex : TMapView] : Pointer read GetMapViewDataPtr;
    function GetMapViewData(const AMapView : TMapView; out AData; const AMaxDataSize : Integer) : Integer;
    procedure SetMapViewData(const AMapView : TMapView; const AData; const ADataSize : Integer);
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
  published
    property Enabled;
  end;

  { TMvMultiMapsDrawPlugin }

  TMvMultiMapsDrawPlugin = class(TMvMultiMapsPlugin)
  private
    const
      DEFAULT_OPACITY = 0.55;
      DEFAULT_BACKGROUND_COLOR = clWhite;
  private
    FBackgroundColor: TColor;
    FBackgroundOpacity: Single;
    FFont: TFont;
    FPen: TPen;
    function IsOpacityStored: Boolean;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBackgroundOpacity(AValue: Single);
    procedure SetFont(AValue: TFont);
    procedure SetPen(AValue: TPen);
  protected
    procedure Changed(Sender: TObject);
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default DEFAULT_BACKGROUND_COLOR;
    property BackgroundOpacity: Single read FBackgroundOpacity write SetBackgroundOpacity stored IsOpacityStored;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  end;

  TMvCustomPluginClass = class of TMvCustomPlugin;


  { TMvPluginList }

  TMvPluginList = class(TMvIndexedComponentList)
  private
    FPluginManager: TMvPluginManager;
    function GetItem(AIndex: Integer): TMvCustomPlugin;
    procedure SetItem(AIndex: Integer; AValue: TMvCustomPlugin);
  protected
    procedure CheckPlugin(AItem: Pointer);
    function IsPlugin(AItem: Pointer): Boolean;
  public
    constructor Create(APluginManager: TMvPluginManager);
    function Add(AItem: Pointer): Integer;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Insert({%H-}AIndex: Integer; {%H-}AItem: Pointer);
    procedure Remove(AItem: Pointer);
    property Items[AIndex: Integer]: TMvCustomPlugin read GetItem write SetItem; default;
  end;

  TMouseButtons = set of TMouseButton;
  TMapViewMouseButtons = record
    MapView: TMapView;
    MouseButtons: TMouseButtons;
  end;

  { TMvPluginManager }

  TMvPluginManager = class(TMvCustomPluginManager)
  private
    FPluginList: TMvPluginList;
    FMapList: TFPList;
    FMouseButtonDown: array of TMapViewMouseButtons;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TMvCustomPlugin;
    function GetMapViewCount: Integer;
    function GetMapViews(AIndex: Integer): TMapView;
    function GetMouseButtonDown(AMapView: TMapView): TMouseButtons;
    procedure AddUpdateMouseButton(const AMapView: TMapView; const AMouseButton: TMouseButton; const APressed : Boolean);
    procedure RemoveMouseButton(const AMapView: TMapView);
  protected
    procedure AddMapView(AMapView: TMapView); override;
    function HandlePlugin(APlugin: TMvCustomPlugin; AMapView: TMapView): Boolean;
    procedure InvalidateMapViews;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RemoveMapView(AMapView: TMapView); override;
    procedure SetName(const AValue: TComponentName); override;
  protected
    // Dispatching events to be handled by the plugins
    function AfterDrawObjects(AMapView: TMapView): Boolean; override;
    function AfterDrawTile(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      ATileID: TTileID; ARect: TRect): Boolean; override;
    function AfterPaint(AMapView: TMapView): Boolean; override;
    function BeforeDrawObjects(AMapView: TMapView): Boolean; override;
    function CenterMove(AMapView: TMapView): Boolean; override;
    function CenterMoving(AMapView: TMapView; var NewCenter: TRealPoint;
      var Allow: Boolean): Boolean; override;
    function DrawGPSPoint(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      APoint: TGPSPoint): Boolean; override;
    function DrawMissingTile(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      ATileID: TTileID; ARect: TRect): Boolean; override;
    function TileAfterGetFromCache(AMapView: TMapView; ATileLayer: TGPSTileLayerBase;
      AMapProvider: TMapProvider; ATileID: TTileID; ATileImg: TPictureCacheItem): Boolean; override;
    function GPSItemsModified(AMapView: TMapView; ModifiedList: TGPSObjectList;
      ActualObjs: TGPSObjList; Adding: Boolean): Boolean; override;
    function MouseDown(AMapView: TMapView; AButton: TMouseButton; AShift: TShiftState;
      X, Y: Integer): Boolean; override;
    function MouseEnter(AMapView: TMapView): Boolean; override;
    function MouseLeave(AMapView: TMapView): Boolean; override;
    function MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer): Boolean; override;
    function MouseUp(AMapView: TMapView; AButton: TMouseButton; AShift: TShiftState;
      X, Y: Integer): Boolean; override;
    function MouseWheel(AMapView: TMapView; AShift: TShiftState; AWheelDelta: Integer;
      AMousePos: TPoint): Boolean; override;
    function Resize(AMapView: TMapView): Boolean; override;
    function ZoomChange(AMapView: TMapView): Boolean; override;
    function ZoomChanging(AMapView: TMapView; NewZoom: Integer; var Allow: Boolean): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddPlugin(APlugin: TMvCustomPlugin);
    procedure DeletePlugin(APlugin: TMvCustomPlugin);
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TMvCustomPlugin read GetItems; default;
    property MapViews[AIndex: Integer]: TMapView read GetMapViews;
    property MapViewCount: Integer read GetMapViewCount;
    property MouseButtonDown[AMapView: TMapView]: TMouseButtons read GetMouseButtonDown;
  published
    property PluginList: TMvPluginList read FPluginList;
  end;

procedure RegisterPluginClass(APluginClass: TMvCustomPluginClass; const ACaption: String);

var
  PluginClassRegistry: TMvClassRegistry = nil;


implementation

{ TMvIndexedComponent, borrowed from TAChart }

procedure TMvIndexedComponent.ChangeNamePrefix(const AOld, ANew: String;
  var AFailed: String);
begin
  if AnsiStartsStr(AOld, Name) then
    try
      Name := ANew + Copy(Name, Length(AOld) + 1, Length(Name));
    except on EComponentError do
      AFailed += IfThen(AFailed = '', '', ', ') + Name;
    end;
end;


{ TMvIndexedComponentList, borrowed from TAChart }

procedure TMvIndexedComponentList.ChangeNamePrefix(const AOld, ANew: String);
var
  failed: String;
  i: Integer;
begin
  failed := '';
  for i := 0 to Count - 1 do
    TMvIndexedComponent(Items[i]).ChangeNamePrefix(AOld, ANew, failed);
  if (failed <> '') then
    ShowMessage(Format('Failed to rename components: %s', [failed]));
end;


{ TMvCustomPlugin }

constructor TMvCustomPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TMvPluginManager then
    SetPluginManager(TMvPluginManager(AOwner));
  FEnabled := true;
end;

destructor TMvCustomPlugin.Destroy;
begin
  SetPluginManager(nil);
  inherited;
end;

procedure TMvCustomPlugin.Assign(Source: TPersistent);
begin
  if Source is TMvCustomPlugin then
    FEnabled := TMvCustomPlugin(Source).Enabled;
  inherited Assign(Source);
end;

procedure TMvCustomPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
end;

procedure TMvCustomPlugin.AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
end;

procedure TMvCustomPlugin.AfterDrawTile(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; ATileID: TTileID; ARect: TRect;
  var Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(ADrawingEngine, ATileID, ARect);
end;

procedure TMvCustomPlugin.BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
end;

procedure TMvCustomPlugin.CenterMove(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
end;

procedure TMvCustomPlugin.CenterMoving(AMapView: TMapView; var NewCenter: TRealPoint;
  var Allow, Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(NewCenter, Allow);
end;

procedure TMvCustomPlugin.DrawGPSPoint(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; APoint: TGPSPoint; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(ADrawingEngine, APoint);
end;

procedure TMvCustomPlugin.DrawMissingTile(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; ATileID: TTileID; ARect: TRect;
  var Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(ADrawingEngine, ATileID, ARect);
end;

procedure TMvCustomPlugin.TileAfterGetFromCache(AMapView: TMapView;
  ATileLayer: TGPSTileLayerBase; AMapProvider: TMapProvider; ATileID: TTileID;
  ATileImg: TPictureCacheItem; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(ATileLayer, AMapProvider);
  Unused(ATileID, ATileImg);
end;

function TMvCustomPlugin.GetIndex: Integer;
begin
  if FPluginManager = nil then
    Result := -1
  else
    Result := FPluginManager.PluginList.IndexOf(Self);
end;

function TMvCustomPlugin.GetParentComponent: TComponent;
begin
  Result := FPluginManager;
end;

{ GPSItemsModified is called if one of the GPSList of the MapView changed their content.
  ActualObjs contains the affected objs, but may nil }
procedure TMvCustomPlugin.GPSItemsModified(AMapView: TMapView;
  ModifiedList: TGPSObjectList; ActualObjs: TGPSObjList; Adding: Boolean;
  var Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(ModifiedList, ActualObjs, Adding);
end;

function TMvCustomPlugin.HasParent: Boolean;
begin
  Result := true;
end;

procedure TMvCustomPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  Unused(AMapView, Handled, Button);
  Unused(Shift, X, Y);
end;

procedure TMvCustomPlugin.MouseEnter(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
end;

procedure TMvCustomPlugin.MouseLeave(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
end;

procedure TMvCustomPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(AShift, X, Y);
end;

procedure TMvCustomPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  Unused(AMapView, Handled, Button);
  Unused(Shift, X, Y);
end;

procedure TMvCustomPlugin.MouseWheel(AMapView: TMapView; AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(AShift, AWheelDelta, AMousePos);
end;

procedure TMvCustomPlugin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then
  begin
    if FMapView = AComponent then
      FMapView := nil;
  end;
end;

procedure TMvCustomPlugin.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TMvPluginManager then
    SetPluginManager(TMvPluginManager(Reader.Parent));
end;

procedure TMvCustomPlugin.Resize(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
end;

procedure TMvCustomPlugin.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    Update;
  end;
end;

procedure TMvCustomPlugin.SetIndex(AValue: Integer);
begin
  FPluginManager.PluginList.Move(Index, EnsureRange(AValue, 0, FPluginManager.PluginList.Count-1));
end;

procedure TMvCustomPlugin.SetMapView(AValue: TMapView);
begin
  if FMapView <> AValue then
  begin
    FMapView := AValue;
    if FMapView <> nil then
      FreeNotification(FMapView);
    Update;
  end;
end;

procedure TMvCustomPlugin.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    SetPluginManager(AParent as TMvPluginManager);
end;

procedure TMvCustomPlugin.SetPluginManager(AValue: TMvPluginManager);
begin
  if FPluginManager = AValue then exit;
  if FPluginManager <> nil then
    FPluginManager.PluginList.Remove(Self);
  FPluginManager := AValue;
  if FPluginManager <> nil then
    FPluginManager.PluginList.Add(Self);
end;

procedure TMvCustomPlugin.Update;
begin
  if Assigned(FPluginManager) then
    FPluginManager.InvalidateMapViews;
end;

procedure TMvCustomPlugin.ZoomChange(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView, Handled);
end;

procedure TMvCustomPlugin.ZoomChanging(AMapView: TMapView; NewZoom: Integer;
  var Allow, Handled: Boolean);
begin
  Unused(AMapView, Handled);
  Unused(NewZoom, Allow);
end;


{ TMvDrawPlugin }

constructor TMvDrawPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor := DEFAULT_BACKGROUND_COLOR;
  FBackgroundOpacity := DEFAULT_OPACITY;
  FFont := TFont.Create;
  FFont.OnChange := @Changed;
  FPen := TPen.Create;
  FPen.OnChange := @Changed;
end;

destructor TMvDrawPlugin.Destroy;
begin
  FFont.Free;
  FPen.Free;
  inherited Destroy;
end;

procedure TMvDrawPlugin.Assign(ASource: TPersistent);
begin
  if ASource is TMvDrawPlugin then
  begin
    FBackgroundColor := TMvDrawPlugin(ASource).BackgroundColor;
    FBackgroundOpacity := TMvDrawPlugin(ASource).BackgroundOpacity;
    FFont.Assign(TMvDrawPlugin(ASource).Font);
    FPen.Assign(TMvDrawPlugin(ASource).Pen);
  end;
  inherited;
end;

procedure TMvDrawPlugin.Changed(Sender: TObject);
begin
  Update;
end;

function TMvDrawPlugin.IsOpacityStored: Boolean;
begin
  Result := FBackgroundOpacity <> DEFAULT_OPACITY;
end;

procedure TMvDrawPlugin.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Update;
  end;
end;

procedure TMvDrawPlugin.SetBackgroundOpacity(AValue: Single);
begin
  if FBackgroundOpacity <> AValue then
  begin
    FBackgroundOpacity := AValue;
    Update;
  end;
end;

procedure TMvDrawPlugin.SetFont(AValue: TFont);
begin
  if (AValue = nil) then
    exit;
  if (AValue.Name = FFont.Name) and (AValue.Size = FFont.Size) and
    (AValue.Style = FFont.Style) and (AValue.Color = FFont.Color)
  then
    exit;
  FFont.Assign(AValue);
  Changed(Self);
end;

procedure TMvDrawPlugin.SetPen(AValue: TPen);
begin
  if (AValue = nil) then
    exit;
  if (AValue.Color = FPen.Color) and (AValue.Width = FPen.Width) and
     (AValue.Style = FPen.Style) and (AValue.Mode = FPen.Mode) and
     (AValue.JoinStyle = FPen.JoinStyle) and (AValue.EndCap = FPen.EndCap)
  then
    exit;
  FPen.Assign(AValue);
  Changed(Self);
end;


{ TMvMultiMapsPluginData }

function TMvMultiMapsPluginData.GetDataSize: Integer;
begin
  Result := Length(FData);
end;

procedure TMvMultiMapsPluginData.SetData(const AData; const ADataSize: Integer);
begin
  SetLength(FData,ADataSize);
  if ADataSize > 0 then
    Move(AData,FData[0],ADataSize);
end;

function TMvMultiMapsPluginData.GetData(out AData; const AMaxDataSize: Integer
  ): Integer;
var
  ds : Integer;
begin
  Result := 0;
  ds := DataSize;
  if ds > AMaxDataSize then
    ds := AMaxDataSize;
  if ds <= 0 then Exit;
  Byte(AData) := 0; // Keep the compiler calm
  Move(FData[0],AData,ds);
  Result := ds;
end;

function TMvMultiMapsPluginData.GetDataPtr: Pointer;
begin
  Result := Nil;
  if Length(FData) > 0 then
    Result := @FData[0];
end;

constructor TMvMultiMapsPluginData.Create;
begin
  inherited;
  FEnabled := DefaultMultiMapsMapViewEnabled;
end;

{ TMvMultiMapsPlugin }

function TMvMultiMapsPlugin.GetMapViewDataIndex(Value: TMapView): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FMapDataList.Count-1 do
    if TMvMultiMapsPluginData(FMapDataList.Items[i]).FMapView = Value then
      Exit(i);
end;

function TMvMultiMapsPlugin.GetMapViewDataSize(Value: TMapView): Integer;
var
  ndx : Integer;
begin
  Result := 0;
  ndx := MapViewDataIndex[Value];
  if ndx < 0 then Exit;
  Result := TMvMultiMapsPluginData(FMapDataList.Items[ndx]).DataSize;
end;

function TMvMultiMapsPlugin.GetMapViewDataItem(Value: TMapView
  ): TMvMultiMapsPluginData;
var
  ndx : Integer;
begin
  Result := Nil;
  ndx := MapViewDataIndex[Value];
  if (ndx < 0) then Exit;
  Result := TMvMultiMapsPluginData(FMapDataList.Items[ndx]);
end;

function TMvMultiMapsPlugin.GetMapViewDataPtr(Value: TMapView): Pointer;
var
  di : TMvMultiMapsPluginData;
begin
  Result := Nil;
  di := GetMapViewDataItem(Value);
  if Assigned(di) then
    Result := di.GetDataPtr;
end;

function TMvMultiMapsPlugin.GetMapViewEnabled(Value: TMapView): Boolean;
var
  di : TMvMultiMapsPluginData;
begin
  Result := DefaultMultiMapsMapViewEnabled;
  di := GetMapViewDataItem(Value);
  if not Assigned(di) then Exit;
  Result := di.Enabled;
end;

procedure TMvMultiMapsPlugin.SetMapViewEnabled(AIndex: TMapView; Value: Boolean);
var
  di : TMvMultiMapsPluginData;
begin
  di := GetMapViewDataItem(AIndex);
  if not Assigned(di) then
  begin
    di := CreateMultiMapsPluginData;
    di.MapView := AIndex;
    FMapDataList.Add(di);
  end;
  di.Enabled := Value;
end;

function TMvMultiMapsPlugin.CreateMultiMapsPluginData: TMvMultiMapsPluginData;
begin
  Result := TMvMultiMapsPluginData.Create;
end;

function TMvMultiMapsPlugin.GetMapViewData(const AMapView: TMapView; out AData;
  const AMaxDataSize: Integer): Integer;
var
  ds : Integer;
  di : TMvMultiMapsPluginData;
begin
  Result := 0;
  di := GetMapViewDataItem(AMapView);
  if not Assigned(di) then Exit;
  ds := di.DataSize;
  if ds > AMaxDataSize then
    ds := AMaxDataSize;
  if ds <= 0 then Exit;
  di.GetData(AData,ds);
  Result := ds;
end;

procedure TMvMultiMapsPlugin.SetMapViewData(const AMapView: TMapView;
  const AData; const ADataSize: Integer);
var
  di : TMvMultiMapsPluginData;
begin
  di := GetMapViewDataItem(AMapView);
  if not Assigned(di) then
  begin
    di := CreateMultiMapsPluginData;
    di.MapView := AMapView;
    FMapDataList.Add(di);
  end;
  di.SetData(AData,ADataSize);
end;

constructor TMvMultiMapsPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMapDataList := TObjectList.Create(True);
end;

destructor TMvMultiMapsPlugin.Destroy;
begin
  if Assigned(FMapDataList) then
    FMapDataList.Free;
  inherited Destroy;
end;


{ TMvMultiMapsDrawPlugin }

constructor TMvMultiMapsDrawPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor := DEFAULT_BACKGROUND_COLOR;
  FBackgroundOpacity := DEFAULT_OPACITY;
  FFont := TFont.Create;
  FFont.OnChange := @Changed;
  FPen := TPen.Create;
  FPen.OnChange := @Changed;
end;

destructor TMvMultiMapsDrawPlugin.Destroy;
begin
  FFont.Free;
  FPen.Free;
  inherited Destroy;
end;

procedure TMvMultiMapsDrawPlugin.Assign(ASource: TPersistent);
begin
  if ASource is TMvDrawPlugin then
  begin
    FBackgroundColor := TMvDrawPlugin(ASource).BackgroundColor;
    FBackgroundOpacity := TMvDrawPlugin(ASource).BackgroundOpacity;
    FFont.Assign(TMvDrawPlugin(ASource).Font);
    FPen.Assign(TMvDrawPlugin(ASource).Pen);
  end;
  inherited;
end;

procedure TMvMultiMapsDrawPlugin.Changed(Sender: TObject);
begin
  Update;
end;

function TMvMultiMapsDrawPlugin.IsOpacityStored: Boolean;
begin
  Result := FBackgroundOpacity <> DEFAULT_OPACITY;
end;

procedure TMvMultiMapsDrawPlugin.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Update;
  end;
end;

procedure TMvMultiMapsDrawPlugin.SetBackgroundOpacity(AValue: Single);
begin
  if FBackgroundOpacity <> AValue then
  begin
    FBackgroundOpacity := AValue;
    Update;
  end;
end;

procedure TMvMultiMapsDrawPlugin.SetFont(AValue: TFont);
begin
  if (AValue = nil) then
    exit;
  if (AValue.Name = FFont.Name) and (AValue.Size = FFont.Size) and
    (AValue.Style = FFont.Style) and (AValue.Color = FFont.Color)
  then
    exit;
  FFont.Assign(AValue);
  Changed(Self);
end;

procedure TMvMultiMapsDrawPlugin.SetPen(AValue: TPen);
begin
  if (AValue = nil) then
    exit;
  if (AValue.Color = FPen.Color) and (AValue.Width = FPen.Width) and
     (AValue.Style = FPen.Style) and (AValue.Mode = FPen.Mode) and
     (AValue.JoinStyle = FPen.JoinStyle) and (AValue.EndCap = FPen.EndCap)
  then
    exit;
  FPen.Assign(AValue);
  Changed(Self);
end;


{ TMvPluginList }

constructor TMvPluginList.Create(APluginManager: TMvPluginManager);
begin
  inherited Create;
  FPluginManager := APluginManager;
end;

function TMvPluginList.Add(AItem: Pointer): Integer;
var
  plugin: TMvCustomPlugin;
begin
  CheckPlugin(AItem);
  Result := IndexOf(AItem);
  if (Result < 0) and IsPlugin(AItem) then  // Don't add plugin twice
  begin
    plugin := TMvCustomPlugin(AItem);
    if plugin.PluginManager = FPluginManager then
      Result := inherited Add(AItem)
    else
    begin
      // Plugin already has been assigned to another plugin manager
      // --> we must remove the plugin from the old and add it to the new plugin list.
      plugin.PluginManager := FPluginManager;
      Result := IndexOf(plugin);
    end;
  end;
end;

{ Inheriting from TFPList, TMvPluginlist basically can store any pointer.
  CheckPlugin raises an exception when the user attempts to add an item
  which does not point to a TMvCustomPlugin descendant. }
procedure TMvPluginList.CheckPlugin(AItem: Pointer);
begin
  if not IsPlugin(AItem) then
    raise EMvPluginException.Create('The PluginList can only store descendants of TMvCustomPlugin.');
end;

procedure TMvPluginList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if IsPlugin(Items[i]) then
      TMvCustomPlugin(Items[i]).FPluginManager := nil;
  inherited Clear;
end;

procedure TMvPluginList.Delete(AIndex: Integer);
var
  item: TMvCustomPlugin;
begin
  item := Items[AIndex];
  AIndex := IndexOf(item);
  if AIndex > -1 then
  begin
    if IsPlugin(item) then
      TMvCustomPlugin(item).FPluginManager := nil;
    inherited Delete(AIndex);
  end;
end;

function TMvPluginList.GetItem(AIndex: Integer): TMvCustomPlugin;
begin
  Result := TMvCustomPlugin(inherited Items[AIndex]);
end;

procedure TMvPluginList.Insert(AIndex: Integer; AItem: Pointer);
begin
  raise EMvPluginException.Create('TMvPluginList.Insert not supported.');
end;

{ Checks whether the stored item descends from TMvCustomPlugin. If the user
  calls the inherited Add method items of the incorrect type could make it
  into the list. }
function TMvPluginList.IsPlugin(AItem: Pointer): Boolean;
begin
  Result := TObject(AItem) is TMvCustomPlugin;
end;

procedure TMvPluginList.Remove(AItem: Pointer);
begin
  if IsPlugin(AItem) then
    TMvCustomPlugin(AItem).FPluginManager := nil;
  inherited Remove(AItem);
end;  

procedure TMvPluginList.SetItem(AIndex: Integer; AValue: TMvCustomPlugin);
begin
  inherited Items[AIndex] := AValue;
end;


{ TMvPluginManager }

constructor TMvPluginManager.Create(AOwner: TComponent);
begin
  inherited;
  FPluginList := TMvPluginList.Create(Self);
  FMapList := TFPList.Create;
end;

destructor TMvPluginManager.Destroy;
begin
  while FPluginList.Count > 0 do
    FPluginList[FPluginList.Count-1].Free;
  FPluginList.Free;
  FMapList.Free;
  inherited;
end;

procedure TMvPluginManager.AddMapView(AMapView: TMapView);
var
  idx: Integer;
begin
  idx := FMaplist.IndexOf(AMapView);
  if idx = -1 then
    FMapList.Add(AMapView);
end;

procedure TMvPluginManager.AddPlugin(APlugin: TMvCustomPlugin);
begin
  Assert(APlugin <> nil, 'Plugin argument must not be nil');
  APlugin.PluginManager := self;
end;

function TMvPluginManager.AfterDrawObjects(AMapView: TMapView): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := False;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.AfterDrawObjects(AMapView, Result);
  end;
end;

function TMvPluginManager.AfterDrawTile(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; ATileID: TTileID; ARect: TRect): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count - 1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.AfterDrawTile(AMapView, ADrawingEngine, ATileID, ARect, Result);
  end;
end;

function TMvPluginManager.AfterPaint(AMapView: TMapView): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := False;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.AfterPaint(AMapView, Result);
  end;
end;

function TMvPluginManager.BeforeDrawObjects(AMapView: TMapView): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.BeforeDrawObjects(AMapView, Result);
  end;
end;

function TMvPluginManager.CenterMove(AMapView: TMapView): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.CenterMove(AMapView, Result);
  end;
end;

function TMvPluginManager.CenterMoving(AMapView: TMapView; var NewCenter: TRealPoint;
  var Allow: Boolean): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.CenterMoving(AMapView, NewCenter, Allow, Result);
  end;
end;

procedure TMvPluginManager.DeletePlugin(APlugin: TMvCustomPlugin);
begin
  if APlugin <> nil then
    FPluginList.Remove(APlugin);
end;

function TMvPluginManager.DrawGPSPoint(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; APoint: TGPSPoint): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.DrawGPSPoint(AMapView, ADrawingEngine, APoint, Result);
  end;
end;

function TMvPluginManager.DrawMissingTile(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; ATileID: TTileID; ARect: TRect): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.DrawMissingTile(AMapView, ADrawingEngine, ATileID, ARect, Result);
  end;
end;

function TMvPluginManager.TileAfterGetFromCache(AMapView: TMapView;
  ATileLayer: TGPSTileLayerBase; AMapProvider: TMapProvider; ATileID: TTileID;
  ATileImg: TPictureCacheItem): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.TileAfterGetFromCache(AMapView, ATileLayer,
                                   AMapProvider, ATileID, ATileImg, Result);
  end;
end;

procedure TMvPluginManager.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  plugin: TMvCustomPlugin;
  i: Integer;
begin
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if plugin.Owner = Root then
      Proc(plugin);
  end;
end;

function TMvPluginManager.GetCount: Integer;
begin
  Result := FPluginList.Count;
end;

function TMvPluginManager.GetItems(AIndex: Integer): TMvCustomPlugin;
begin
  Result := TMvCustomPlugin(FPluginList.Items[AIndex]);
end;

function TMvPluginManager.GetMapViewCount: Integer;
begin
  Result := FMapList.Count;
end;

function TMvPluginManager.GetMapViews(AIndex: Integer): TMapView;
begin
  Result := TMapView(FMapList[AIndex]);
end;

function TMvPluginManager.GetMouseButtonDown(AMapView: TMapView): TMouseButtons;
var
  i : Integer;
begin
  Result := [];
  for i := 0 to High(FMouseButtonDown) do
    if FMouseButtonDown[i].MapView = AMapView then
      Exit(FMouseButtonDown[i].MouseButtons)
end;

procedure TMvPluginManager.AddUpdateMouseButton(const AMapView: TMapView;
  const AMouseButton: TMouseButton; const APressed: Boolean);
var
  i : Integer;
  ndx : Integer;
begin
  ndx := -1;
  for i := 0 to High(FMouseButtonDown) do
  begin
    if FMouseButtonDown[i].MapView = AMapView then
    begin
      ndx := i;
      Break;
    end;
  end;
  if ndx < 0 then
  begin
    ndx := Length(FMouseButtonDown);
    SetLength(FMouseButtonDown,ndx+1);
    FMouseButtonDown[ndx].MapView := AMapView;
  end;
  if APressed then
    Include(FMouseButtonDown[ndx].MouseButtons, AMouseButton)
  else
    Exclude(FMouseButtonDown[ndx].MouseButtons, AMouseButton);
end;

procedure TMvPluginManager.RemoveMouseButton(const AMapView: TMapView);
var
  i : Integer;
  ndx : Integer;
begin
  ndx := -1;
  for i := 0 to High(FMouseButtonDown) do
  begin
    if FMouseButtonDown[i].MapView = AMapView then
    begin
      ndx := i;
      Break;
    end;
  end;
  if ndx < 0 then Exit;
  for i := ndx+1 to High(FMouseButtonDown) do
    FMouseButtonDown[i-1] := FMouseButtonDown[i];
  SetLength(FMouseButtonDown,High(FMouseButtonDown));
end;

function TMvPluginManager.GPSItemsModified(AMapView: TMapView;
  ModifiedList: TGPSObjectList; ActualObjs: TGPSObjList; Adding: Boolean): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.GPSItemsModified(AMapView, ModifiedList, ActualObjs, Adding, Result);
  end;
  inherited GPSItemsModified(AMapView, ModifiedList, ActualObjs, Adding);
end;

function TMvPluginManager.HandlePlugin(APlugin: TMvCustomPlugin; AMapView: TMapView): Boolean;
begin
  Result := APlugin.Enabled and ((APlugin.MapView = AMapView) or (APlugin.MapView = nil));
end;

procedure TMvPluginManager.InvalidateMapViews;
var
  i: Integer;
begin
  for i := 0 to FMapList.Count-1 do
    TMapView(FMapList[i]).Invalidate;
end;

function TMvPluginManager.MouseDown(AMapView: TMapView; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  AddUpdateMouseButton(AMapView,AButton,True);
  Result := false;
  for i := FPluginList.Count-1 downto 0 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseDown(AMapView, AButton, AShift, X, Y, Result);
  end;
end;

function TMvPluginManager.MouseEnter(AMapView: TMapView): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := FPluginList.Count-1 downto 0 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseEnter(AMapView, Result);
  end;
end;

function TMvPluginManager.MouseLeave(AMapView: TMapView): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := FPluginList.Count-1 downto 0 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseLeave(AMapView, Result);
  end;
end;

function TMvPluginManager.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X, Y: Integer): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := FPluginList.Count-1 downto 0 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseMove(AMapView, AShift, X, Y, Result);
  end;
end;

function TMvPluginManager.MouseUp(AMapView: TMapView; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  AddUpdateMouseButton(AMapView, AButton, False);
  for i := FPluginList.Count-1 downto 0 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseUp(AMapView, AButton, AShift, X, Y, Result);
  end;
end;

function TMvPluginManager.MouseWheel(AMapView: TMapView; AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := False;
  for i := FPluginList.Count-1 downto 0 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseWheel(AMapView, AShift, AWheelDelta, AMousePos, Result);
  end;
end;

function TMvPluginManager.Resize(AMapView: TMapView): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := False;
  for i := FPluginList.Count-1 downto 0 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.Resize(AMapView, Result);
  end;
end;

procedure TMvPluginManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TMapView then
      RemoveMapView(TMapView(AComponent));
    // Do not handle deleted plugins here -- will crash
  end;
end;

procedure TMvPluginManager.RemoveMapView(AMapView: TMapView);
begin
  RemoveMouseButton(AMapView);
  if not (csDestroying in ComponentState) then
    FMapList.Remove(AMapView);
end;

procedure TMvPluginManager.SetChildOrder(Child: TComponent; Order: Integer);
var
  i: Integer;
begin
  i := FPluginList.IndexOf(Child);
  if i >= 0 then
    FPluginList.Move(i, Order);
end;

procedure TMvPluginManager.SetName(const AValue: TComponentName);
var
  oldName: String;
begin
  if Name = AValue then exit;
  oldName := Name;
  inherited SetName(AValue);
  if csDesigning in ComponentState then
    PluginList.ChangeNamePrefix(oldName, AValue);
end;

function TMvPluginManager.ZoomChange(AMapView: TMapView): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.ZoomChange(AMapView, Result);
  end;
end;

function TMvPluginManager.ZoomChanging(AMapView: TMapView; NewZoom: Integer;
  var Allow: Boolean): Boolean;
var
  i: Integer;
  plugin: TMvCustomPlugin;
begin
  Result := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Items[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.ZoomChanging(AMapView, NewZoom, Allow, Result);
  end;
end;


{ Plugin registration }

procedure RegisterPluginClass(APluginClass: TMvCustomPluginClass; const ACaption: String);
begin
  RegisterClass(APluginClass);
  if PluginClassRegistry.IndexOfClass(APluginClass) < 0 then
    PluginClassRegistry.Add(TMvClassRegistryItem.Create(APluginClass, ACaption));
end;

initialization
  if PluginClassRegistry = nil then
    PluginClassRegistry := TMvClassRegistry.Create;

finalization
  FreeAndNil(PluginClassRegistry);

end.

