unit mvPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  Graphics, Controls, LCLIntf, //LazLoggerBase,
  mvMapViewer, mvDrawingEngine, mvPluginCommon, mvGPSObj, mvTypes;

type
  { TCenterMarkerPlugin - draws a cross in the map center }

  TCenterMarkerPlugin = class(TMvDrawPlugin)
  private
    const
      DEFAULT_MARKER_SIZE = 15;
  private
    FSize: Integer;
    procedure SetSize(AValue: Integer);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var {%H-}Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pen;
    property Size: Integer read FSize write SetSize default DEFAULT_MARKER_SIZE;
  end;


{ TileInfoPlugin - draws a grid corresponding to the tiles used. For debugging. }

  TTileInfoPosition = (tipTopLeft, tipTopCenter, tipTopRight,
    tipCenterLeft, tipCenter, tipCenterRight,
    tipBottomLeft, tipBottomCenter, tipBottomRight);

  TTileInfoPlugin = class(TMvDrawPlugin)
  private
    const
      DEFAULT_TILEINFO_DISTANCE = 2;
      DEFAULT_INFO_MASK = 'x=%0:d y=%1:d';
  private
    FDistance: Integer;
    FPosition: TTileInfoPosition;
    FInfoMask: TCaption;
    function IsInfoMaskStored: Boolean;
    procedure SetDistance(AValue: Integer);
    procedure SetInfoMask(AValue: TCaption);
    procedure SetPosition(AValue: TTileInfoPosition);
  protected
    procedure AfterDrawTile(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      ATileID: TTileID; ARect: TRect; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BackgroundColor;
    property BackgroundOpacity;
    property Distance: Integer read FDistance write SetDistance default DEFAULT_TILEINFO_DISTANCE;
    property Font;
    { Mask for Format(), accepting TileInfo elements in order 0..2.
      Example: InfoMark := 'x=%0:d y=%1:d, zoom=%2:d' }
    property InfoMask: TCaption read FInfoMask write SetInfoMask stored IsInfoMaskStored;
    property Pen;
    property Position: TTileInfoPosition read FPosition write SetPosition default tipCenter;
  end;


  { TLinkedMapsPlugin - all linked maps use the same zoom and center point }

  TLinkedMapsPlugin = class(TMvPlugin)
  private
    FLocked: Integer;
  protected
    procedure CenterMove(AMapView: TMapView; var {%H-}Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var {%H-}Handled: Boolean); override;
  end;


  { TLegalNoticePlugin - displays a clickable copyright text }

  TLegalNoticePosition = (lnpTopLeft, lnpTopRight, lnpBottomLeft, lnpBottomRight);

  TLegalNoticePlugin = class(TMvDrawPlugin)
  private
    const
      DEFAULT_LEGALNOTICE_OPACITY = 0.55;
      DEFAULT_LEGALNOTICE_SPACING = 4;
    type
      TLegalNoticePart = class
        Text: String;
        URL: String;
        Rect: TRect;
        constructor Create(AText, AURL: String);
      end;
      TLegalNoticeParts = class(TFPObjectList);
  private
    FLegalNotice: TCaption;
    FLegalNoticeParts: TLegalNoticeParts;
    FBackgroundOpacity: Single;
    FPosition: TLegalNoticePosition;
    FSpacing: Integer;
    FBackgroundColor: TColor;
    FMouseOverPart: Integer;
    FURLFontColor: TColor;
  private
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBackgroundOpacity(AValue: Single);
    procedure SetLegalNotice(AValue: TCaption);
    procedure SetPosition(AValue: TLegalNoticePosition);
    procedure SetSpacing(AValue: Integer);
  protected
    procedure Changed(Sender: TObject);
    procedure ExtractLegalNoticeParts(AMapView: TMapView);
    function PointInURLPart(APoint: TPoint; out URL: String): Integer;
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; {%H-}Shift: TShiftState; X, Y: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property BackgroundOpacity: Single read FBackgroundOpacity write SetBackgroundOpacity default DEFAULT_LEGALNOTICE_OPACITY;  // 0..1
    property LegalNotice: TCaption read FLegalNotice write SetLegalNotice;
    property Position: TLegalNoticePosition read FPosition write SetPosition default lnpBottomRight;
    property Spacing: Integer read FSpacing write SetSpacing default DEFAULT_LEGALNOTICE_SPACING;
    property URLFontColor: TColor read FURLFontColor write FURLFontColor default clBlue;
    // inherited properties
    property Font;
  end;

  { TDraggableMarkerPlugin }

  TDraggableMarkerPlugin = class;
  TDraggableMarkerCanMoveEvent = function (Sender : TDraggableMarkerPlugin; AMarker : TGPSPoint) : Boolean of object;
  TDraggableMarkerMovedEvent = procedure (Sender : TDraggableMarkerPlugin; AMarker : TGPSPoint; AOrgPosition : TRealPoint) of object;

  { TDraggableMarkerData }
  PDraggableMarkerData = ^TDraggableMarkerData;
  TDraggableMarkerData = record
    FDraggedMarker : TGPSPoint;
    FOrgPosition : TRealPoint;
  end;

  TDraggableMarkerPlugin = class(TMvMultiMapsPlugin)
  private
    const
      DEFAULT_TOLERANCE = 5;
  private
    FDraggableMarkerCanMoveEvent : TDraggableMarkerCanMoveEvent;
    FDraggableMarkerMovedEvent : TDraggableMarkerMovedEvent;
    FDragMouseButton: TMouseButton;
    FTolerance: Integer;
    function GetFirstMarkerAtMousePos(const AMapView: TMapView; const AX, AY : Integer) : TGPSPoint;
    function GetDraggedMarker(AMapView : TMapView) : TGPSPoint;
    function GetOrgPosition(AMapView : TMapView): TRealPoint;
  protected
    procedure MouseDown(AMapView: TMapView; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; {%H-}AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    property DraggedMarker[AMapView : TMapView] : TGPSPoint read GetDraggedMarker;
    property OrgPosition[AMapView : TMapView] : TRealPoint read GetOrgPosition;
  published
    property DraggableMarkerCanMoveEvent : TDraggableMarkerCanMoveEvent read FDraggableMarkerCanMoveEvent write FDraggableMarkerCanMoveEvent;
    property DraggableMarkerMovedEvent : TDraggableMarkerMovedEvent read FDraggableMarkerMovedEvent write FDraggableMarkerMovedEvent;
    property DragMouseButton : TMouseButton read FDragMouseButton write FDragMouseButton default mbLeft;
    property Tolerance: Integer read FTolerance write FTolerance default DEFAULT_TOLERANCE;
  end;

type
  TMvPluginCenterMovingEvent = procedure (Sender: TObject; AMapView: TMapView;
    var ANewCenter: TRealPoint; var Allow, Handled: Boolean) of object;

  TMvPluginDrawGPSPointEvent = procedure (Sender: TObject; AMapView: TMapView;
    ADrawingEngine: TMvCustomDrawingEngine; APoint: TGPSPoint; var Handled: Boolean) of object;

  TMvPluginDrawTileEvent = procedure (Sender: TObject; AMapView: TMapView;
    ADrawingEngine: TMvCustomDrawingEngine; ATileID: TTileID; ARect: TRect;
    var Handled: Boolean) of object;

  TMvPluginDrawMissingTileEvent = TMvPluginDrawTileEvent;       // deprecated

  TMvPluginGPSItemsModifiedEvent = procedure (Sender: TObject; AMapView: TMapView;
    ChangedList: TGPSObjectList; ActualObjs: TGPSObjList; Adding: Boolean;
    var Handled: Boolean) of Object;

  TMvPluginNotifyEvent = procedure (Sender : TObject; AMapView: TMapView;
    var Handled: Boolean) of Object;

  TMvPluginMouseEvent = procedure (Sender : TObject; AMapView: TMapView; Button: TMouseButton;
    AShift: TShiftState; X, Y: Integer; var Handled: Boolean) of Object;

  TMvPluginMouseMoveEvent = procedure (Sender : TObject; AMapView: TMapView;
    AShift: TShiftState; X,Y: Integer; var Handled: Boolean) of Object;

  TMvPluginMouseWheelEvent = procedure (Sender: TObject; AMapView: TMapView;
    AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean) of object;

  TMvPluginZoomChangingEvent = procedure (Sender: TObject; AMapView: TMapView;
    NewZoom: Integer; var Allow, Handled: Boolean) of object;

  { TUserDefinedPlugin }

  TUserDefinedPlugin = class(TMvCustomPlugin)
  private
    FAfterDrawObjectsEvent : TMvPluginNotifyEvent;
    FAfterDrawTileEvent: TMvPluginDrawTileEvent;
    FAfterPaintEvent : TMvPluginNotifyEvent;
    FBeforeDrawObjectsEvent : TMvPluginNotifyEvent;
    FCenterMoveEvent : TMvPluginNotifyEvent;
    FCenterMovingEvent: TMvPluginCenterMovingEvent;
    FDrawGPSPointEvent: TMvPluginDrawGPSPointEvent;
    FDrawMissingTileEvent: TMvPluginDrawTileEvent;
    FGPSItemsModifiedEvent : TMvPluginGPSItemsModifiedEvent;
    FMouseDownEvent : TMvPluginMouseEvent;
    FMouseEnterEvent : TMvPluginNotifyEvent;
    FMouseLeaveEvent : TMvPluginNotifyEvent;
    FMouseMoveEvent : TMvPluginMouseMoveEvent;
    FMouseUpEvent : TMvPluginMouseEvent;
    FMouseWheelEvent : TMvPluginMouseWheelEvent;
    FResizeEvent: TMvPluginNotifyEvent;
    FZoomChangeEvent : TMvPluginNotifyEvent;
    FZoomChangingEvent : TMvPluginZoomChangingEvent;
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure AfterDrawTile(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      ATileID: TTileID; ARect: TRect; var Handled: Boolean); override;
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); override;
    procedure CenterMoving(AMapView: TMapView; var NewCenter: TRealPoint;
      var Allow, Handled: Boolean); override;
    procedure DrawGPSPoint(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      APoint: TGPSPoint; var Handled: Boolean); override;
    procedure DrawMissingTile(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      ATileID: TTileID; ARect: TRect; var Handled: Boolean); override;
    procedure GPSItemsModified(AMapView: TMapView; ChangedList: TGPSObjectList;
      ActualObjs: TGPSObjList; Adding: Boolean; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseEnter(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseLeave(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseWheel(AMapView: TMapView; AShift: TShiftState;
      AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean); override;
    procedure Resize(AMapView: TMapView; var Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); override;
    procedure ZoomChanging(AMapView: TMapView; NewZoom: Integer; var Allow, Handled: Boolean); override;
  public
  published
    property OnAfterDrawObjects : TMvPluginNotifyEvent read FAfterDrawObjectsEvent write FAfterDrawObjectsEvent;
    property OnAfterDrawTile: TMvPluginDrawTileEvent read FAfterDrawTileEvent write FAfterDrawTileEvent;
    property OnAfterPaint : TMvPluginNotifyEvent read FAfterPaintEvent write FAfterPaintEvent;
    property OnBeforeDrawObjects : TMvPluginNotifyEvent read FBeforeDrawObjectsEvent write FBeforeDrawObjectsEvent;
    property OnCenterMove : TMvPluginNotifyEvent read FCenterMoveEvent write FCenterMoveEvent;
    property OnCenterMoving: TMvPluginCenterMovingEvent read FCenterMovingEvent write FCenterMovingEvent;
    property OnDrawGPSPoint: TMvPluginDrawGPSPointEvent read FDrawGPSPointEvent write FDrawGPSPointEvent;
    property OnGPSItemsModified : TMvPluginGPSItemsModifiedEvent read FGPSItemsModifiedEvent write FGPSItemsModifiedEvent;
    property OnMouseDown : TMvPluginMouseEvent read FMouseDownEvent write FMouseDownEvent;
    property OnMouseEnter : TMvPluginNotifyEvent read FMouseEnterEvent write FMouseEnterEvent;
    property OnMouseLeave : TMvPluginNotifyEvent read FMouseLeaveEvent write FMouseLeaveEvent;
    property OnMouseMove : TMvPluginMouseMoveEvent read FMouseMoveEvent write FMouseMoveEvent;
    property OnMouseUp : TMvPluginMouseEvent read FMouseUpEvent write FMouseUpEvent;
    property OnMouseWheel : TMvPluginMouseWheelEvent read FMouseWheelEvent write FMouseWheelEvent;
    property OnZoomChange : TMvPluginNotifyEvent read FZoomChangeEvent write FZoomChangeEvent;
    property OnZoomChanging: TMvPluginZoomChangingEvent read FZoomChangingEvent write FZoomChangingEvent;

    // inherited
    property Enabled;
    property MapView;
  end;

implementation

uses
  Types;

{ TCenterMargerPlugin }

constructor TCenterMarkerPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FSize := DEFAULT_MARKER_SIZE;
end;

procedure TCenterMarkerPlugin.Assign(Source: TPersistent);
begin
  if Source is TCenterMarkerPlugin then
    FSize := TCenterMarkerPlugin(Source).Size;
  inherited;
end;

procedure TCenterMarkerPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
var
  C: TPoint;
begin
  C := Point(AMapView.ClientWidth div 2, AMapView.ClientHeight div 2);
  AMapView.DrawingEngine.PenColor := Pen.Color;
  AMapView.DrawingEngine.PenStyle := Pen.Style;
  AMapView.DrawingEngine.PenWidth := Pen.Width;
  AMapView.DrawingEngine.Opacity := 1.0;
  AMapView.DrawingEngine.Line(C.X, C.Y - FSize, C.X, C.Y + FSize);
  AMapView.DrawingEngine.Line(C.X - FSize, C.Y, C.X + FSize, C.Y);
end;

procedure TCenterMarkerPlugin.SetSize(AValue: Integer);
begin
  if FSize <> AValue then
  begin
    FSize := AValue;
    Update;
  end;
end;


{ TTileInfoPlugin }

constructor TTileInfoPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FDistance := DEFAULT_TILEINFO_DISTANCE;
  FInfoMask := DEFAULT_INFO_MASK;
  FPosition := tipCenter;
end;

procedure TTileInfoPlugin.AfterDrawTile(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; ATileID: TTileID; ARect: TRect;
  var Handled: Boolean);
var
  mask, txt: String;
  sz: TSize;
  txtRect: TRect;
  pw: Integer;
  savedPen: TMvPen;
  savedFont: TMvFont;
  savedOpacity: Single;
begin
  savedPen := ADrawingEngine.GetPen;
  savedFont := ADrawingEngine.GetFont;
  savedOpacity := ADrawingEngine.Opacity;

  ADrawingEngine.SetPen(Pen.Style, Pen.Width, Pen.Color);
  ADrawingEngine.Line(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom);
  ADrawingEngine.Line(ARect.Left, ARect.Top, ARect.Right, ARect.Top);
  ADrawingEngine.Line(ARect.Right, ARect.Top, ARect.Right, ARect.Bottom);
  ADrawingEngine.Line(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom);

  if FInfoMask = '' then
    mask := DEFAULT_INFO_MASK
  else
    mask := FInfoMask;
  txt := Format(mask, [ATileID.X, ATileID.Y, ATileID.Z]);

  ADrawingEngine.SetFont(Font.Name, Font.Size, Font.Style, Font.Color);
  sz := ADrawingEngine.TextExtent(txt);
  txtRect := Rect(0, 0, sz.CX, sz.CY);
  pw := Pen.Width div 2 + FDistance;
  case FPosition of
    tipTopLeft:
      OffsetRect(txtRect, ARect.Left + pw, ARect.Top + pw);
    tipTopCenter:
      OffsetRect(txtRect, (ARect.Left + ARect.Right - txtRect.Right) div 2, ARect.Top + pw);
    tipTopRight:
      OffsetRect(txtRect, ARect.Right - txtRect.Right - pw, ARect.Top + pw);
    tipCenterLeft:
      OffsetRect(txtRect, ARect.Left + pw, (ARect.Top + ARect.Bottom - txtRect.Bottom) div 2);
    tipCenter:
      OffsetRect(txtRect, (ARect.Left + ARect.Right - txtRect.Right) div 2, (ARect.Top + ARect.Bottom - txtRect.Bottom) div 2);
    tipCenterRight:
      OffsetRect(txtRect, ARect.Right - txtRect.Right - pw, (ARect.Top + ARect.Bottom - txtRect.Bottom) div 2);
    tipBottomLeft:
      OffsetRect(txtRect, ARect.Left + pw, ARect.Bottom - txtRect.Bottom - pw);
    tipBottomCenter:
      OffsetRect(txtRect, (ARect.Left + ARect.Right - txtRect.Right) div 2, ARect.Bottom - txtRect.Bottom - pw);
    tipBottomRight:
      OffsetRect(txtRect, ARect.Right - txtRect.Right - pw, ARect.Bottom - txtRect.Bottom - pw);
  end;

  if BackgroundColor <> clNone then
  begin
    ADrawingEngine.Opacity := BackgroundOpacity;
    ADrawingEngine.BrushStyle := bsSolid;
    ADrawingEngine.BrushColor := ColorToRGB(BackgroundColor);
    with txtRect do
      AMapView.DrawingEngine.FillRect(Left, Top, Right, Bottom);
  end;

  ADrawingEngine.BrushStyle := bsClear;
  ADrawingEngine.TextOut(txtRect.Left, txtRect.Top, txt);

  ADrawingEngine.Opacity := savedOpacity;
  ADrawingEngine.SetPen(savedPen);
  ADrawingEngine.SetFont(savedFont);
end;

function TTileInfoPlugin.isInfoMaskStored: Boolean;
begin
  Result := FInfoMask <> DEFAULT_INFO_MASK;
end;

procedure TTileInfoPlugin.SetDistance(AValue: Integer);
begin
  if FDistance <> AValue then
  begin
    FDistance := AValue;
    Update;
  end;
end;

procedure TTileInfoPlugin.SetInfoMask(AValue: TCaption);
begin
  if FInfoMask <> AValue then
  begin
    FInfoMask := AValue;
    Update;
  end;
end;

procedure TTileInfoPlugin.SetPosition(AValue: TTileInfoPosition);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    Update;
  end;
end;


{ TLinkedMapsPlugin }

procedure TLinkedMapsPlugin.CenterMove(AMapView: TMapView; var Handled: Boolean);
var
  i: Integer;
  map: TMapView;
begin
  if FLocked > 0 then
    exit;
  inc(FLocked);
  try
    for i := 0 to PluginManager.MapViewCount-1 do
    begin
      map := PluginManager.MapViews[i];
      if AMapView <> map then
        map.Center := AMapView.Center;
    end;
  finally
    dec(FLocked);
  end;
end;

procedure TLinkedMapsPlugin.ZoomChange(AMapView: TMapView; var Handled: Boolean);
var
  i: Integer;
  map: TMapView;
  zoomToCrs: Boolean;
begin
  if FLocked > 0 then
    exit;
  inc(FLocked);
  try
    for i := 0 to PluginManager.MapViewCount-1 do
    begin
      map := PluginManager.MapViews[i];
      if AMapView <> map then
      begin
        zoomToCrs := map.ZoomToCursor;
        try
          map.ZoomToCursor := false;
          map.Zoom := AMapView.Zoom;
        finally
          map.ZoomToCursor := zoomToCrs;
        end;
      end;
    end;
  finally
    dec(FLocked);
  end;
end;


{ TLegalNoticePlugin }

constructor TLegalNoticePlugin.TLegalNoticePart.Create(AText, AURL: String);
begin
  inherited Create;
  Text := AText;
  URL := AURL;
end;

constructor TLegalNoticePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor := clNone;
  FBackgroundOpacity := DEFAULT_LEGALNOTICE_OPACITY;
  FURLFontColor := clBlue;
  FPosition := lnpBottomRight;
  FSpacing := DEFAULT_LEGALNOTICE_SPACING;
  FLegalNoticeParts := TLegalNoticeParts.Create;
end;

destructor TLegalNoticePlugin.Destroy;
begin
  FLegalNoticeParts.Free;
  inherited;
end;

procedure TLegalNoticePlugin.Assign(Source: TPersistent);
begin
  if Source is TLegalNoticePlugin then
  begin
    FBackgroundColor := TLegalNoticePlugin(Source).BackgroundColor;
    FBackgroundOpacity := TLegalNoticePlugin(Source).BackgroundOpacity;
    FLegalNotice := TLegalNoticePlugin(Source).LegalNotice;
    FPosition := TLegalNoticePlugin(Source).Position;
    FSpacing := TLegalNoticePlugin(Source).Spacing;
    FURLFontColor := TLegalNoticePlugin(Source).URLFontColor;
  end;
  inherited;
end;

procedure TLegalNoticePlugin.AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
var
  i: Integer;
  lBounds: TRect;
  lSavedFont: TMvFont;
  lSavedOpacity: Single;
  part: TLegalNoticePart;
begin
  if not Assigned(AMapView) then Exit;
  Handled := True;

  lSavedFont := AMapView.DrawingEngine.GetFont;
  lSavedOpacity := AMapView.DrawingEngine.Opacity;
  try
    ExtractLegalNoticeParts(AMapView);

    lBounds := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
    for i := 0 to FLegalNoticeParts.Count-1 do
    begin
      part := TLegalNoticePart(FLegalNoticeParts[i]);
      if part.Rect.Left < lBounds.Left then lBounds.Left := part.Rect.Left;
      if part.Rect.Top < lBounds.Top then lBounds.Top := part.Rect.Top;
      if part.Rect.Right > lBounds.Right then lBounds.Right := part.Rect.Right;
      if part.Rect.Bottom > lBounds.Bottom then lBounds.Bottom := part.Rect.Bottom;
    end;

    // Draw the common (semi-transparent) background of all parts
    if FBackgroundColor <> clNone then
    begin
      AMapView.DrawingEngine.Opacity := FBackgroundOpacity;
      AMapView.DrawingEngine.BrushStyle := bsSolid;
      AMapView.DrawingEngine.BrushColor := ColorToRGB(FBackgroundColor);
      with lBounds do
          AMapView.DrawingEngine.FillRect(Left, Top, Right, Bottom);
    end;
    AMapView.DrawingEngine.BrushStyle := bsClear;

    // Draw the part texts
    for i := 0 to FLegalNoticeParts.Count-1 do
    begin
      part := TLegalNoticePart(FLegalNoticeParts[i]);
      if part.URL <> '' then
      begin
        if i = FMouseOverPart then
          AMapView.DrawingEngine.SetFont(Font.Name, Font.Size, Font.Style + [fsUnderline], FURLFontColor)
        else
          AMapView.DrawingEngine.SetFont(Font.Name, Font.Size, Font.Style, FURLFontColor)
      end
      else
        AMapView.DrawingEngine.SetFont(Font.Name, Font.Size, Font.Style, Font.Color);
      AMapView.DrawingEngine.TextOut(part.Rect.Left, part.Rect.Top, part.Text);
    end;
  finally
    AMapView.DrawingEngine.Opacity := lSavedOpacity;
    AMapView.DrawingEngine.SetFont(lSavedFont);
  end;
end;

{ LegalNotice can contain text and embedded URLs with text following the
  wikipedia mark-down.

  Line breaks allowed as #13, #10, #13#10, or '\n'

  Example:
    'Map data from [https://openstreetmap.org/copyright OpenStreetMap and contributors]'
  displayed as "Map data from OpenStreamMap and contributors"
  Embedded URL (https://openstreetmap.org/copyright) is assigned to text
  "OpenStreetMap and contributors" }
procedure TLegalNoticePlugin.ExtractLegalNoticeParts(AMapView: TMapView);
var
  P: PAnsiChar;
  partType: (ptText, ptURL, ptURLText, prLineBreak);
  txt: String;
  url: String;
  part: TLegalNoticePart;
  savedFont: TMvFont;
  R: TRect;
  lineWidths: array of integer = nil;
  sz: TSize;
  i, line, dx, dy, nLines: Integer;
begin
  FLegalNoticeParts.Clear;
  if FLegalNotice = '' then
    exit;

  P := PChar(FLegalNotice);
  partType := ptText;
  txt := '';
  url := '';
  nLines := 1;

  while true do
  begin
    case P^ of
      #0 : break;
      '[': begin
             // entering a URL part
             if partType = ptText then
             begin
               if (txt <> '') or (url <> '') then
               begin
                 // Store away previously found txt and url
                 part := TLegalNoticePart.Create(txt, url);
                 FLegalNoticeParts.Add(part);
               end;
               partType := ptURL;  // the next part will be in URL
               txt := '';
               url := '';
             end;
           end;
      ' ': if partType = ptURL then  // in URL
           begin
             partType := ptURLText;  // next part will be the text assigned to the URL
             txt := '';
           end else
             txt := txt + P^;
      ']': if partType = ptURLText then
           begin
             if (txt <> '') and (url <> '') then
             begin
               // Store away url and its text
               part := TLegalNoticePart.Create(txt, url);
               FLegalNoticeParts.Add(part);
             end;
             partType := ptText;  // next part will be normal text again
             txt := '';
             url := '';
           end;
      #13,
      #10: begin
             if P^ = #13 then
             begin
               inc(P);
               if P^ <> #10 then dec(P);
             end;
             // Store away previously found text and url
             FLegalNoticeParts.Add(TLegalNoticePart.Create(txt, url));
             // Store #13 as indicator of a line break. Keep url.
             FLegalNoticeParts.Add(TLegalNoticePart.Create(#13, ''));
             txt := '';
             inc(nLines);
           end;
      '\': begin
             inc(P);
             if (P^ in ['n', 'N']) then
             begin
               // Store away previously found text and url
               FLegalNoticeParts.Add(TLegalNoticePart.Create(txt, url));
               // Store #13 as indicator of a line break. Keep url.
               FLegalNoticeParts.Add(TLegalNoticePart.Create(#13, ''));
               txt := '';
               inc(nLines);
             end else
             begin
               dec(P);
               txt := txt + P^;
             end;
           end;
      else
        if partType = ptURL then
          url := url + P^
        else
          txt := txt + P^;
    end;
    inc(P);
  end;

  if (partType = ptText) and (txt <> '') then
  begin
    part := TLegalNoticePart.Create(txt, '');
    FLegalNoticeParts.Add(part);
  end;

  // Measure pixel size of parts
  savedFont := AMapView.DrawingEngine.GetFont;
  try
    AMapView.DrawingEngine.SetFont(Font.Name, Font.Size, Font.Style, Font.Color);
    R := Rect(0, 0, 0, 0);
    SetLength(lineWidths, nLines);
    line := 0;
    lineWidths[line] := 0;
    for i := 0 to FLegalNoticeParts.Count-1 do
    begin
      part := TLegalNoticePart(FLegalNoticeParts[i]);
      sz := AMapView.DrawingEngine.TextExtent(part.Text);
      R := Rect(R.Right, R.Top, R.Right + sz.CX, R.Top + sz.CY);
      if R.Right > lineWidths[line] then
        lineWidths[line] := R.Right;
      if part.Text = #13 then  // line break
      begin
        R := Rect(0, R.Bottom - sz.CY div 2, 0, R.Bottom);
        inc(line);
        lineWidths[line] := 0;
      end;
      part.Rect := R;
    end;
  finally
    AMapView.DrawingEngine.SetFont(savedFont);
  end;

  // Calculate left/top corner of total text
  case FPosition of
    lnpTopLeft, lnpBottomLeft:
      dx := FSpacing;
    lnpTopRight, lnpBottomRight:
      dx := AMapView.Width - FSpacing - lineWidths[0];
  end;
  case FPosition of
    lnpTopLeft, lnpTopRight:
      dy := FSpacing;
    lnpBottomLeft, lnpBottomRight:
      dy := AMapView.Height - R.Bottom - FSpacing;
  end;

  // Move text rectangles to correct position
  line := 0;
  for i := 0 to FLegalNoticeParts.Count-1 do
  begin
    part := TLegalNoticePart(FLegalNoticeParts[i]);
    if part.Text = #13 then
    begin
      inc(line);
      if FPosition in [lnpTopRight, lnpBottomRight] then
        dx := AMapView.Width - FSpacing - lineWidths[line];
    end;
    OffsetRect(part.Rect, dx, dy);
  end;
end;

procedure TLegalNoticePlugin.Changed(Sender: TObject);
begin
  Update;
end;

procedure TLegalNoticePlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  url: String;
begin
  Unused(AMapView);
  // The button down event is consumed by a different plugin, so do nothing here
  if Handled then Exit;
  if PointInURLPart(Point(X, Y), url) <> -1 then
  begin
    // The button down event is consumed by this plugin
    OpenURL(url);
    Handled := True;
  end;
end;

procedure TLegalNoticePlugin.MouseMove(AMapView: TMapView; Shift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
var
  url: String;
begin
  ExtractLegalNoticeParts(AMapView);

  if not (AMapView.Engine.InDrag) then
  begin
    FMouseOverPart := PointInURLPart(Point(X, Y), url);
    if (FMouseOverPart <> -1) then
    begin
      if url <> '' then
      begin
        AMapView.Cursor := crHandPoint;
        Handled := true;
      end;
    end else
      if not Handled then
        AMapView.Cursor := crDefault;
    Update;
  end;
end;

function TLegalNoticePlugin.PointInURLPart(APoint: TPoint; out URL: String): Integer;
var
  part: TLegalNoticePart;
begin
  for Result := 0 to FLegalNoticeParts.Count-1 do
  begin
    part := TLegalNoticePart(FLegalNoticeParts[Result]);
    if PtInRect(part.Rect, APoint) and (part.URL <> '') then
    begin
      uRL := part.URL;
      exit;
    end;
  end;
  URL := '';
  Result := -1;
end;


procedure TLegalNoticePlugin.SetPosition(AValue: TLegalNoticePosition);
begin
  if FPosition = AValue then Exit;
  FPosition := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetLegalNotice(AValue: TCaption);
begin
  if FLegalNotice = AValue then Exit;
  FLegalNotice := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then Exit;
  FBackgroundColor := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetBackgroundOpacity(AValue: Single);
begin
  if FBackgroundOpacity = AValue then Exit;
  FBackgroundOpacity := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetSpacing(AValue: Integer);
begin
  if FSpacing = AValue then Exit;
  FSpacing := AValue;
  Update;
end;


{ TDraggableMarkerPlugin }

constructor TDraggableMarkerPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FTolerance := DEFAULT_TOLERANCE;
end;

procedure TDraggableMarkerPlugin.Assign(Source: TPersistent);
begin
  if Source is TDraggableMarkerPlugin then
  begin
    FDraggableMarkerCanMoveEvent := TDraggableMarkerPlugin(Source).DraggableMarkerCanMoveEvent;
    FDraggableMarkerMovedEvent := TDraggableMarkerPlugin(Source).DraggableMarkerMovedEvent;
    FDragMouseButton := TDraggableMarkerPlugin(Source).DragMouseButton;
    FTolerance := TDraggableMarkerPlugin(Source).Tolerance;
  end;
  inherited;
end;

function TDraggableMarkerPlugin.GetFirstMarkerAtMousePos(const AMapView: TMapView;
  const AX, AY: Integer): TGPSPoint;

  function FindInList(AGpsList: TGpsObjList): TGpsPoint;
  var
    i: Integer;
  begin
    if Assigned(AGpsList) then
      for i := AGpsList.Count-1 downto 0 do
      begin
        if (AGpsList[i] is TGpsPoint) then
        begin
          Result := TGpsPoint(AGpsList[i]);
          if (not Assigned(FDraggableMarkerCanMoveEvent)) or
             DraggableMarkerCanMoveEvent(Self, Result)
          then
            exit;
        end;
      end;
    Result := nil;
  end;

var
  aArea : TRealArea;
  gpsList: TGpsObjList;
  layer: TMapLayer;
  i : Integer;
begin
  Result := Nil;
  aArea.TopLeft := AMapView.ScreenToLatLon(Point(AX - FTolerance, AY - FTolerance));
  aArea.BottomRight := AMapView.ScreenToLatLon(Point(AX + FTolerance, AY + FTolerance));

  // Search in GPSItems for all gps-type-of-points
  gpsList := AMapView.GPSItems.GetObjectsInArea(aArea);
  try
    Result := FindInList(gpsList);
    if Result <> nil then
      exit;
  finally
    gpsList.Free;
  end;

  // Search in all layers for all map-type points
  for i := AMapView.Layers.Count-1 downto 0 do
  begin
    layer := AMapView.Layers[i];
    gpsList := layer.GetObjectsInArea(aArea);
    try
      Result := FindInList(gpsList);
      if Result <> nil then
        exit;
    finally
      gpsList.Free;
    end;
  end;
end;

function TDraggableMarkerPlugin.GetDraggedMarker(AMapView: TMapView): TGPSPoint;
var
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  Result := Nil;
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) then
    Result := lDraggableMarkerData.FDraggedMarker;
end;

function TDraggableMarkerPlugin.GetOrgPosition(AMapView : TMapView): TRealPoint;
var
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  Result.InitXY(0.0,0.0);
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) then
    Result := lDraggableMarkerData.FOrgPosition;
end;

procedure TDraggableMarkerPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  lDraggableMarkerData : TDraggableMarkerData;
begin
  if Handled then Exit;
  if not MapViewEnabled[AMapView] then Exit;
  if FDragMouseButton <> Button then Exit;
  lDraggableMarkerData.FDraggedMarker := GetFirstMarkerAtMousePos(AMapView,X,Y);
  if Assigned(lDraggableMarkerData.FDraggedMarker) then
  begin
    lDraggableMarkerData.FOrgPosition.Lon:= lDraggableMarkerData.FDraggedMarker.Lon;
    lDraggableMarkerData.FOrgPosition.Lat:= lDraggableMarkerData.FDraggedMarker.Lat;
    SetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
    Handled := True;
  end;
end;

procedure TDraggableMarkerPlugin.MouseMove(AMapView: TMapView;
  AShift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  pt : TPoint;
  rpt : TRealPoint;
  ele : Double;
  dt : TDateTime;
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if not MapViewEnabled[AMapView] then Exit;
  if (cnt >= SizeOf(lDraggableMarkerData)) and
     Assigned(lDraggableMarkerData.FDraggedMarker) then
  begin
    pt.X := X;
    pt.Y := Y;
    rpt := AMapView.ScreenToLatLon(pt);
    ele := lDraggableMarkerData.FDraggedMarker.Elevation;
    dt := lDraggableMarkerData.FDraggedMarker.DateTime;
    lDraggableMarkerData.FDraggedMarker.MoveTo(rpt.Lon, rpt.Lat,ele,dt);
    AMapView.Invalidate;
    Handled := True; // Prevent the dragging of the map!!
  end
  else
  begin
    if Assigned(GetFirstMarkerAtMousePos(AMapView,X,Y)) then
    begin
      AMapView.Cursor := crHandPoint;
      Handled := True;
    end
    else if not Handled then
      AMapView.Cursor := crDefault;
  end
end;

procedure TDraggableMarkerPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  lpDraggableMarkerData : PDraggableMarkerData;
begin
  if not MapViewEnabled[AMapView] then Exit;
  if FDragMouseButton <> Button then Exit;
  lpDraggableMarkerData := MapViewDataPtr[AMapView];
  if Assigned(lpDraggableMarkerData) and Assigned(lpDraggableMarkerData^.FDraggedMarker) then
  begin
    if Assigned(FDraggableMarkerMovedEvent) then
      FDraggableMarkerMovedEvent(Self,lpDraggableMarkerData^.FDraggedMarker,lpDraggableMarkerData^.FOrgPosition);
    Handled := True;
    lpDraggableMarkerData^.FDraggedMarker := Nil;
  end;
end;


{ TMvCustomPlugin }

procedure TUserDefinedPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  if Assigned(FAfterDrawObjectsEvent) then
    FAfterDrawObjectsEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.AfterDrawTile(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; ATileID: TTileID; ARect: TRect;
  var Handled: Boolean);
begin
  if Assigned(FAfterDrawTileEvent) then
    FAfterDrawTileEvent(Self, AMapView, ADrawingEngine, ATileID, ARect, Handled);
end;

procedure TUserDefinedPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FAfterPaintEvent) then
    FAfterPaintEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.BeforeDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  if Assigned(FBeforeDrawObjectsEvent) then
    FBeforeDrawObjectsEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.CenterMove(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FCenterMoveEvent) then
    FCenterMoveEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.CenterMoving(AMapView: TMapView;
  var NewCenter: TRealPoint; var Allow, Handled: Boolean);
begin
  if Assigned(FCenterMovingEvent) then
    FCenterMovingEvent(Self, AMapView, NewCenter, Allow, Handled);
end;

procedure TUserDefinedPlugin.DrawGPSPoint(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; APoint: TGPSPoint; var Handled: Boolean);
begin
  if Assigned(FDrawGPSPointEvent) then
    FDrawGPSPointEvent(Self, AMapView, ADrawingEngine, APoint, Handled);
end;

procedure TUserDefinedPlugin.DrawMissingTile(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; ATileID: TTileID; ARect: TRect;
  var Handled: Boolean);
begin
  if Assigned(FDrawMissingTileEvent) then
    FDrawMissingTileEvent(Self, AMapView, ADrawingEngine, ATileID, ARect, Handled);
end;

procedure TUserDefinedPlugin.GPSItemsModified(AMapView: TMapView;
  ChangedList: TGPSObjectList; ActualObjs: TGPSObjList; Adding: Boolean;
  var Handled: Boolean);
begin
  if Assigned(FGPSItemsModifiedEvent) then
    FGPSItemsModifiedEvent(Self, AMapView, ChangedList, ActualObjs, Adding, Handled);
end;

procedure TUserDefinedPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseDownEvent) then
    FMouseDownEvent(Self, AMapView, Button, Shift, X,Y, Handled);
end;

procedure TUserDefinedPlugin.MouseEnter(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FMouseEnterEvent) then
    FMouseEnterEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.MouseLeave(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FMouseLeaveEvent) then
    FMouseLeaveEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState; X,
  Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseMoveEvent) then
    FMouseMoveEvent(Self,AMapView, AShift, X, Y, Handled);
end;

procedure TUserDefinedPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseUpEvent) then
    FMouseUpEvent(Self, AMapView, Button, Shift, X, Y, Handled);
end;

procedure TUserDefinedPlugin.MouseWheel(AMapView: TMapView; AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FMouseWheelEvent) then
    FMouseWheelEvent(Self, AMapView, AShift, AWheelDelta, AMousePos, Handled);
end;

procedure TUserDefinedPlugin.Resize(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FResizeEvent) then
    FResizeEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.ZoomChange(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FZoomChangeEvent) then
    FZoomChangeEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.ZoomChanging(AMapView: TMapView; NewZoom: Integer;
  var Allow, Handled: Boolean);
begin
  if Assigned(FZoomChangingEvent) then
    FZoomChangingEvent(Self, AMapView, NewZoom, Allow, Handled);
end;


initialization
  RegisterPluginClass(TCenterMarkerPlugin, 'Center marker');
  RegisterPluginClass(TTileInfoPlugin, 'Tile info');
  RegisterPluginClass(TLegalNoticePlugin, 'Legal notice');
  RegisterPluginClass(TLinkedMapsPlugin, 'Linked maps');
  RegisterPluginClass(TDraggableMarkerPlugin, 'Draggable marker');
  RegisterPluginClass(TUserDefinedPlugin, 'User-defined');

end.

