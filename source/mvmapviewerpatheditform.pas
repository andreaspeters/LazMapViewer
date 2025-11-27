unit mvMapViewerPathEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Math,
  Forms, Controls, LCLType, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, ActnList, EditBtn,
  mvStrConsts, mvMapViewer, mvGpsObj, mvTypes, Types;

type

  TMapViewerPathEditMode = (pemSelect, pemAddPOI, pemAddTrack, pemAddArea);

  { TMapViewerPathEditForm }

  TMapViewerPathEditForm = class(TForm, IFPObserver)
    actDelTP: TAction;
    actNewArea: TAction;
    actSelect: TAction;
    actNewTP: TAction;
    actNewTrack: TAction;
    actNewPOI: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    alEditActions: TActionList;
    cmbImageIndex: TComboBox;
    edDateTime: TEdit;
    edLongitude: TEdit;
    edElevation: TEdit;
    cbSelectedLayer: TComboBox;
    cbSelectedPt: TEdit;
    edLatitude: TEdit;
    edCaption: TEdit;
    ilImages: TImageList;
    lblImageIndex: TLabel;
    lblDateTime: TLabel;
    lblMeters: TLabel;
    lblInfoText: TLabel;
    lblInfoTitle: TLabel;
    lblCaption: TLabel;
    lblLatitude: TLabel;
    lblLongitude: TLabel;
    lblElevation: TLabel;
    lblSelectedLayer: TLabel;
    lblSelectedPt: TLabel;
    pnlSel: TPanel;
    pnlInfo: TPanel;
    pnlFrame: TPanel;
    ToolBar: TToolBar;
    tbSelect: TToolButton;
    tbZoomIn: TToolButton;
    tbZoomOut: TToolButton;
    tbNewPOI: TToolButton;
    tbNewArea: TToolButton;
    tbNewTrack: TToolButton;
    ToolButton6: TToolButton;
    tbNewTrackPoint: TToolButton;
    tbDeleteTrackPoint: TToolButton;
    ToolButton9: TToolButton;
    procedure actDelTPExecute(Sender: TObject);
    procedure actNewAreaExecute(Sender: TObject);
    procedure actNewPOIExecute(Sender: TObject);
    procedure actNewTPExecute(Sender: TObject);
    procedure actNewTrackExecute(Sender: TObject);
    procedure actSelectExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure cbSelectedLayerDropDown(Sender: TObject);
    procedure cbSelectedLayerSelect(Sender: TObject);
    procedure cmbImageIndexCloseUp(Sender: TObject);
    procedure cmbImageIndexDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure edCaptionEditingDone(Sender: TObject);
    procedure edDateTimeEditingDone(Sender: TObject);
    procedure edDateTimeEnter(Sender: TObject);
    procedure edEditExit(Sender: TObject);
    procedure edElevationEditingDone(Sender: TObject);
    procedure edLatLonEditingDone(Sender: TObject);
    procedure edElevationEnter(Sender: TObject);
    procedure edLatitudeEnter(Sender: TObject);
    procedure edLongitudeEnter(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FPointCnt: Integer;
    FMapLayer: TMapLayer;
    FMapView: TMapView;
    FInternalSelect: Boolean;
    FTempPolyLine: TGPSPolyLine;
    FEditMode: TMapViewerPathEditMode;
    FSkipAPoint: Boolean;
    FActivated: Boolean;
    procedure AddTempPolylineOrRevert(ANewEditMode: TMapViewerPathEditMode);
    procedure SetEditMode(AValue: TMapViewerPathEditMode);
    procedure CancelAddMode;
    procedure AddTempPoint;
    procedure NewTrackFromTemp;
    procedure NewAreaFromTemp;
    procedure NewPOIFromTemp;
    procedure SetMapLayer(AValue: TMapLayer);
    procedure SetMapView(AValue: TMapView);
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
    procedure DrawTempTrack(Sender: TObject; AGPSObj: TGPSObj; AArea: TRealArea);
    procedure DrawTempArea(Sender: TObject; AGPSObj: TGPSObj; {%H-}AArea: TRealArea);
    procedure DrawTempMark(AView: TMapView; APt: TRealPoint);
  protected
    procedure UpdateControls;
    procedure UpdateInfoPanel;
    procedure UpdateLayerItems;
    function GetOwnerOfType(ANested: TPersistent; AClass: TClass): TPersistent;
    procedure PersistentAdded({%H-}APersistent: TPersistent; {%H-}Select: Boolean); virtual;
    procedure DeletePersistent({%H-}APersistent: TPersistent); virtual;
    procedure UnselectPersistent({%H-}APersistent: TPersistent); virtual;
    procedure ObjectModified({%H-}AObject: TObject; {%H-}PropName: ShortString = ''); virtual;
    procedure SelectInOI(AView: TMapView; {%H-}ForceUpdate: Boolean); virtual;
    procedure SetStrings;
    property InternalSelect: Boolean read FInternalSelect write FInternalSelect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MapView: TMapView read FMapView write SetMapView;
    property MapLayer: TMapLayer read FMapLayer write SetMapLayer;
    property EditMode: TMapViewerPathEditMode read FEditMode write SetEditMode;
  end;

var
  MapViewerPathEditForm: TMapViewerPathEditForm;

implementation

uses
  mvGeoMath;

const
  EditModeHints: array[TMapViewerPathEditMode] of String = (
    mvRS_SelectDragMode,    // pemSelect
    mvRS_POIMode,           // pemAddPOI
    mvRS_TrackMode,         // pemAddTrack
    mvRS_AreaMode           // pemAddArea
  );

  ELEVATION_FORMAT = '0';            // no decimal places
  DATETIME_FORMAT = 'ddddd t';       // short date & short time format
  IMG_MARGIN = 2;

type
  TPersistentAccess = class(TPersistent);

{$R *.lfm}

function MapItemCaption(AItem: TMapItem): String;
begin
  Result := AItem.DisplayName;
  if Result <> AItem.ClassName then
    Result := Result + ': ' + AItem.ClassName;
  if Assigned(AItem.Collection) then
    Result := Format('%d - ', [AItem.Index]) + Result;
end;

{ TMapViewerPathEditForm }

constructor TMapViewerPathEditForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lblInfoTitle.Caption := '';
  lblInfoText.Caption := '';
  SetStrings;
end;

destructor TMapViewerPathEditForm.Destroy;
begin
  EditMode := pemSelect;
  MapView := nil;
  inherited Destroy;
end;

procedure TMapViewerPathEditForm.actSelectExecute(Sender: TObject);
begin
  if (FTempPolyLine <> nil) and (FTempPolyLine.Points.Count > 0) then
    AddTempPolyLineOrRevert(pemSelect);
  EditMode := pemSelect;
end;

procedure TMapViewerPathEditForm.actNewPOIExecute(Sender: TObject);
begin
  if (FTempPolyLine <> nil) and (FTempPolyLine.Points.Count > 0) then
    AddTempPolyLineOrRevert(pemAddPOI);
  EditMode := pemAddPOI;
end;

procedure TMapViewerPathEditForm.actNewTrackExecute(Sender: TObject);
begin
  if (FTempPolyLine <> nil) and (FTempPolyLine.Points.Count > 0) then
    AddTempPolyLineOrRevert(pemAddTrack);
  EditMode := pemAddTrack;
end;

procedure TMapViewerPathEditForm.actNewAreaExecute(Sender: TObject);
begin
  if (FTempPolyLine <> nil) and (FTempPolyLine.Points.Count > 0) then
    AddTempPolyLineOrRevert(pemAddArea);
  EditMode := pemAddArea;
end;

procedure TMapViewerPathEditForm.actNewTPExecute(Sender: TObject);
var
  P: TMapPoint;
  L: TMapTrack;
  A: TMapArea;
  C: TCollection;
  I, I1, I2: Integer;
  V: TMapView;

  procedure InsertPt(J: Integer; ANext: TMapPoint);
  var
    PN: TMapPoint;
  begin
    PN := C.Insert(J) as TMapPoint;
    PN.Latitude := (P.Latitude + ANext.Latitude) / 2;
    PN.Longitude := (P.Longitude + ANext.Longitude) / 2;
    MapView.EditMark.Selection.Add(C);
    MapView.EditMark.Selection.Insert(0, PN);
    PersistentAdded(PN, True);
  end;

begin
  P := MapView.EditMark.CurrentPoint;
  L := MapView.EditMark.CurrentTrack;
  if Assigned(L) then
    C := L.Points
  else
  begin
    A := MapView.EditMark.CurrentArea;
    if not Assigned(A) then
      Exit;
    C := A.Points;
  end;
  I := P.Index;
  if I < 0 then
    Exit;
  I1 := (I + 1) mod C.Count; // Next point index
  if I > 0
    then I2 := Pred(I)  // Prev point index
    else I2 := Pred(C.Count);
  V := MapView;
  try
    // If the next point on track/area is selected
    // then insert before next
    if MapView.EditMark.IsSelected(C.Items[I1]) then
      InsertPt(I1, C.Items[I1] as TMapPoint)
    // If the prev point on track/area is selected
    // then insert before current
    else if MapView.EditMark.IsSelected(C.Items[I2]) then
      InsertPt(I, C.Items[I2] as TMapPoint)
    // If the current point is not the last
    // then insert before next(last)
    else if I < Pred(C.Count) then
      InsertPt(I1, C.Items[I1] as TMapPoint)
    // else insert before current
    else
      InsertPt(I, C.Items[I2] as TMapPoint);
  finally
    MapView := V;
    SelectInOI(V, False);
  end;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.actDelTPExecute(Sender: TObject);
var
  Pt: TMapPoint;
  PtId: Integer;
  PtCol: TCollection;
  P: TPersistent;
  msg: String;
begin
  Pt := MapView.EditMark.CurrentPoint;
  PtCol := Pt.Collection;
  PtId := Pt.ID;

  if (Pt is TMapAreaPoint) and (PtCol.Count < 4) or
     (Pt is TMapTrackPoint) and (PtCol.Count < 3)
  then
    Exit; // Not enough points left

  msg := Format(mvRS_AreYouSureYouWantToDelete, [Pt.DisplayName]);
  if MessageDlg(mvRS_ConfirmDeletion, msg, mtConfirmation, mbYesNo, 0 ) <> mrYes then
    Exit;

  // Exclude from the selection
  MapView.EditMark.Selection.DelIfPresent(PtCol);
  MapView.EditMark.Selection.DelIfPresent(Pt);

  // No points left?
  if MapView.EditMark.HasSelection and
    not (MapView.EditMark.Selection[0] is TMapPoint) then
    MapView.EditMark.ClearSelection; // Clear the whole selection

  // Delete from the object inspector
  UnselectPersistent(Pt);
  P := Pt; DeletePersistent(P);

  // In case DeletePersistent() isn't overriden (stub)
  if PtCol.FindItemID(PtId) = Pt then
    PtCol.Delete(Pt.Index);

  UpdateControls;
end;

procedure TMapViewerPathEditForm.actZoomInExecute(Sender: TObject);
begin
  MapView.Zoom := MapView.Zoom + 1;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.actZoomOutExecute(Sender: TObject);
begin
  MapView.Zoom := MapView.Zoom - 1;
  UpdateControls;
end;

{ When points for a new track or a new area are being added, but the user
  selects another edit mode ("ANewEditMode"), the already prepared points
  would be lost. --> We ask whether the track/area should be used or discarded. }
procedure TMapViewerPathEditForm.AddTempPolylineOrRevert(ANewEditMode: TMapViewerPathEditMode);
const
  TRACK_AREA: array[boolean] of String = (mvRS_Track, mvRS_Area);
var
  msg: String;
begin
  msg := Format(mvRS_ClickToAddNew, [TRACK_AREA[FEditMode = pemAddArea]]) +
    LineEnding +
    mvRS_ClickToDiscardIt;
  if MessageDlg(msg, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
  begin
    case FEditMode of
      pemAddArea: NewAreaFromTemp;
      pemAddTrack: NewTrackFromTemp;
    end;
    // Tool button checked state was changed in previous command --> restore it.
    case ANewEditMode of
      pemSelect: actSelect.Checked := true;
      pemAddPOI: actNewPOI.Checked := true;
      pemAddArea: actNewArea.Checked := true;
      pemAddTrack: actNewTrack.Checked := true;
    end;
  end;
end;

procedure TMapViewerPathEditForm.cbSelectedLayerDropDown(Sender: TObject);
begin
  UpdateLayerItems;
end;

procedure TMapViewerPathEditForm.cbSelectedLayerSelect(Sender: TObject);
begin
  if cbSelectedLayer.ItemIndex = 0
    then MapLayer := Nil
    else MapLayer := MapView.Layers[Pred(cbSelectedLayer.ItemIndex)];
  UpdateControls;
end;

procedure TMapViewerPathEditForm.cmbImageIndexCloseUp(Sender: TObject);
var
  cmb: TComboBox;
  P: TMapPoint;
begin
  cmb := Sender as TComboBox;
  for P in MapView.EditMark.Selection.Points do
  begin
    if (P is TMapPointOfInterest) then
    begin
      if cmb.ItemIndex = -1 then
        TMapPointOfInterest(P).ImageIndex := -1
      else
        TMapPointOfInterest(P).ImageIndex := cmb.ItemIndex - 1;
      ObjectModified(P, '');
    end;
  end;
end;

procedure TMapViewerPathEditForm.cmbImageIndexDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  cmb: TCombobox;
  h: Integer;
begin
  cmb := Control as TComboBox;
  cmb.Canvas.Font.Assign(cmb.Font);
  h := cmb.Canvas.TextHeight('Tgj');
  if (odFocused in State) then
  begin
    cmb.Canvas.Brush.Color := clHighlight;
    cmb.Canvas.Font.Color := clHighlightText;
  end else
  begin
    cmb.Canvas.Brush.Color := clWindow;
    cmb.Canvas.Font.Color := clWindowText;
  end;
  if not (odBackgroundPainted in State) then
    cmb.Canvas.FillRect(ARect);
  InflateRect(ARect, -IMG_MARGIN, -IMG_MARGIN);
  if Index <= 0 then
    cmb.Canvas.TextOut(ARect.Left, (ARect.Top + ARect.Bottom - h) div 2, mvRS_None)
  else
    FMapView.POIImages.Draw(cmb.Canvas, ARect.Left, ARect.Top, Index - 1);
end;

procedure TMapViewerPathEditForm.edCaptionEditingDone(Sender: TObject);
var
  E: TEdit;
  P: TMapPoint;
begin
  E := Sender as TEdit;
  if not E.Modified then
    Exit;
  for P in MapView.EditMark.Selection.Points do
  begin
    if (P is TMapPointOfInterest) then
    begin
      P.Caption := edCaption.Text;
      ObjectModified(P, '');
    end;
  end;
end;

procedure TMapViewerPathEditForm.edDateTimeEditingDone(Sender: TObject);
var
  E: TEdit;
  dateVal: TDate = NO_DATE;
  timeVal: TTime = 0.0;
  dt: TDateTime;
  P: TMapPoint;
  sa: TStringArray;
begin
  E := Sender as TEdit;
  if not E.Modified then
    exit;

  if E.Text = '' then
    dt := NO_DATE
  else
  begin
    sa := String(E.Text).Split(' ');
    if sa[0] <> '' then
    begin
      if not TryStrToDate(sa[0], dateVal) then
        dateVal := NO_DATE
    end;
    if (Length(sa) > 1) and (sa[1] <> '') then
    begin
      if not TryStrToTime(sa[1], timeVal) then
        timeVal := 0.0;
    end;
    if dateVal <> NO_DATE then
      dt := dateVal + timeVal
    else
      dt := NO_DATE;
  end;

  for P in MapView.EditMark.Selection.Points do
  begin
    P.DateTime := dt;
    ObjectModified(P, '');
  end;
end;

procedure TMapViewerPathEditForm.edDateTimeEnter(Sender: TObject);
begin
  if MapView.EditMark.CurrentPoint.DateTime = NO_DATE then
    edDateTime.Text := ''
  else
    edDateTime.Text := FormatDateTime(DATETIME_FORMAT, MapView.EditMark.CurrentPoint.DateTime);
end;

procedure TMapViewerPathEditForm.edElevationEditingDone(Sender: TObject);
var
  E: TEdit;
  elev: Double;
  P: TMapPoint;
begin
  E := Sender as TEdit;
  if not E.Modified then
    exit;
  if E.Text = '' then
    elev := NO_ELEVATION
  else
  if not TryStrToFloat(E.Text, elev) then
    raise EArgumentException.Create(mvRS_InvalidValue);
  for P in MapView.EditMark.Selection.Points do
  begin
    P.Elevation := elev;
    ObjectModified(P, '');
  end;
end;

procedure TMapViewerPathEditForm.edElevationEnter(Sender: TObject);
begin
  if MapView.EditMark.CurrentPoint.Elevation = NO_ELEVATION then
    edElevation.Text := ''
  else
    edElevation.Text := FormatFloat(ELEVATION_FORMAT, MapView.EditMark.CurrentPoint.Elevation);
end;

procedure TMapViewerPathEditForm.edLatitudeEnter(Sender: TObject);
begin
  edLatitude.Text := LatToStr(MapView.EditMark.CurrentPoint.Latitude,
    mvoLatLonInDMS in MapView.Options);
end;

procedure TMapViewerPathEditForm.edLatLonEditingDone(Sender: TObject);
var
  E: TEdit;
  Deg: Double;
  R: Boolean;
  P: TMapPoint;
  IsLat: Boolean;
begin
  E := Sender as TEdit;
  if not E.Modified then
    Exit;
  R := TryStrDMSToDeg(E.Text, Deg);
  if not R then
    raise EArgumentException.Create(mvRS_InvalidValue);
  // Assignment
  IsLat := Sender = edLatitude;
  for P in MapView.EditMark.Selection.Points do
  begin
    if IsLat
      then P.Latitude := Deg
      else P.Longitude := Deg;
    ObjectModified(P, '');
  end;
end;

procedure TMapViewerPathEditForm.edEditExit(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMapViewerPathEditForm.edLongitudeEnter(Sender: TObject);
begin
  edLongitude.Caption := LonToStr(MapView.EditMark.CurrentPoint.Longitude,
    mvoLatLonInDMS in MapView.Options);
end;

procedure TMapViewerPathEditForm.FormActivate(Sender: TObject);
var
  w: Integer;
begin
  if not FActivated then
  begin
    AutoSize := false;
    w := MaxValue([lblSelectedPt.Width, lblLatitude.Width, lblLongitude.Width,
      lblElevation.Width, lblDateTime.Width, lblCaption.Width, lblImageIndex.Width]);
    cbSelectedPt.Left := w + lblSelectedPt.BorderSpacing.Left + lblSelectedPt.BorderSpacing.Right;
    cbSelectedLayer.Left := cbSelectedPt.Left + pnlSel.Left;
    inc(w, 8+6);
    lblSelectedPt.Constraints.MinWidth := w;
    lblLatitude.Constraints.MinWidth := w;
    lblLongitude.Constraints.MinWidth := w;
    lblElevation.Constraints.MinWidth := w;
    lblDateTime.Constraints.MinWidth := w;
    lblCaption.Constraints.MinWidth := w;
    lblImageIndex.Constraints.MinWidth := w;
    AutoSize := true;
    FActivated := true;
  end;
end;

procedure TMapViewerPathEditForm.FormShow(Sender: TObject);
begin
  UpdateInfoPanel;
end;

procedure TMapViewerPathEditForm.UpdateInfoPanel;
var
  sa: TStringArray;
begin
  sa := EditModeHints[FEditMode].Split('|');
  lblInfoTitle.Caption := sa[0];
  lblInfoText.Caption := sa[1];
end;

procedure TMapViewerPathEditForm.UpdateLayerItems;
var
  L: TCollectionItem;
begin
  cbSelectedLayer.Items.Clear;
  cbSelectedLayer.Items.Add(mvRS_None); // At 0
  if Assigned(MapView) then
    for L in MapView.Layers do
      cbSelectedLayer.Items.Add(MapItemCaption(TMapLayer(L)));
end;

procedure TMapViewerPathEditForm.SetMapLayer(AValue: TMapLayer);
begin
  if FMapLayer = AValue then Exit;
  FMapLayer := AValue;
end;

procedure TMapViewerPathEditForm.SetEditMode(AValue: TMapViewerPathEditMode);

  procedure AddTempLine(ALine: TGPSPolyLine);
  begin
    FTempPolyLine := ALine;
    MapView.GPSItems.Add(FTempPolyLine, {_MAPEDITOR_ID_=}-42, MaxInt);
  end;

  procedure RemoveTempLine;
  begin
    if Assigned(FTempPolyLine) then
    begin
      MapView.GPSItems.Delete(FTempPolyLine);
      FTempPolyLine := Nil;
    end;
  end;

begin
  if FEditMode = AValue then Exit;
  case AValue of
    pemSelect:
      begin
        RemoveTempLine;
        actSelect.Checked := True;
        MapView.EditMark.CursorShape := crDefault;
      end;
    pemAddTrack:
      begin
        MapView.EditMark.ClearSelection;
        RemoveTempLine;
        AddTempLine(TGPSTrack.Create);
        with TGPSTrack(FTempPolyLine) do
        begin
          LineWidth := 0.4;
          LineColor := clBlack;
          Opacity := 0.4;
          OnDrawObj := @DrawTempTrack;
        end;
        MapView.EditMark.CursorShape := crCross;
        edCaption.Enabled := false;
        lblCaption.Enabled := false;
      end;
    pemAddArea:
      begin
        MapView.EditMark.ClearSelection;
        RemoveTempLine;
        AddTempLine(TGPSArea.Create);
        with TGPSArea(FTempPolyLine) do
        begin
          LineColor := clNone;
          FillColor := clBlack;
          Opacity := 0.2;
          OnDrawObj := @DrawTempArea;
        end;
        MapView.EditMark.CursorShape := crCross;
        edCaption.Enabled := false;
        lblCaption.Enabled := false;
      end;
    pemAddPOI:
      begin
        MapView.EditMark.ClearSelection;
        RemoveTempLine;
        AddTempLine(TGPSTrack.Create);
        MapView.EditMark.CursorShape := crCross;
        edCaption.Enabled := true;
        lblCaption.Enabled := true;
      end;
  end;
  FEditMode := AValue;
  UpdateInfoPanel;
  MapView.Invalidate;
end;

procedure TMapViewerPathEditForm.SetMapView(AValue: TMapView);
begin
  if FMapView = AValue then
    Exit;
  // Detach the old FMapView
  if Assigned(FMapView) then
  begin
    FMapView.FPODetachObserver(Self);
    FMapView.Layers.FPODetachObserver(Self);
    FMapView.ControlStyle := FMapView.ControlStyle - [csDesignInteractive];
    EditMode := pemSelect; // Should free the FTempPolyLine if allocated
  end;
  // Attach the new FMapView
  if Assigned(AValue) then
  begin
    AValue.FPOAttachObserver(Self);
    AValue.Layers.FPOAttachObserver(Self);
    if Visible then
      AValue.ControlStyle := AValue.ControlStyle + [csDesignInteractive];
  end;
  FMapLayer := Nil;
  FMapView := AValue;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.CancelAddMode;
begin
  EditMode := pemSelect;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.AddTempPoint;
var
  P: TPoint;
  RealPt: TRealPoint;
begin
  if not (EditMode in [pemAddPOI, pemAddTrack, pemAddArea]) then
    Exit;
  P := Mouse.CursorPos;
  P := MapView.ScreenToControl(P);
  RealPt := MapView.ScreenToLatLon(P);
  FTempPolyLine.Points.Add(TGPSPoint.CreateFrom(RealPt));
  if EditMode = pemAddPOI then
    NewPOIFromTemp
  else
  if ssCtrl in GetKeyShiftState then
    case EditMode of
      pemAddTrack: NewTrackFromTemp;
      pemAddArea: NewAreaFromTemp;
    end;
  MapView.Invalidate;
end;

procedure TMapViewerPathEditForm.NewTrackFromTemp;
var
  Trk: TMapTrack;
  I: Integer;
  P: TMapTrackPoint;
begin
  if not Assigned(MapLayer) then
    Exit;
  if FTempPolyLine.Points.Count < 2 then
    Exit;
  Trk := TMapTrack(MapLayer.Tracks.Add);
  for I := 0 to Pred(FTempPolyLine.Points.Count) do
  begin
    P := TMapTrackPoint(Trk.Points.Add);
    P.RealPoint := FTempPolyLine.Points[I].RealPoint;
    MapView.EditMark.Select(P);
  end;
  CancelAddMode;
  try
    PersistentAdded(Trk, True);
  finally
    SelectInOI(MapView, False);
  end;
end;

procedure TMapViewerPathEditForm.NewAreaFromTemp;
var
  Ar: TMapArea;
  I: Integer;
  P: TMapAreaPoint;
begin
  if not Assigned(MapLayer) then
    Exit;
  if FTempPolyLine.Points.Count < 3 then
    Exit;
  Ar := TMapArea(MapLayer.Areas.Add);
  for I := 0 to Pred(FTempPolyLine.Points.Count) do
  begin
    P := TMapAreaPoint(Ar.Points.Add);
    P.RealPoint := FTempPolyLine.Points[I].RealPoint;
    MapView.EditMark.Select(P);
  end;
  CancelAddMode;
  try
    PersistentAdded(Ar, True);
  finally
    SelectInOI(MapView, False);
  end;
end;

procedure TMapViewerPathEditForm.NewPOIFromTemp;
var
  P: TMapPointOfInterest;
begin
  if not Assigned(MapLayer) then
    Exit;
  if FTempPolyLine.Points.Count < 1 then
    Exit;
  P := TMapPointOfInterest(MapLayer.PointsOfInterest.Add);
  P.RealPoint := FTempPolyLine.Points[0].RealPoint;
  CancelAddMode;
  try
    PersistentAdded(P, True);
  finally
    SelectInOI(MapView, False);
  end;
end;

procedure TMapViewerPathEditForm.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
var
  What: TMapObserverCustomOperation;
  V: TMapView;

  procedure MapViewChanged;
  begin
    if Operation = ooFree then
    begin
      FMapView := Nil; // Too late to call anyhing
      FTempPolyLine := Nil; // Probably dangling
      FMapLayer := Nil; // Ditto
      UpdateControls;
      Exit;
    end;

    if Operation <> ooCustom then
      Exit;

    V := ASender as TMapView;
    What := PMapObserverCustomOperation(Data)^;
    case What of

      mooSelectionCompleted:
        begin
          if EditMode = pemSelect then
            SelectInOI(V, False)
          else if FSkipAPoint then
            FSkipAPoint := False
          else
            AddTempPoint;
        end;

      mooStartDrag:
        if EditMode in [pemAddTrack, pemAddArea] then
        begin
          FSkipAPoint := True; // Skip the 2-nd drag point
          V.EditMark.DoEndDrag(Nil);
        end;

      mooDrag: ;

      mooIsDirty: ;

      mooEndDrag:
        if V.EditMark.Dirty then
        begin
          ObjectModified(Self, 'Layers');
          V.EditMark.Dirty := False;
        end;
    end;
  end;

  procedure LayersChanged;
  begin
    if Operation = ooFree then
    begin
      MapLayer := Nil;
      Exit;
    end;
    V := (ASender as TMapLayers).MapView;
    if (Operation = ooDeleteItem) and (Data = Pointer(FMapLayer)) then
    begin
      V.EditMark.ClearSelection;
      MapLayer := Nil;
      // The old layer is still around! Can't UpdateControls!
    end
    else if Operation = ooChange then
      UpdateControls;
  end;

begin
  if ASender is TMapView then
    MapViewChanged
  else if ASender is TMapLayers then
    LayersChanged;
end;

procedure TMapViewerPathEditForm.DrawTempTrack(Sender: TObject;
  AGPSObj: TGPSObj; AArea: TRealArea);
var
  T: TGPSTrack;
  I: Integer;
begin
  T := TGPSTrack(AGPSObj);
  MapView.DrawTrack(AArea, T);
  for I := 0 to Pred(T.Points.Count) do
    DrawTempMark(MapView, T.Points[I].RealPoint)
end;

procedure TMapViewerPathEditForm.DrawTempArea(Sender: TObject;
  AGPSObj: TGPSObj; AArea: TRealArea);
var
  A: TGPSArea;
  I: Integer;
begin
  A := TGPSArea(AGPSObj);
  MapView.DrawArea(AArea, A);
  for I := 0 to Pred(A.Points.Count) do
    DrawTempMark(MapView, A.Points[I].RealPoint)
end;

procedure TMapViewerPathEditForm.DrawTempMark(AView: TMapView; APt: TRealPoint);
var
  P: TPoint;
begin
  P := AView.LatLonToScreen(APt);
  with AView.DrawingEngine do
  begin
    PenStyle := psSolid;
    PenColor := clRed;
    PenWidth := 2;
    Opacity := 1.0;
    Line(P.X - 4, P.Y - 4, P.X + 4, P.Y + 4);
    Line(P.X - 4, P.Y + 4, P.X + 4, P.Y - 4);
  end;
end;

procedure TMapViewerPathEditForm.SelectInOI(AView: TMapView; ForceUpdate: Boolean);
var
  L, L2: TMapLayer;
  P: TMapPoint;
  LC: Integer = 0;
begin
  if not Assigned(AView) then
    Exit;
  L := Nil;
  for P in AView.EditMark.Selection.Points do
  begin
    L2 := TMapLayer(GetOwnerOfType(P, TMapLayer));
    if Assigned(L2) and L2.Visible and (L <> L2)  then
    begin
      Inc(LC);
      L := L2;
    end;
  end;
  if LC = 1 // Just one layer?
    then MapLayer := L // Yes, assign it
    else {MapLayer := Nil}; // Multiple layers or no layer
  UpdateControls;
end;

procedure TMapViewerPathEditForm.SetStrings;
begin
  actSelect.Caption := mvRS_Select;
  actSelect.Hint := mvRS_SelectTool_Hint;
  actNewPOI.Caption := mvRS_NewPOI;
  actNewPOI.Hint := mvRS_NewPOI_Hint;
  actNewTrack.Caption := mvRS_NewTrack;
  actNewTrack.Hint := mvRS_NewTrack_Hint;
  actNewArea.Caption := mvRS_NewArea;
  actNewArea.Hint := mvRS_NewArea_Hint;
  actNewTP.Caption := mvRS_InsertNewPoint;
  actNewTP.Hint := mvRS_InsertNewPoint_Hint;
  actDelTP.Caption := mvRS_DeletePoint;
  actDelTP.Hint := mvRS_DeletePoint_Hint;
  actZoomIn.Caption := mvRS_ZoomIn;
  actZoomIn.Hint := mvRS_ZoomIn_Hint;
  actZoomOut.Caption := mvRS_ZoomOut;
  actZoomOut.Hint := mvRS_ZoomOut_Hint;

  lblSelectedLayer.Caption := mvRS_Layer;
  lblSelectedPt.Caption := mvRS_Selection;
  lblLatitude.Caption := mvRS_Latitude;
  lblLongitude.Caption := mvRS_Longitude;
  lblElevation.Caption := mvRS_Elevation;
  lblDateTime.Caption := mvRS_DateTime;
  lblCaption.Caption := mvRS_Caption;
  lblImageIndex.Caption := mvRS_Image;
end;

procedure TMapViewerPathEditForm.UpdateControls;
var
  P: TMapPoint;
  P0: TMapPoint = nil;
  PtTxt: String;
  i: Integer;
  PtCnt: Integer = 0;
  HaveView, HaveLayer, HaveSel, HavePt, HaveImg: Boolean;
  VaryingLat, VaryingLon, VaryingElev, VaryingDateTime: Boolean;
  Erasable: Boolean = False;
begin
  HaveView := Assigned(MapView);
  HaveSel := HaveView and MapView.EditMark.HasSelection;
  HavePt := HaveSel and (MapView.EditMark.Selection[0] is TMapPoint);
  HaveLayer := Assigned(MapLayer);
  HaveImg := HavePt and Assigned(MapView.POIImages) and (MapView.POIImages.Count > 0);

  if not HaveLayer then
  begin
    if HavePt then
      MapLayer := (MapView.EditMark.Selection[0] as TMapPoint).Layer
    else if HaveView and (MapView.Layers.Count > 0) then
      MapLayer := MapView.Layers.Last;
    HaveLayer := Assigned(MapLayer);
  end;

  if HaveView
    then Caption := MapView.Name + ': ' + TMapView.ClassName
    else Caption := TMapView.ClassName + ' ' + mvRS_NotSelected;

  // Update layer name
  if HaveLayer
    then cbSelectedLayer.Text := MapItemCaption(MapLayer)
    else cbSelectedLayer.Text := mvRS_None;
  cbSelectedLayer.Hint := cbSelectedLayer.Text;

  // Update currently selected point
  PtTxt := mvRS_None;
  if HavePt then
  begin
    for P in MapView.EditMark.Selection.Points do
      Inc(PtCnt);
    P0 := TMapPoint(MapView.EditMark.Selection[0]);
    haveImg := HaveImg and (P0 is TMapPointOfInterest);

    VaryingLat := False;
    for P in MapView.EditMark.Selection.Points.Skip(1) do
      if P.Latitude <> P0.Latitude then
      begin
        VaryingLat := True;
        Break;
      end;

    VaryingLon := False;
    for P in MapView.EditMark.Selection.Points.Skip(1) do
      if P.Longitude <> P0.Longitude then
      begin
        VaryingLon := True;
        Break;
      end;

    VaryingElev := False;
    for P in MapView.EditMark.Selection.Points.Skip(1) do
      if P.Elevation <> P0.Elevation then
      begin
        VaryingElev := True;
        Break;
      end;

    VaryingDateTime := False;
    for P in MapView.EditMark.Selection.Points.Skip(1) do
      if P.DateTime <> P0.DateTime then
      begin
        VaryingDateTime := True;
        Break;
      end;

    if VaryingLat
      then edLatitude.Text := mvRS_Varying
      else edLatitude.Text := LatToStr(P0.Latitude, mvoLatLonInDMS in MapView.Options);

    if VaryingLon
      then edLongitude.Text := mvRS_Varying
      else edLongitude.Text := LonToStr(P0.Longitude, mvoLatLonInDMS in MapView.Options);

    if VaryingElev
      then edElevation.Text := mvRS_Varying
      else if P0.Elevation = NO_ELEVATION
        then edElevation.Text := ''
        else edElevation.Text := FormatFloat(ELEVATION_FORMAT, P0.Elevation);

    if VaryingDateTime
      then edDateTime.Text := mvRS_Varying
      else if P0.DateTime = NO_DATE
        then edDateTime.Text := ''
        else edDateTime.Text := FormatDateTime(DATETIME_FORMAT, P0.DateTime);

    edCaption.Text := P0.Caption;

    if HaveImg then
    begin
      cmbImageIndex.ItemHeight := MapView.POIImages.Height + 2 * IMG_MARGIN;
      cmbImageIndex.Items.Clear;
      for i := 0 to MapView.POIImages.Count do  // missing -1 intentional!
        cmbImageIndex.Items.Add(IntToStr(i));
      cmbImageIndex.ItemIndex := TMapPointOfInterest(P0).ImageIndex + 1;
    end else
      cmbImageIndex.Items.Clear;

    FPointCnt := PtCnt;
    if PtCnt > 0 then
    begin
      PtTxt := MapItemCaption(P);
      if PtCnt > 1 then
        PtTxt := PtTxt + ' +' + Format(mvRS_More, [PtCnt - 1]);
    end;

    if P0.Collection is TMapTrackPoints then
      Erasable := P0.Collection.Count > 2
    else if P0.Collection is TMapAreaPoints then
      Erasable := P0.Collection.Count > 3
    else
      Erasable := True;
  end
  else
  begin
    FPointCnt := 0;
    edLatitude.Text := '';
    edLongitude.Text := '';
    edElevation.Text := '';
    edCaption.Text := '';
    edDateTime.Text := '';
    cmbImageIndex.ItemIndex := -1;
    cmbImageIndex.Items.Clear;
  end;

  cbSelectedPt.Text := PtTxt;
  cbSelectedPt.Hint := PtTxt;

  edLatitude.Enabled := HavePt;
  lblLatitude.Enabled := HavePt;
  edLongitude.Enabled := HavePt;
  lblLongitude.Enabled := HavePt;
  edElevation.Enabled := HavePt;
  lblElevation.Enabled := HavePt;
  lblMeters.Enabled := HavePt;
  edDateTime.Enabled := HavePt;
  lblDateTime.Enabled := HavePt;
  edCaption.Enabled := HavePt and (P0 is TMapPointOfInterest);
  lblCaption.Enabled := edCaption.Enabled;
  cmbImageIndex.Enabled := HaveImg;
  lblImageIndex.Enabled := HaveImg;

  // Update actions
  actZoomIn.Enabled := HaveView and (MapView.Zoom < MapView.ZoomMax);
  actZoomOut.Enabled := HaveView and (MapView.Zoom > MapView.ZoomMin);
  actNewPOI.Enabled := HaveView and HaveLayer;
  actNewTrack.Enabled := HaveView and HaveLayer;
  actNewArea.Enabled := HaveView and HaveLayer;
  actNewTP.Enabled := HaveView and HavePt;
  actDelTP.Enabled := HaveView and HavePt and Erasable;
  actSelect.Enabled := HaveView;
end;

function TMapViewerPathEditForm.GetOwnerOfType(ANested: TPersistent;
  AClass: TClass): TPersistent;
begin
  Result := Nil;
  while Assigned(ANested) do
    if ANested is AClass
      then Exit(ANested)
      else ANested := TPersistentAccess(ANested).GetOwner;
end;

procedure TMapViewerPathEditForm.PersistentAdded(APersistent: TPersistent;
  Select: Boolean);
begin
  ;
end;

procedure TMapViewerPathEditForm.DeletePersistent(APersistent: TPersistent);
begin
  ;
end;

procedure TMapViewerPathEditForm.UnselectPersistent(APersistent: TPersistent);
begin
  ;
end;

procedure TMapViewerPathEditForm.ObjectModified(AObject: TObject;
  PropName: ShortString);
begin
  ;
end;

end.

