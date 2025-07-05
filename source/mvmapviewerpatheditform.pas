unit mvMapViewerPathEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, ActnList, mvMapViewer, mvGpsObj, mvTypes, Types;

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
    Bevel: TBevel;
    cbLon: TEdit;
    cbSelectedLayer: TComboBox;
    cbSelectedPt: TEdit;
    cbLat: TEdit;
    edCaption: TEdit;
    ilImages: TImageList;
    lblInfoText: TLabel;
    lblInfoTitle: TLabel;
    lblCaption: TLabel;
    lblLat: TLabel;
    lblLon: TLabel;
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
    procedure cbLatLonEditingDone(Sender: TObject);
    procedure cbLatEnter(Sender: TObject);
    procedure cbLatLonExit(Sender: TObject);
    procedure cbLonEnter(Sender: TObject);
    procedure cbSelectedLayerDropDown(Sender: TObject);
    procedure cbSelectedLayerSelect(Sender: TObject);
    procedure edCaptionEditingDone(Sender: TObject);
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
    property InternalSelect: Boolean read FInternalSelect write FInternalSelect;
  public
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
    'Select/drag mode|Click point to select. CTRL-click to add to selection.',  // pemSelect
    'POI mode|Click to add point.',                                             // pemAddPOI
    'Track mode|Click to add point, CTRL-click to add last point.',             // pemAddTrack
    'Area mode|Click to add point, CTRL-click to add last point.'               // pemAddArea
  );

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
begin
  Pt := MapView.EditMark.CurrentPoint;
  PtCol := Pt.Collection;
  PtId := Pt.ID;

  if (Pt is TMapAreaPoint) and (PtCol.Count < 4) or
     (Pt is TMapTrackPoint) and (PtCol.Count < 3)
  then
    Exit; // Not enough points left

  if MessageDlg('Confirm deletion', 'Are you sure you want to delete ''' +
    Pt.DisplayName + '''?', mtConfirmation, mbYesNo, 0 ) <> mrYes then
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
  TRACK_AREA: array[boolean] of String = ('track', 'area');
var
  msg: String;
begin
  msg := Format(
    'Click on "OK" to add the new %s.' + LineEnding +
    'Click on "Cancel" to discard it.', [
    TRACK_AREA[FEditMode = pemAddArea]
  ]);
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

procedure TMapViewerPathEditForm.cbLatLonEditingDone(Sender: TObject);
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
    raise EArgumentException.Create('Invalid value.');
  // Assignment
  IsLat := Sender = cbLat;
  for P in MapView.EditMark.Selection.Points do
  begin
    if IsLat
      then P.Latitude := Deg
      else P.Longitude := Deg;
    ObjectModified(P, '');
  end;
end;

procedure TMapViewerPathEditForm.cbLatEnter(Sender: TObject);
begin
  cbLat.Caption := LatToStr(MapView.EditMark.CurrentPoint.Latitude,
    mvoLatLonInDMS in MapView.Options);
end;

procedure TMapViewerPathEditForm.cbLatLonExit(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMapViewerPathEditForm.cbLonEnter(Sender: TObject);
begin
  cbLon.Caption := LonToStr(MapView.EditMark.CurrentPoint.Longitude,
    mvoLatLonInDMS in MapView.Options);
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

procedure TMapViewerPathEditForm.FormActivate(Sender: TObject);
var
  w: Integer;
begin
  if not FActivated then
  begin
    AutoSize := false;
    w := MaxValue([lblSelectedPt.Width, lblLat.Width, lblLon.Width, lblCaption.Width]);
    cbSelectedPt.Left := w + lblSelectedPt.BorderSpacing.Left + lblSelectedPt.BorderSpacing.Right;
    cbSelectedLayer.Left := cbSelectedPt.Left + pnlSel.Left;
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
  cbSelectedLayer.Items.Add('(none)'); // At 0
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
    V := (ASender as TMapLayers).View;
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

destructor TMapViewerPathEditForm.Destroy;
begin
  EditMode := pemSelect;
  MapView := Nil;
  inherited Destroy;
end;

procedure TMapViewerPathEditForm.UpdateControls;
var
  P, P0: TMapPoint;
  PtTxt: String;
  PtCnt: Integer = 0;
  HaveView, HaveLayer, HaveSel, HavePt: Boolean;
  VaryingLat, VaryingLon: Boolean;
  Erasable: Boolean = False;
begin
  HaveView := Assigned(MapView);
  HaveSel := HaveView and MapView.EditMark.HasSelection;
  HavePt := HaveSel and (MapView.EditMark.Selection[0] is TMapPoint);
  HaveLayer := Assigned(MapLayer);

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
    else Caption := TMapView.ClassName + ' (Not selected)';

  // Update layer name
  if HaveLayer
    then cbSelectedLayer.Text := MapItemCaption(MapLayer)
    else cbSelectedLayer.Text := '(none)';
  cbSelectedLayer.Hint := cbSelectedLayer.Text;

  // Update currently selected point
  PtTxt := '(none)';
  if HavePt then
  begin
    for P in MapView.EditMark.Selection.Points do
      Inc(PtCnt);
    P0 := TMapPoint(MapView.EditMark.Selection[0]);

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

    if VaryingLat
      then cbLat.Caption := '(varying)'
      else cbLat.Caption := LatToStr(P0.Latitude, mvoLatLonInDMS in MapView.Options);

    if VaryingLon
      then cbLon.Caption := '(varying)'
      else cbLon.Caption := LonToStr(P0.Longitude, mvoLatLonInDMS in MapView.Options);

    edCaption.Text := P0.Caption;

    FPointCnt := PtCnt;
    if PtCnt > 0 then
    begin
      PtTxt := MapItemCaption(P);
      if PtCnt > 1 then
        PtTxt := PtTxt + Format(' +%d more', [PtCnt - 1]);
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
    cbLat.Caption := '';
    cbLon.Caption := '';
    edCaption.Text := '';
  end;

  cbSelectedPt.Text := PtTxt;
  cbSelectedPt.Hint := PtTxt;

  cbLat.Enabled := HavePt;
  lblLat.Enabled := HavePt;
  cbLon.Enabled := HavePt;
  lblLon.Enabled := HavePt;
  edCaption.Enabled := HavePt and (P0 is TMapPointOfInterest);
  lblCaption.Enabled := edCaption.Enabled;

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

