unit mvLayersPropEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, ActnList, Menus,
  PropEdits, IDEImagesIntf, ComponentEditors, CollectionPropEditForm,
  mvStrConsts, mvMapViewer, mvGpsObj, mvGPX, mvTypes;

type

  { TLayersPropertyEditForm }

  TLayersPropertyEditForm = class(TCollectionPropertyEditorForm)
    actLoadAndZoom: TAction;
    actSaveGPX: TAction;
    actLoadGPX: TAction;
    mnuLoadGPX: TMenuItem;
    mnuLoadAndZoom: TMenuItem;
    LoadPopup: TPopupMenu;
    ToolButton1: TToolButton;
    LoadButton: TToolButton;
    SaveButton: TToolButton;
    procedure actAddExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actLoadGPXExecute(Sender: TObject);
    procedure actSaveGPXExecute(Sender: TObject);
    procedure CollectionListBoxClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetMapView: TMapView;
    procedure Modified;
    procedure LoadFromFile(AFileName: String; ALayer: TMapLayer; ZoomToBounds: Boolean);
    procedure SaveToFile(AFileName: String; ALayer: TMapLayer);
    procedure SetStrings;
    procedure UpdateVisuals;
  public
    procedure UpdateButtons;

  end;

var
  LayersPropertyEditForm: TLayersPropertyEditForm;

implementation

{$R *.lfm}

{ TLayersPropertyEditForm }

procedure TLayersPropertyEditForm.CollectionListBoxClick(Sender: TObject);
begin
  inherited;
  UpdateVisuals;
end;

procedure TLayersPropertyEditForm.FormActivate(Sender: TObject);
begin
  UpdateVisuals;
end;

procedure TLayersPropertyEditForm.FormCreate(Sender: TObject);
begin
  inherited;
  SetStrings;
  LoadButton.ImageIndex := IDEImages.LoadImage('items_load');
  actSaveGPX.ImageIndex := IDEImages.LoadImage('items_save');
  UpdateVisuals;
end;

function TLayersPropertyEditForm.GetMapView: TMapView;
begin
  Result := OwnerPersistent as TMapView;
end;

procedure TLayersPropertyEditForm.actAddExecute(Sender: TObject);
var
  L: TMapLayer;
begin
  if not Assigned(GlobalDesignHook) then
    Exit;
  if not Assigned(Collection) then
    Exit;

  L := Collection.Add as TMapLayer;
  GlobalDesignHook.PersistentAdded(L, true);
  CollectionListbox.ItemIndex := L.Index;

  UpdateVisuals;
end;

procedure TLayersPropertyEditForm.actDelExecute(Sender: TObject);
var
  I: Integer;
  L: TMapLayer;
begin
  if not Assigned(GlobalDesignHook) then
    exit;
  if not Assigned(Collection) then
    exit;

  I := CollectionListbox.ItemIndex;
  if I = -1 then
    exit;

  L := Collection.Items[I] as TMapLayer;
  GlobalDesignHook.DeletePersistent(TPersistent(L));

  //inherited;

  UpdateVisuals;
end;

procedure TLayersPropertyEditForm.actLoadGPXExecute(Sender: TObject);
var
  I: Integer;
  L: TMapLayer;
begin
  if not Assigned(Collection) then
    Exit;

  I := CollectionListBox.ItemIndex;
  if I = -1 then
    L := Collection.Add as TMapLayer
  else
    L := Collection.Items[I] as TMapLayer;
  with TOpenDialog.Create(Nil) do
    try
      DefaultExt := '.gpx';
      Filter := mvRS_GPXFileFilter;
      if Execute then
        LoadFromFile(FileName, L, Sender=actLoadAndZoom);
    finally
      Free;
    end;
end;

procedure TLayersPropertyEditForm.actSaveGPXExecute(Sender: TObject);
var
  I: Integer;
  L: TMapLayer;
begin
  if not Assigned(Collection) then
    Exit;
  I := CollectionListBox.ItemIndex;
  L := Collection.Items[I] as TMapLayer;
  with TSaveDialog.Create(Nil) do
    try
      DefaultExt := '.gpx';
      Filter := mvRS_GPXFileFilter;
      Options := Options + [ofOverwritePrompt, ofEnableSizing];
      FileName := L.Caption;
      if Execute then
        SaveToFile(FileName, L);
    finally
      Free;
    end;
end;

procedure TLayersPropertyEditForm.UpdateVisuals;
begin
  actLoadGPX.Enabled := (Collection <> Nil) and (CollectionListbox.ItemIndex > -1) and GetMapView.Active; {and btnLoad.Visible};
  actLoadAndZoom.Enabled := actLoadGPX.Enabled;
  actSaveGPX.Enabled := actLoadGPX.Enabled;
  LoadButton.Enabled := actLoadGPX.Enabled or actLoadAndZoom.Enabled;
end;

procedure TLayersPropertyEditForm.Modified;
begin
  if GlobalDesignHook <> nil then
    GlobalDesignHook.Modified(Self);
end;

procedure TLayersPropertyEditForm.SaveToFile(AFileName: String; ALayer: TMapLayer);
begin
  TGpxWriter.SaveToFile(AFileName, ALayer.ComboLayer);
end;

procedure TLayersPropertyEditForm.SetStrings;
begin
  LoadButton.Caption := mvRS_LoadGPX;
  actLoadGPX.Caption := mvRS_Load;
  actLoadAndZoom.Caption := mvRS_LoadAndZoom;
  actSaveGPX.Caption := mvRS_SaveGPX;
end;

procedure TLayersPropertyEditForm.LoadFromFile(AFileName: String;
  ALayer: TMapLayer; ZoomToBounds: Boolean);
const
  DO_NOT_CLEAR = false;
var
  List: TGpsObjectList;
  lBounds: TRealArea;
  tracks, pois: TFPList;
  T: TMapTrack;
  P: TMapPoint;
begin
  with TGpxReader.Create do
    try
      List := TGPSObjectList.Create;
      try
        LoadFromFile(AFileName, List, lBounds);
        if ZoomToBounds then
          with GetMapView do
          begin
            Engine.ZoomOnArea(lBounds);
            MapCenter.RealPt := Center;
          end;

        // Update the object tree
        tracks := TFPList.Create;
        pois := TFPList.Create;
        try
          ALayer.AssignFromGPSList(List, DO_NOT_CLEAR, tracks, pois);
          if GlobalDesignHook <> nil then
          begin
            // Add the track and its points to the object tree
            // (significant speed-up by explicitely adding only the last track point)
            if tracks.Count > 0 then
            begin
              T := TMapTrack(tracks[tracks.Count-1]);
              if T.Points.Count > 0 then
                GlobalDesignHook.PersistentAdded(TMapTrackPoint(T.Points[T.Points.Count-1]), false);
            end;
            // Add the last map point to the object tree.
            if pois.Count > 0 then
            begin
              P := TMapPoint(pois[pois.Count-1]);
              GlobalDesignHook.PersistentAdded(P, false);
            end;
          end;
        finally
          pois.Free;
          tracks.Free;
        end;
      finally
        List.Free;
      end;
    finally
      Free;
    end;
  Modified;
end;

// wp: UpdateButtons should be virtual...
procedure TLayersPropertyEditForm.UpdateButtons;
begin
  inherited;  // Make it "public" in Laz 2.0.x
end;

end.

