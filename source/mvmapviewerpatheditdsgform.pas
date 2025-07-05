unit mvMapViewerPathEditDsgForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  PropEdits, mvMapViewer, mvMapViewerPathEditForm;

type

  { TMapViewerPathEditDsgForm }

  TMapViewerPathEditDsgForm = class(TMapViewerPathEditForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure OnModified(Sender: TObject);
    procedure OnModifiedWithName(Sender: TObject; {%H-}PropName: ShortString);
    procedure OnRefreshPropertyValues;
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
  protected
    procedure PersistentAdded(APersistent: TPersistent; Select: Boolean);
      override;
    procedure DeletePersistent(APersistent: TPersistent); override;
    procedure UnselectPersistent(APersistent: TPersistent); override;
    procedure ObjectModified(AObject: TObject; PropName: ShortString = '');
      override;
    procedure SelectInOI(AView: TMapView; ForceUpdate: Boolean); override;
  public

  end;

var
  MapViewerPathEditDsgForm: TMapViewerPathEditDsgForm;

implementation

{$R *.lfm}

{ TMapViewerPathEditDsgForm }

procedure TMapViewerPathEditDsgForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  MapView := Nil;
end;

procedure TMapViewerPathEditDsgForm.FormCreate(Sender: TObject);
begin
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.AddHandlerModified(@OnModified);
    GlobalDesignHook.AddHandlerModifiedWithName(@OnModifiedWithName);
    GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnRefreshPropertyValues);
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  end;
end;

procedure TMapViewerPathEditDsgForm.FormDestroy(Sender: TObject);
begin
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TMapViewerPathEditDsgForm.FormShow(Sender: TObject);
begin
  if Assigned(MapView) then
    MapView.ControlStyle := MapView.ControlStyle + [csDesignInteractive]
end;

procedure TMapViewerPathEditDsgForm.OnModified(Sender: TObject);
begin
  ;
end;

procedure TMapViewerPathEditDsgForm.OnModifiedWithName(Sender: TObject;
  PropName: ShortString);
begin
  ;
end;

procedure TMapViewerPathEditDsgForm.OnRefreshPropertyValues;
begin
  ;
end;

procedure TMapViewerPathEditDsgForm.OnSetSelection(
  const ASelection: TPersistentSelectionList);
var
  I: Integer;
  V: TMapView = Nil;
  PtCnt: Integer = 0;
  P: TMapPoint = Nil;
begin
  // Try to find the the containing map view
  for I := 0 to Pred(ASelection.Count) do
  begin
    V := TMapView(GetOwnerOfType(ASelection[I], TMapView));
    if Assigned(V) then
      Break;
  end;
  if Assigned(V) then
  begin
    MapView := V;
    for I := 0 to Pred(ASelection.Count) do
    begin
      // If not internal select (i.e. from designer tree view)
      if not InternalSelect and (ASelection[I] is TMapPoint) then
      begin
        Inc(PtCnt);
        if not Assigned(P) then
        begin
          P := TMapPoint(ASelection[I]);
          V.EditMark.Select(P, True); // TODO Make it visible?
        end
        else
          V.EditMark.Select(TMapPoint(ASelection[I]), False);
      end;
    end;
    // From the designer and no points into?
    if not InternalSelect and not Assigned(P) then
      V.EditMark.ClearSelection;
  end;
  UpdateControls;
end;

procedure TMapViewerPathEditDsgForm.PersistentAdded(APersistent: TPersistent;
  Select: Boolean);
begin
  inherited PersistentAdded(APersistent, Select);
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.PersistentAdded(APersistent, Select);
end;

procedure TMapViewerPathEditDsgForm.DeletePersistent(APersistent: TPersistent);
begin
  //inherited DeletePersistent(APersistent);
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.DeletePersistent(APersistent);
end;

procedure TMapViewerPathEditDsgForm.UnselectPersistent(APersistent: TPersistent
  );
begin
  inherited UnselectPersistent(APersistent);
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.Unselect(APersistent);
end;

procedure TMapViewerPathEditDsgForm.ObjectModified(AObject: TObject;
  PropName: ShortString);
begin
  inherited ObjectModified(AObject, PropName);
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.Modified(AObject, PropName);
end;

procedure TMapViewerPathEditDsgForm.SelectInOI(AView: TMapView;
  ForceUpdate: Boolean);
var
  I: Integer = 0;
  Sel: TPersistentSelectionList;
  P: TMapPoint;
begin
  if not Assigned(GlobalDesignHook) or not Assigned(AView) then
    Exit;
  if AView.EditMark.Selection.Count > 0 then
  begin
    // select in OI
    InternalSelect := True;
    Sel := TPersistentSelectionList.Create;
    Sel.ForceUpdate := ForceUpdate;
    try
      for P in AView.EditMark.Selection.Points do
      begin
        Sel.Add(P);
        Inc(I);
      end;
      GlobalDesignHook.SetSelection(Sel);
    finally
      Sel.Free;
      InternalSelect := False;
    end;
  end;
  inherited SelectInOI(AView, ForceUpdate);
end;


end.

