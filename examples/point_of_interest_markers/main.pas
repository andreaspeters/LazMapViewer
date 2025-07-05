unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, mvMapViewer, mvPluginCommon, mvPlugins, StdCtrls, SysUtils, Forms,
  Controls, Graphics, Dialogs, mvTypes;

type
  TMainForm = class(TForm)
    cbAllowDragging: TCheckBox;
    MarkerImages: TImageList;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    LegalNoticePlugin_Icons: TLegalNoticePlugin;
    LegalNoticePlugin_Map: TLegalNoticePlugin;
    procedure cbAllowDraggingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  L: TMapLayer;
  P: TMapPointOfInterest;
begin
  MapView.Active := true;
  L := MapView.Layers.Add as TMapLayer;

  P := L.PointsOfInterest.Add as TMapPointOfInterest;
  // or: P := TMapPointOfInterest.Create(L.PointsOfInterest);
  P.RealPoint := RealPoint(68, 111);
  P.ImageIndex := 0;
  P.ImageAnchorX := 0;
  P.ImageAnchorY := 100;
  P.TextPositionHor := tphLeft;
  P.Caption := 'Tilted Pin';

  P := L.PointsOfInterest.Add as TMapPointOfInterest;
  P.RealPoint := RealPoint(19, -45);
  P.ImageIndex := 2;
  P.Caption := 'Vertical Pin';

  P := L.PointsOfInterest.Add as TMapPointOfInterest;
  P.RealPoint := RealPoint(0.0, 0.0);
  P.ImageIndex := 1;
  P.Caption := 'Marker';

  P := L.PointsOfInterest.Add as TMapPointOfInterest;
  P.RealPoint := RealPoint(0.0, -120.0);
  P.ImageIndex := 3;
  P.ImageAnchorX := 27;
  P.ImageAnchorY := 0;
  P.TextPositionHor := tphLeft;
  P.TextPositionVert := tpvAbove;
  P.Caption := 'Safety Pin';
end;

procedure TMainForm.cbAllowDraggingChange(Sender: TObject);
begin
  if cbAllowDragging.Checked then
    MapView.Options := MapView.Options + [mvoEditorEnabled]
  else
    MapView.Options := MapView.Options - [mvoEditorEnabled];
end;

end.

