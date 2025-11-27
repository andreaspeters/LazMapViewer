unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, mvMapViewer, StdCtrls, SysUtils, Forms, Controls, Graphics,
  Dialogs;

type
  TMainForm = class(TForm)
    cbShowOverlay: TCheckBox;
    MapView: TMapView;
    Panel1: TPanel;
    procedure cbShowOverlayChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  mvGPSObj, mvDrawingEngine;

const
  _TILELAYERS_ID_ = 42;

procedure TMainForm.FormCreate(Sender: TObject);
var
  layer: TGPSTileLayer;
begin
  // Prepare the MapView
  MapView.MapProvider := 'OpenStreetMap Standard';
  MapView.Zoom := 5;
  MapView.MapCenter.Longitude := 10;
  MapView.MapCenter.Latitude := 49;
  MapView.Active := true;

  // Create a GPSTileLayer and add it
  layer := TGPSTilelayer.Create;
  layer.MapProvider := 'OpenRailwayMap Standard';
  layer.DrawMode := idmUseSourceAlpha;
  MapView.GPSLayer[0].Add(layer, _TILELAYERS_ID_);
end;

procedure TMainForm.cbShowOverlayChange(Sender: TObject);
begin
  MapView.GPSLayer[0].Visible := cbShowOverlay.Checked;
  MapView.Invalidate;
end;

end.

