unit main;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, SysUtils, Math,
  Graphics, Controls, StdCtrls, ExtCtrls, Forms,
  mvMapViewer, mvPluginCommon, mvPlugins;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList: TImageList;
    Label1: TLabel;
    MapView1: TMapView;
    MapView2: TMapView;
    Panel1: TPanel;
    PluginManager: TMvPluginManager;
    LinkedMapsPlugin: TLinkedMapsPlugin;
    btnZoomIn: TSpeedButton;
    btnZoomOut: TSpeedButton;
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapView1ZoomChange(Sender: TObject);
  private
    procedure UpdateZoomBtns;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView1.Active := true;
  MapView2.Active := true;
  UpdateZoomBtns;
end;

procedure TMainForm.btnZoomInClick(Sender: TObject);
begin
  MapView1.Zoom := MapView1.Zoom + 1;
end;

procedure TMainForm.btnZoomOutClick(Sender: TObject);
begin
  MapView1.Zoom := MapView1.Zoom - 1;
end;

procedure TMainForm.MapView1ZoomChange(Sender: TObject);
begin
  UpdateZoomBtns;
end;

procedure TMainForm.UpdateZoomBtns;
begin
  btnZoomOut.Enabled := (MapView1.Zoom > MapView1.ZoomMin);
  btnZoomIn.Enabled := (MapView1.Zoom < MapView1.ZoomMax);
  btnZoomOut.Hint := Format('Zoom out (Current zoom level: %d)', [MapView1.Zoom]);
  btnZoomIn.Hint := Format('Zoom in (Current zoom level: %d)', [MapView1.Zoom]);
end;

end.

