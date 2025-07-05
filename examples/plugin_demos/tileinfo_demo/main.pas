unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvPluginCommon, mvPlugins;

type

  { TMainForm }

  TMainForm = class(TForm)
    cbShowTileInfo: TCheckBox;
    cbShowCenter: TCheckBox;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    CenterMarkerPlugin: TCenterMarkerPlugin;
    TileInfoPlugin: TTileInfoPlugin;
    ParamsPanel: TPanel;
    procedure cbShowCenterChange(Sender: TObject);
    procedure cbShowTileInfoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView.Active := true;
end;

procedure TMainForm.cbShowTileInfoChange(Sender: TObject);
begin
  TileInfoPlugin.Enabled := cbShowTileInfo.Checked;
end;

procedure TMainForm.cbShowCenterChange(Sender: TObject);
begin
  CenterMarkerPlugin.Enabled := cbShowCenter.Checked;
end;

end.

