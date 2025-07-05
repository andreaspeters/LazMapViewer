unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, mvMapViewer, mvPluginCommon, mvPlugins, StdCtrls, SysUtils,
  Forms, Controls, Graphics, Dialogs;

type
  TForm1 = class(TForm)
    CenterBevel: TBevel;
    btnMove: TButton;
    MapView_Left: TMapView;
    MapView_Right: TMapView;
    MvPluginManager_Left: TMvPluginManager;
    CenterMarkerPlugin: TCenterMarkerPlugin;
    MvPluginManager_Right: TMvPluginManager;
    procedure btnMoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.btnMoveClick(Sender: TObject);
begin
  if CenterMarkerPlugin.PluginManager = MvPluginManager_Left then
    CenterMarkerPlugin.PluginManager := MvPluginManager_Right
  else
    CenterMarkerPlugin.PluginManager := MvPluginManager_Left;
  MapView_Left.Invalidate;
  MapView_Right.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MapView_Left.Active := true;
  MapView_Right.Active := true;
end;

end.

