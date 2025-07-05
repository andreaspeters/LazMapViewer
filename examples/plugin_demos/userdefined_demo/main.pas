unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvPluginCommon, mvPlugins;

type

  { TMainForm }

  TMainForm = class(TForm)
    Label1: TLabel;
    lblMessageLabel: TLabel;
    lblUserdefinedPluginMessage: TLabel;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    InfoPanel: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    procedure UserdefinedPluginMouseUp(Sender: TObject; AMapView: TMapView; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
    procedure UserdefinedPluginMouseDown(Sender: TObject; AMapView: TMapView; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  udp : TUserDefinedPlugin;
begin
  MapView.Active := true;

  udp := TUserdefinedPlugin.Create(PluginManager);
  udp.OnMouseUp := @UserdefinedPluginMouseUp;
  udp.OnMouseDown := @UserdefinedPluginMouseDown;
end;

procedure TMainForm.UserdefinedPluginMouseUp(Sender: TObject; AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
begin
  lblUserdefinedPluginMessage.Caption := Format('MouseUp X:%d Y:%d',[X,Y]);
end;

procedure TMainForm.UserdefinedPluginMouseDown(Sender: TObject; AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
begin
  lblUserdefinedPluginMessage.Caption := Format('MouseDown X:%d Y:%d',[X,Y]);
end;

end.

