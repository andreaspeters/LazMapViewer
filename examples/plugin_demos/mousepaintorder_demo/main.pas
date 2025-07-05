{ This example will create 9 DragColorPlugins and allow the user to drag the
  items with the right mouse button down.
  The Items show different MouseCursors to identify the options
  It also allows the change of the size using the mouse wheel.
}
unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvPluginCommon, uDragColoredItemPlugin;

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckBox1: TCheckBox;
    MapView: TMapView;
    MvPluginManager: TMvPluginManager;
    Panel1: TPanel;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }
const
  PluginCount = 9;
  PluginColors : array[0..PluginCount-1] of TColor = (
    clFuchsia,clRed,$ff8c00,clYellow,clLime,clGreen,clNavy,clBlue,clAqua
  );
  PlugInMouseCursors : array[0..PluginCount-1] of TCursor = (
    crCross,crDrag, crNoDrop, crHSplit,
    crVSplit, crMultiDrag, crSQLWait, crNo,
    crSize
  );

procedure TMainForm.FormCreate(Sender: TObject);
var
  lDragColoredItemPlugin : TDragColoredItemPlugin;
  i: Integer;
begin
  MapView.Active := true;
  for i := 0 to High(PluginColors) do
  begin
    lDragColoredItemPlugin := TDragColoredItemPlugin.Create(Self);
    lDragColoredItemPlugin.Color := PluginColors[i];
    lDragColoredItemPlugin.MouseCursor := PlugInMouseCursors[i];
    lDragColoredItemPlugin.ShowCaption := CheckBox1.Checked;
    lDragColoredItemPlugin.MapView := MapView;
    MvPluginManager.AddPlugin(lDragColoredItemPlugin);
  end;
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to MvPluginManager.Count-1 do
  begin
    if MvPluginManager.Items[i] is TDragColoredItemPlugin then
      TDragColoredItemPlugin(MvPluginManager.Items[i]).ShowCaption:= CheckBox1.Checked;
  end;
  MapView.Invalidate;
end;

end.

