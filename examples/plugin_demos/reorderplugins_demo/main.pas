unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs, ValEdit,
  mvMapViewer, mvPluginCommon, mvPlugins;

type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1LegalNoticePlugin1: TLegalNoticePlugin;
    MvPluginManager1LegalNoticePlugin2: TLegalNoticePlugin;
    MvPluginManager1LegalNoticePlugin3: TLegalNoticePlugin;
    Panel1: TPanel;
    ValueListEditor1: TValueListEditor;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdatePluginList;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  i, idx, n: Integer;
begin
  if Combobox1.ItemIndex = -1 then
    exit;

  n := MvPluginManager1.PlugInList.Count;
  idx := -1;
  for i := 0 to n-1 do
    if (MvPluginManager1.PluginList[i] is TLegalNoticePlugin) and
       (TLegalNoticePlugin(MvPluginManager1.PluginList[i]).LegalNotice = Combobox1.Items[Combobox1.ItemIndex]) then
    begin
      idx := i;
      break;
    end;

  if idx = -1 then
    exit;

   MvPluginManager1.PluginList.Move(idx, n-1);
   // or:
   //  MvPluginManager1.PluginList[i].Index := n-1;
  UpdatePluginList;

  MapView1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  MapView1.Active := true;
  Combobox1.Items.Clear;
  for i := 0 to MvPluginManager1.PluginList.Count-1 do
    Combobox1.Items.Add(TLegalNoticePlugin(MvPluginManager1.PluginList[i]).LegalNotice);
  Combobox1.ItemIndex := Combobox1.Items.Count-1;

  UpdatePluginList;
end;

procedure TForm1.UpdatePluginList;
var
  i, n: Integer;
  txt: String;
begin
  ValueListeditor1.Clear;
  n := MvPluginManager1.PluginList.Count;
  for i := n-1 downto 0 do
  begin
    txt := IntToStr(i);
    if i = 0 then txt := txt + ' (bottom)' else if i = n-1 then txt := txt + ' (top)';
    ValueListEditor1.InsertRow(txt, TLegalNoticePlugin(MvPluginManager1.PluginList[i]).LegalNotice, true);
  end;
end;

end.

