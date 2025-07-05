unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLIntf, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Dialogs, Spin,
  mvMapViewer, mvPluginCommon, mvPlugins;

type
  TMainForm = class(TForm)
    FormCenterBevel: TBevel;
    btnSaveToImage: TButton;
    cbShowMapCenter: TCheckBox;
    cbShowLegalNotice: TCheckBox;
    cmbPosition: TComboBox;
    edLegalNotice: TEdit;
    rbLeftMap: TRadioButton;
    rbRightMap: TRadioButton;
    seOpacity: TSpinEdit;
    lblLegalNotice: TLabel;
    lblOpacity: TLabel;
    ParamsPanel: TPanel;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    procedure btnSaveToImageClick(Sender: TObject);
    procedure cbShowMapCenterChange(Sender: TObject);
    procedure cbShowLegalNoticeChange(Sender: TObject);
    procedure cmbPositionChange(Sender: TObject);
    procedure edLegalNoticeChange(Sender: TObject);
    procedure rbLeftMapChange(Sender: TObject);
    procedure seOpacityChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMapView1: TMapView;
    FMapView2: TMapView;
    FPluginManager: TMvPluginManager;
    FLegalNoticePlugin1: TLegalNoticePlugin;
    FLegalNoticePlugin2: TLegalNoticePlugin;
    FCenterMarkerPlugin: TCenterMarkerPlugin;
    FLinkedMapsPlugin: TLinkedMapsPlugin;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPluginManager := TMvPluginManager.Create(Self);

  FMapView1 := TMapView.Create(self);
  FMapView1.Align := alClient;
  FMapView1.Parent := LeftPanel;
  FMapView1.MapProvider := 'OpenStreetMap Standard';
  FMapView1.UseThreads := true;
  FMapView1.Zoom := 9;
  FMapView1.MapCenter.Longitude := 11;
  FMapView1.MapCenter.Latitude := 46.3;
  FMapView1.Active := true;
  FMapView1.PluginManager := FPluginManager;

  FMapView2 := TMapView.Create(self);
  FMapView2.Align := alClient;
  FMapView2.Parent := RightPanel;
  FMapView2.MapProvider := 'Maps for free';
  FMapView2.UseThreads := true;
  FMapView2.Zoom := 9;
  FMapView2.MapCenter.Longitude := 11;
  FMapView2.MapCenter.Latitude := 46.3;
  FMapView2.Active := true;
  FMapView2.PluginManager := FPluginManager;

  FLegalNoticePlugin1 := TLegalNoticePlugin.Create(FPluginManager);
  with FLegalNoticePlugin1 do
  begin
    LegalNotice := 'Map data from [https://www.openstreetmap.org/copyright OpenStreetMap and contributors]';
    Spacing := 0;
    Font.Size := 8;
    BackgroundColor := clWhite;
    MapView := FMapView1;

    edLegalNotice.Text := LegalNotice;
    seOpacity.Value := round(BackgroundOpacity * 100);
  end;

  FLegalNoticePlugin2 := TLegalNoticePlugin.Create(FPluginManager);
  with FLegalNoticePlugin2 do
  begin
    LegalNotice := '(c) [https://maps-for-free.com/html/about.html maps-for-free]';
    Spacing := 0;
    Font.Size := 8;
    Font.Color := clBlue;
    BackgroundColor := clWhite;
    MapView := FMapView2;
  end;

  FCenterMarkerPlugin := TCenterMarkerPlugin.Create(FPluginManager);
  with FCenterMarkerPlugin do
  begin
    Size := 15;
    Pen.Width := 3;
    Pen.Color := clRed;
  end;

  FLinkedMapsPlugin := TLinkedMapsPlugin.Create(FPluginManager);
end;

procedure TMainForm.edLegalNoticeChange(Sender: TObject);
begin
  if rbLeftMap.Checked then
    FLegalNoticePlugin1.LegalNotice := edLegalNotice.Text;
  if rbRightMap.Checked then
    FLegalNoticePlugin2.LegalNotice := edLegalNotice.Text;
end;

procedure TMainForm.rbLeftMapChange(Sender: TObject);
begin
  if rbLeftMap.Checked then
    edLegalNotice.Text := FLegalNoticePlugin1.LegalNotice;
  if rbRightMap.Checked then
    edLegalNotice.Text := FLegalNoticePlugin2.LegalNotice;
end;

procedure TMainForm.seOpacityChange(Sender: TObject);
begin
  if rbLeftMap.Checked then
    FLegalNoticePlugin1.BackgroundOpacity := seOpacity.Value / 100;
  if rbRightMap.Checked then
    FLegalNoticePlugin2.BackgroundOpacity := seOpacity.Value / 100;
end;

procedure TMainForm.btnSaveToImageClick(Sender: TObject);
begin
  if rbLeftMap.Checked then
    FMapView1.SaveToFile(TPortableNetworkGraphic, 'map1.png');
  if rbRightMap.Checked then
    FMapView2.SaveToFile(TPortableNetworkGraphic, 'map2.png');
end;

procedure TMainForm.cbShowMapCenterChange(Sender: TObject);
begin
  FCenterMarkerPlugin.Enabled := cbShowMapCenter.Checked;
end;

procedure TMainForm.cbShowLegalNoticeChange(Sender: TObject);
begin
  if rbLeftMap.Checked then
    FLegalNoticePlugin1.Enabled := cbShowLegalNotice.Checked;
  if rbRightMap.Checked then
    FLegalNoticePlugin2.Enabled := cbShowLegalNotice.Checked;
end;

procedure TMainForm.cmbPositionChange(Sender: TObject);
begin
  if rbLeftMap.Checked then
    FLegalNoticePlugin1.Position := TLegalNoticePosition(cmbPosition.ItemIndex);
  if rbRightMap.Checked then
    FLegalNoticePlugin2.Position := TLegalNoticePosition(cmbPosition.ItemIndex);
end;

end.

