unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  mvMapViewer, mvMapProvider, mvPluginCommon, mvMapScalePlugin;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnFont: TButton;
    cbScaleVisible: TCheckBox;
    clbBackgroundColor: TColorButton;
    clbPenColor: TColorButton;
    FontDialog: TFontDialog;
    gbFont: TGroupBox;
    gbBackground: TGroupBox;
    gbZoomMin: TGroupBox;
    gbPen: TGroupBox;
    lblFontSample: TLabel;
    lblPenWidth: TLabel;
    lblOpacity: TLabel;
    lblZoomMinInfo: TLabel;
    lblCurrentZoom: TLabel;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    ParamsPanel: TPanel;
    rgLengthUnits: TRadioGroup;
    rgScaleAlign: TRadioGroup;
    seOpacity: TFloatSpinEdit;
    seZoomMin: TSpinEdit;
    sePenWidth: TSpinEdit;
    procedure btnFontClick(Sender: TObject);
    procedure cbScaleVisibleChange(Sender: TObject);
    procedure clbBackgroundColorChanged(Sender: TObject);
    procedure clbPenColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapViewZoomChange(Sender: TObject);
    procedure rgLengthUnitsClick(Sender: TObject);
    procedure rgScaleAlignClick(Sender: TObject);
    procedure seOpacityChange(Sender: TObject);
    procedure sePenWidthChange(Sender: TObject);
    procedure seZoomMinChange(Sender: TObject);
  private
    FScalePlugin: TMapScalePlugin;
    procedure UpdateZoomInfo;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  P: TMapProvider;
  zoomMin, zoomMax: Integer;
begin
  MapView.Active := true;
  FScalePlugin := TMapScalePlugin.Create(PluginManager);

  P := MapView.Engine.MapProviderByName(MapView.MapProvider);
  P.GetZoomInfos(zoomMin, zoomMax);
  seZoomMin.MaxValue := zoomMax;
  seZoomMin.MinValue := zoomMin;
  seZoomMin.Value := FScalePlugin.ZoomMin;
  sePenWidth.Value := FScalePlugin.Pen.Width;
  seOpacity.Value := FScalePlugin.BackgroundOpacity;
  clbPenColor.ButtonColor := FScalePlugin.Pen.Color;
  clbBackgroundColor.ButtonColor := FScalePlugin.BackgroundColor;
  lblFontSample.Font.Assign(FScalePlugin.Font);

  UpdateZoomInfo;
end;

procedure TMainForm.btnFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(FScalePlugin.Font);
  if FontDialog.Execute then
  begin
    FScalePlugin.Font.Assign(FontDialog.Font);
    lblFontSample.Font.Assign(FontDialog.Font);
  end;
end;

procedure TMainForm.cbScaleVisibleChange(Sender: TObject);
begin
  FScalePlugin.Enabled := cbScaleVisible.Checked;
end;

procedure TMainForm.clbBackgroundColorChanged(Sender: TObject);
begin
  FScalePlugin.BackgroundColor := clbBackgroundColor.ButtonColor;
end;

procedure TMainForm.clbPenColorChanged(Sender: TObject);
begin
  FScalePlugin.Pen.Color := clbPenColor.ButtonColor;
end;

procedure TMainForm.rgLengthUnitsClick(Sender: TObject);
begin
  FScalePlugin.Imperial := rgLengthUnits.ItemIndex = 1;
end;

procedure TMainForm.MapViewZoomChange(Sender: TObject);
begin
  UpdateZoomInfo;
end;

procedure TMainForm.rgScaleAlignClick(Sender: TObject);
var
  alignSet: TScaleAlignSet;
begin
  alignSet := [];
  case rgScaleAlign.ItemIndex of
    0: alignSet := [alLeft, alTop];
    1: alignSet := [alLeft, alRight, alTop];
    2: alignSet := [alRight, alTop];
    3: alignSet := [alLeft, alTop, alBottom];
    4: alignSet := [alLeft, alRight, alTop, alBottom];
    5: alignSet := [alRight, alTop, alBottom];
    6: alignSet := [alLeft, alBottom];
    7: alignSet := [alLeft, alRight, alBottom];
    8: alignSet := [alRight, alBottom];
  end;
  FScalePlugin.AlignSet := alignSet;
end;

procedure TMainForm.seOpacityChange(Sender: TObject);
begin
  FScalePlugin.BackgroundOpacity := seOpacity.Value;
end;

procedure TMainForm.sePenWidthChange(Sender: TObject);
begin
  FScalePlugin.Pen.Width := sePenWidth.Value;
end;

procedure TMainForm.seZoomMinChange(Sender: TObject);
begin
  FScalePlugin.ZoomMin := seZoomMin.Value;
end;

procedure TMainForm.UpdateZoomInfo;
begin
  lblCurrentZoom.Caption := Format('Current zoom level %d', [MapView.Zoom]);
end;


end.

