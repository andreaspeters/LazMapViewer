unit ConfigFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  Graphics, Forms, Controls, StdCtrls, Buttons, Dialogs, ExtCtrls,
  ColorBox, ExtDlgs, Spin, Types,
  mvTypes, mvMapViewer, mvEngine, mvPluginCommon, mvMapGridPlugin, mvMapScalePlugin,
  globals;

type
  { TCfgFrame }

  TCfgFrame = class(TFrame)
    Bevel1: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    btnLoadMapProviders: TSpeedButton;
    btnPOITextFont: TButton;
    btnSaveMapProviders: TSpeedButton;
    btnSelectPOIImage: TButton;
    cbCyclicView: TCheckBox;
    cbDebugTiles: TCheckBox;
    cbDoubleBuffer: TCheckBox;
    cbPOITextBgColor: TColorBox;
    cbPreviewTiles: TCheckBox;
    cbMapScale: TCheckBox;
    cbMapGrid: TCheckBox;
    cmbProviders: TComboBox;
    cbUseThreads: TCheckBox;
    cbZoomToCursor: TCheckBox;
    clbBackColor: TColorButton;
    cmbTileSize: TComboBox;
    edProxyHost: TEdit;
    edProxyPassword: TEdit;
    edProxyUserName: TEdit;
    FontDialog: TFontDialog;
    gbProxy: TGroupBox;
    ImageList: TImageList;
    infoTileSize: TLabel;
    lblTileSize: TLabel;
    LblPOITextBgColor: TLabel;
    LblProviders: TLabel;
    lblProxyHost: TLabel;
    lblProxyPassword: TLabel;
    lblProxyPort: TLabel;
    lblProxyUserName: TLabel;
    PluginManager: TMvPluginManager;
    MapGridPlugin: TMapGridPlugin;
    MapScalePlugin: TMapScalePlugin;
    OpenPictureDialog: TOpenPictureDialog;
    rbNoProxy: TRadioButton;
    rbProxyData: TRadioButton;
    rbSystemProxy: TRadioButton;
    rgPOIMode: TRadioGroup;
    seProxyPort: TSpinEdit;
    procedure btnLoadMapProvidersClick(Sender: TObject);
    procedure btnPOITextFontClick(Sender: TObject);
    procedure btnSaveMapProvidersClick(Sender: TObject);
    procedure btnSelectPOIImageClick(Sender: TObject);
    procedure cbCyclicViewChange(Sender: TObject);
    procedure cbDebugTilesChange(Sender: TObject);
    procedure cbDoubleBufferChange(Sender: TObject);
    procedure cbMapGridChange(Sender: TObject);
    procedure cbMapScaleChange(Sender: TObject);
    procedure cbPOITextBgColorChange(Sender: TObject);
    procedure cbPreviewTilesChange(Sender: TObject);
    procedure cbUseThreadsChange(Sender: TObject);
    procedure cbZoomToCursorChange(Sender: TObject);
    procedure clbBackColorColorChanged(Sender: TObject);
    procedure cmbProvidersChange(Sender: TObject);
    procedure cmbTileSizeChange(Sender: TObject);
    procedure rbNoProxyChange(Sender: TObject);
    procedure rgPOIModeClick(Sender: TObject);
    procedure rbProxyDataChange(Sender: TObject);
    procedure rbSystemProxyChange(Sender: TObject);
  private
    FMapView: TMapView;
    FPOIImage: TCustomBitmap;
    FDrawGPSPointEvent: TDrawGpsPointEvent;
    FUpdateLayersEvent: TNotifyEvent;
    function GetMapProviders: TStrings;
    function GetPOIMode: TPOIMode;
    function GetPOITextBkColor: TColor;
    procedure SetMapView(AValue: TMapView);
  protected
    procedure DoUpdateLayers; virtual;
    procedure UpdateDownloadEngineProxy;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTileSizeInfo(ATileSize: TSize);

    procedure ReadFromIni(ini: TCustomIniFile); virtual;
    procedure WriteToIni(ini: TCustomIniFile); virtual;

    property MapView: TMapView read FMapView write SetMapView;
    property MapProviders: TStrings read GetMapProviders;
    property POIMode: TPOIMode read GetPOIMode;
    property POITextBkColor: TColor read GetPOITextBkColor;
    property OnDrawGPSPoint: TDrawGpsPointEvent read FDrawGPSPointEvent write FDrawGPSPointEvent;
    property OnUpdateLayers: TNotifyEvent read FUpdateLayersEvent write FUpdateLayersEvent;
  end;

implementation

{$R *.lfm}

{ TCfgFrame }

constructor TCfgFrame.Create(AOwner: TComponent);
begin
  inherited;
  FPOIImage := TPortableNetworkGraphic.Create;
  FPOIImage.LoadFromResourceName(HINSTANCE, 'mapmarker');
end;

destructor TCfgFrame.Destroy;
begin
  FPOIImage.Free;
  inherited;
end;

procedure TCfgFrame.btnLoadMapProvidersClick(Sender: TObject);
var
  fn: String;
  msg: String;
begin
  fn := Application.Location + MAP_PROVIDER_FILENAME;
  if FileExists(fn) then begin
    if MapView.Engine.ReadProvidersFromXML(fn, msg) then begin
      MapView.GetMapProviders(cmbProviders.Items);
      cmbProviders.ItemIndex := 0;
      MapView.MapProvider := cmbProviders.Text;
      // sgLayers.Columns[1].PickList.Assign(cmbProviders.Items);   // <<--- FIXME
    end else
      ShowMessage(msg);
  end;
end;

procedure TCfgFrame.btnPOITextFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(MapView.Font);
  if FontDialog.Execute then
    MapView.Font.Assign(FontDialog.Font);
end;

procedure TCfgFrame.btnSaveMapProvidersClick(Sender: TObject);
begin
  MapView.Engine.WriteProvidersToXML(Application.Location + MAP_PROVIDER_FILENAME);
end;

procedure TCfgFrame.btnSelectPOIImageClick(Sender: TObject);
begin
  OpenPictureDialog.Filter := 'PNG files|*.png|BMP files|*.bmp';
  OpenPictureDialog.DefaultExt := '.png';
  if OpenPictureDialog.Execute then
  begin
    FPOIImage.LoadFromFile(OpenPictureDialog.FileName);
    MapView.POIImage.Assign(FPOIImage);
  end;
end;

procedure TCfgFrame.cbCyclicViewChange(Sender: TObject);
begin
  MapView.Cyclic := cbCyclicView.Checked;
  DoUpdateLayers;
end;

procedure TCfgFrame.cbDebugTilesChange(Sender: TObject);
begin
  MapView.DebugTiles := CbDebugTiles.Checked;
  MapView.Invalidate;
end;

procedure TCfgFrame.cbDoubleBufferChange(Sender: TObject);
begin
  MapView.DoubleBuffered := cbDoubleBuffer.Checked;
end;

procedure TCfgFrame.cbMapGridChange(Sender: TObject);
begin
  MapGridPlugin.Enabled := cbMapGrid.Checked;
end;

procedure TCfgFrame.cbMapScaleChange(Sender: TObject);
begin
  MapScalePlugin.Enabled := cbMapScale.Checked;
end;

procedure TCfgFrame.cbPOITextBgColorChange(Sender: TObject);
begin
  MapView.POITextBgColor := cbPOITextBgColor.Selected;
end;

procedure TCfgFrame.cbPreviewTilesChange(Sender: TObject);
begin
  MapView.DrawPreviewTiles := cbPreviewTiles.Checked;
end;

procedure TCfgFrame.cbUseThreadsChange(Sender: TObject);
begin
  MapView.UseThreads := cbUseThreads.Checked;
  DoUpdateLayers;
end;

procedure TCfgFrame.clbBackColorColorChanged(Sender: TObject);
begin
  MapView.InactiveColor := clbBackColor.ButtonColor;
end;

procedure TCfgFrame.cbZoomToCursorChange(Sender: TObject);
begin
  MapView.ZoomToCursor := CbZoomToCursor.Checked;
end;

procedure TCfgFrame.cmbProvidersChange(Sender: TObject);
begin
  MapView.MapProvider := cmbProviders.Text;
end;

procedure TCfgFrame.cmbTileSizeChange(Sender: TObject);
begin
  MapView.TileSize := TMapTileSize(cmbTileSize.ItemIndex);
end;

procedure TCfgFrame.DoUpdateLayers;
begin
  if Assigned(FUpdateLayersEvent) then
    FUpdateLayersEvent(Self);
end;

function TCfgFrame.GetMapProviders: TStrings;
begin
  Result := cmbProviders.Items;
end;

function TCfgFrame.GetPOIMode: TPOIMode;
begin
  Result := TPOIMode(rgPOIMode.ItemIndex);
end;

function TCfgFrame.GetPOITextBkColor: TColor;
begin
  Result := cbPOITextBgColor.Selected;
end;

procedure TCfgFrame.ReadFromIni(ini: TCustomIniFile);
var
  n: Integer;
begin
  n := ini.ReadInteger('Proxy', 'UseProxy', 0);
  case n of
    0: rbNoProxy.Checked := true;
    1: rbSystemProxy.Checked := true;
    2: rbProxyData.Checked := true;
  end;
  edProxyHost.Text := ini.ReadString('Proxy', 'ProxyHost', edProxyHost.Text);
  seProxyPort.Value := ini.ReadInteger('Proxy', 'ProxyPort', seProxyPort.Value);
  edProxyUserName.Text := ini.ReadString('Proxy', 'ProxyName', edProxyUserName.Text);
  edProxyPassword.Text := ini.ReadString('Proxy', 'ProxyPassword', edProxyPassword.Text);

  UpdateDownloadEngineProxy;

  // The following parameters already have been read by the main form,
  // we still must update the frame controls.
  MapView.GetMapProviders(cmbProviders.Items);
  cmbProviders.ItemIndex := cmbProviders.Items.IndexOf(MapView.MapProvider);
  cmbTileSize.ItemIndex := ord(MapView.TileSize);
  clbBackColor.ButtonColor := MapView.InactiveColor;
end;

procedure TCfgFrame.rbNoProxyChange(Sender: TObject);
begin
  UpdateDownloadEngineProxy;
end;

procedure TCfgFrame.rbProxyDataChange(Sender: TObject);
begin
  UpdateDownloadEngineProxy;
end;

procedure TCfgFrame.rbSystemProxyChange(Sender: TObject);
begin
  UpdateDownloadEngineProxy;
end;

procedure TCfgFrame.rgPOIModeClick(Sender: TObject);
begin
  case rgPOIMode.ItemIndex of
    0: begin  // default symbol
         if MapView.POIImage <> nil then MapView.POIImage.Clear;
         MapView.OnDrawGPSPoint := nil;
       end;
    1: begin  // default POI image
         MapView.POIImage := FPOIImage;
         MapView.OnDrawGPSPoint := nil;
       end;
    2: begin // image from image list, to be applied to the next POI only
         //
       end;
    3: begin  // custom-drawn symbol
         if MapView.POIImage <> nil then MapView.POIImage.Clear;
         MapView.OnDrawGPSPoint := FDrawGpsPointEvent;
       end;
  end;
  MapView.Invalidate;
  btnSelectPOIImage.Enabled := (rgPOIMode.ItemIndex = 1);
end;

procedure TCfgFrame.SetMapView(AValue: TMapView);
begin
  if FMapView = AValue then
    exit;
  FMapView := AValue;
  if FMapView = nil then
    raise Exception.Create('[TCfgFrame.SetMapView] New mapview is nil.');

  FMapView.GetMapProviders(cmbProviders.Items);
  cmbProviders.ItemIndex := cmbProviders.Items.IndexOf(FMapView.MapProvider);
  //sgLayers.Columns[1].PickList.Assign(CbProviders.Items);    // << --- FIX ME
  FMapView.DoubleBuffered := true;
  FMapView.Zoom := 1;
  FMapView.PluginManager := PluginManager;
  cbZoomToCursor.Checked := FMapView.ZoomToCursor;
  cbUseThreads.Checked := FMapView.UseThreads;
  cbDoubleBuffer.Checked := FMapView.DoubleBuffered;
  cbPOITextBgColor.Selected := FMapView.POITextBgColor;
  cbPOITextBgColor.ItemHeight := cmbProviders.ItemHeight + 2;
  cbMapGrid.Checked := MapGridPlugin.Enabled;
  cbMapScale.Checked := MapScalePlugin.Enabled;
  clbBackColor.ButtonColor := FMapView.InactiveColor;

  FDrawGPSpointEvent := FMapView.OnDrawGPSPoint;
end;

procedure TCfgFrame.UpdateDownloadEngineProxy;
begin
  if MapView.DownloadEngine.HasProxySupport then
  begin
    MapView.DownloadEngine.SetProxy(
      rbSystemProxy.Checked, rbProxyData.Checked,
      edProxyHost.Name, seProxyPort.Value, edProxyUserName.Text, edProxyPassword.Text
    );
    rbSystemProxy.Enabled := MapView.DownloadEngine.SupportsSystemProxy;
    lblProxyHost.Enabled := rbProxyData.Checked;
    edProxyHost.Enabled := rbProxyData.Checked;
    lblProxyPort.Enabled := rbProxyData.Checked;
    seProxyPort.Enabled := rbProxyData.Checked;
    lblProxyUserName.Enabled := rbProxyData.Checked;
    edProxyUserName.Enabled := rbProxyData.Checked;
    lblProxyPassword.Enabled := rbProxyData.Checked;
    edProxyPassword.Enabled := rbProxyData.Checked;
  end;
end;

procedure TCfgFrame.UpdateTileSizeInfo(ATileSize: TSize);
begin
  infoTileSize.Caption := Format('%d x %d', [ATileSize.CX, ATileSize.CY]);
end;

procedure TCfgFrame.WriteToIni(ini: TCustomIniFile);
var
  n: Integer = 0;
begin
  if rbSystemProxy.Checked then
    n := 1
  else if rbProxyData.Checked then
    n := 2;
  ini.EraseSection('Proxy');
  ini.WriteInteger('Proxy', 'UseProxy', n);
  ini.WriteString('Proxy', 'ProxyHost', edProxyHost.Text);
  ini.WriteInteger('Proxy', 'ProxyPort', seProxyPort.Value);
  ini.WriteString('Proxy', 'ProxyName', edProxyUserName.Text);
  ini.WriteString('Proxy', 'ProxyPassword', edProxyPassword.Text);
end;


end.

