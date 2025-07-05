unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, DividerBevel, ExtCtrls, Spin, StdCtrls, SysUtils,
  Forms, Controls, Graphics, Dialogs, //LazLogger,
  mvMapViewer, mvTypes, mvEngine, mvPluginCommon, mvMapGridPlugin;

type
  TMainForm = class(TForm)
    cbCyclic: TCheckBox;
    cbEnabled: TCheckBox;
    cbLeft: TCheckBox;
    cbTop: TCheckBox;
    cbRight: TCheckBox;
    cbBottom: TCheckBox;
    clbBackgroundColor: TColorButton;
    clbLabelTextColor: TColorButton;
    clbPenColor: TColorButton;
    cmbIncrement: TComboBox;
    divLines: TDividerBevel;
    divLabels: TDividerBevel;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    lblLabelDistance: TLabel;
    lblIncrement: TLabel;
    lblOpacity: TLabel;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    ParamsPanel: TPanel;
    seLabelDistance: TSpinEdit;
    tbOpacity: TTrackBar;
    procedure cbEnabledChange(Sender: TObject);
    procedure cbCyclicChange(Sender: TObject);
    procedure clbLabelTextColorColorChanged(Sender: TObject);
    procedure LabelPositionChange(Sender: TObject);
    procedure clbBackgroundColorColorChanged(Sender: TObject);
    procedure clbPenColorColorChanged(Sender: TObject);
    procedure cmbIncrementChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure seLabelDistanceChange(Sender: TObject);
    procedure tbOpacityChange(Sender: TObject);
  private
    FMapGridPlugin: TMapGridPlugin;
    procedure AddOrDeletePointOfInterest(X, Y: Integer);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;
  MapView.Active := true;
  MapView.Zoom := 5;
  FMapGridPlugin := TMapGridPlugin.Create(PluginManager);

  clbBackgroundColor.ButtonColor := ColorToRGB(FMapGridPlugin.BackgroundColor);
  clbPenColor.ButtonColor := ColorToRGB(FMapGridPlugin.Pen.Color);
  clbLabelTextColor.ButtonColor := ColorToRGB(FMapGridPlugin.Font.Color);
  tbOpacity.Position := round(FMapGridPlugin.BackgroundOpacity * 100);
end;

procedure TMainForm.AddOrDeletePointOfInterest(X, Y: Integer);
const
  DELTA = 5;   // Tolerance of the HitTest
var
  layer: TMapLayer;
  poi: TMapPointOfInterest;
  RP: TRealPoint;
  area: TRealArea;
  list: TMapObjectList;
  i: Integer;
begin
  if MapView.Layers.Count = 0 then
    layer := TMapLayer(MapView.Layers.Add)
  else
    layer := MapView.Layers[0];

  RP := MapView.Engine.ScreenToLatLon(Point(X, Y));
  area := MapView.Engine.ScreenRectToRealArea(Rect(X-DELTA, Y-DELTA, X+DELTA, Y+DELTA));

  list := layer.PointsOfInterest.HitTest(area);
  try
    if (list = nil) then
    begin
      poi := TMapPointOfInterest(layer.PointsOfInterest.Add);
      poi.Caption := 'Test ' + IntToStr(layer.PointsOfInterest.Count);
      poi.ImageIndex := Random(ImageList1.Count);
      poi.Longitude := RP.Lon;
      poi.Latitude := RP.Lat;
    end else
    begin
      for i := list.Count-1 downto 0 do
      begin
        if list[i] is TMapPointOfInterest then
        begin
          poi := TMapPointOfInterest(list[i]);
          poi.Free;
        end;
      end;
    end;
  finally
    list.Free;
  end;
end;

procedure TMainForm.cbCyclicChange(Sender: TObject);
begin
  MapView.Cyclic := cbCyclic.Checked;
end;

procedure TMainForm.cbEnabledChange(Sender: TObject);
begin
  FMapGridPlugin.Enabled := cbEnabled.Checked;
end;

procedure TMainForm.clbBackgroundColorColorChanged(Sender: TObject);
begin
  FMapGridPlugin.BackgroundColor := clbBackgroundColor.ButtonColor;
end;

procedure TMainForm.clbLabelTextColorColorChanged(Sender: TObject);
begin
  FMapGridPlugin.Font.Color := clbLabelTextColor.ButtonColor;
end;

procedure TMainForm.clbPenColorColorChanged(Sender: TObject);
begin
  FMapGridPlugin.Pen.Color := clbPenColor.ButtonColor;
end;

procedure TMainForm.cmbIncrementChange(Sender: TObject);
var
  s: String;
  multiplier: Double;
  p: Integer;
begin
  if cmbIncrement.ItemIndex <= 0 then
    FMapGridPlugin.Increment := 0
  else
  begin
    s := cmbIncrement.Items[cmbIncrement.ItemIndex];
    p := pos('°', s);
    if p > 0 then
      multiplier := 1.0
    else
    begin
      p := pos('''', s);
      if p > 0 then
        multiplier := 1.0/60
      else
      begin
        p := pos('"', s);
        if p > 0 then
          multiplier := 1.0/3600
        else
          exit;
      end;
    end;
    s := copy(s, 1, p-1);
    FMapGridPlugin.Increment := StrToInt(s) * multiplier;
  end;
end;

procedure TMainForm.LabelPositionChange(Sender: TObject);
begin
  with FMapGridPlugin do
  begin
    if cbLeft.Checked then
      LabelPositions := LabelPositions + [glpLeft]
    else
      LabelPositions := LabelPositions - [glpLeft];

    if cbTop.Checked then
      LabelPositions := LabelPositions + [glpTop]
    else
      LabelPositions := LabelPositions - [glpTop];

    if cbRight.Checked then
      LabelPositions := LabelPositions + [glpRight]
    else
      LabelPositions := LabelPositions - [glpRight];

    if cbBottom.Checked then
      LabelPositions := LabelPositions + [glpBottom]
    else
      LabelPositions := LabelPositions - [glpBottom];
  end;
end;

procedure TMainForm.MapViewMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (Shift = []) then
    AddOrDeletePointOfInterest(X, Y);
end;

procedure TMainForm.seLabelDistanceChange(Sender: TObject);
begin
  FMapGridPlugin.LabelDistance := seLabelDistance.Value;
end;

procedure TMainForm.tbOpacityChange(Sender: TObject);
begin
  FMapGridPlugin.BackgroundOpacity := tbOpacity.Position / 100;
end;

end.

