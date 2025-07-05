unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph,
  TATools, mvMapViewer, mvPluginCommon;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    ChartToolset1: TChartToolset;
    procedure FormCreate(Sender: TObject);
  private
    FMapView: TMapView;
    FPluginManager: TMvPluginManager;
    procedure MouseDownHandler(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; X, Y: Integer);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
  TClickPlugin = class(TMvPlugin)
  private
    FMessageText: String;
  protected
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
  public
    property Messagetext: String read FMessagetext write FMessageText;
  end;

procedure TClickPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  ShowMessage(FMessagetext);
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  FPluginManager := TMvPluginManager.create(Self);

  FMapView := TMapView.Create(self);
  FMapView.Align := alClient;
  FMapView.Parent := Self;
  FMapView.mapProvider := 'OpenStreetMap Standard';
  FMapView.Active := true;
  FMapView.OnMouseDown := @MouseDownHandler;
  FMapView.PluginManager := FPluginManager;

  with TClickPlugin.Create(FPluginManager) do
    MessageText := 'aaa';

  with TClickPlugin.Create(FPluginManager) do
    Messagetext := 'bbb';
end;


procedure TForm1.MouseDownHandler(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer);
begin
  Caption := FormatDateTime('hh:nn:ss.zzz', time);
  Abort;
end;

end.

