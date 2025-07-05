unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs,
  mvMapViewer, mvTypes, mvPluginCommon, mvAreaSelectionPlugin;

type

  { TMainForm }

  TMainForm = class(TForm)
    lblHeader: TStaticText;
    MapView: TMapView;
    Memo: TMemo;
    PluginManager: TMvPluginManager;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);

  private
    FAreaSelectionPlugin: TAreaSelectionPlugin;
    FAreaSelectionPlugin2: TAreaSelectionPlugin;
    procedure OnSelectedAreaHit(Sender: TObject);
    procedure OnSelectedAreaBeginChange(Sender: TObject);

    procedure OnSelectedAreaChanged(Sender: TObject);
    procedure OnSelectedAreaChanging(Sender: TObject; ANewArea: TRealArea; var Allow: Boolean);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  lArea : TRealArea;
begin
  MapView.Active := true;
  FAreaSelectionPlugin := TAreaSelectionPlugin.Create(PluginManager);
  FAreaSelectionPlugin.Name := 'FAreaSelectionPlugin';
  FAreaSelectionPlugin.MapView := MapView;
  FAreaSelectionPlugin.OnSelectedAreaHit := @OnSelectedAreaHit;
  FAreaSelectionPlugin.OnSelectedAreaBeginChange := @OnSelectedAreaBeginChange;
  FAreaSelectionPlugin.OnSelectedAreaChanged := @OnSelectedAreaChanged;
  FAreaSelectionPlugin.OnSelectedAreaChanging := @OnSelectedAreaChanging;
  FAreaSelectionPlugin.Caption := '#1';
  FAreaSelectionPlugin.CaptionPosition := ascpRightTop;
  lblHeader.Caption := '                            Left     Top   Right  Bottom';


  FAreaSelectionPlugin2 := TAreaSelectionPlugin.Create(PluginManager);
  FAreaSelectionPlugin2.Name := 'FAreaSelectionPlugin2';
  FAreaSelectionPlugin2.SelectedArea.West := -150.0;
  FAreaSelectionPlugin2.SelectedArea.South := -60;
  FAreaSelectionPlugin2.MapView := MapView;
  FAreaSelectionPlugin2.OnSelectedAreaHit := @OnSelectedAreaHit;
  FAreaSelectionPlugin2.OnSelectedAreaBeginChange := @OnSelectedAreaBeginChange;
  FAreaSelectionPlugin2.OnSelectedAreaChanged := @OnSelectedAreaChanged;
  FAreaSelectionPlugin2.OnSelectedAreaChanging := @OnSelectedAreaChanging;
  FAreaSelectionPlugin2.Caption := '#2';
  FAreaSelectionPlugin2.CaptionPosition := ascpLeftBottom;
end;

procedure TMainForm.OnSelectedAreaHit(Sender: TObject);
var
  lSelAreaPlugin : TAreaSelectionPlugin absolute Sender;
  i : Integer;
begin
  if not (Sender is TAreaSelectionPlugin) then Exit;
  lSelAreaPlugin.Pen.Color := clGreen;
  for i := 0 to PluginManager.Count-1 do
  begin
    if (PluginManager.Items[i] is TAreaSelectionPlugin) and
       (PluginManager.Items[i] <> lSelAreaPlugin) then
    begin
      TAreaSelectionPlugin(PluginManager.Items[i]).Pen.Color := clNavy;
    end;
  end;
  Memo.Lines.Add('[%s] Hit:                                    %s',[
    FormatDateTime('hh:nn:ss.zzz', Now()),
    lSelAreaPlugin.Name
  ]);
end;

procedure TMainForm.OnSelectedAreaBeginChange(Sender: TObject);
var
  lSelAreaPlugin : TAreaSelectionPlugin absolute Sender;
begin
  if not (Sender is TAreaSelectionPlugin) then Exit;
  lSelAreaPlugin.Pen.Color := clRed;
  Memo.Lines.Add('[%s] BeginChange:                            %s',[
    FormatDateTime('hh:nn:ss.zzz', Now()),
    lSelAreaPlugin.Name
  ]);
end;

procedure TMainForm.OnSelectedAreaChanged(Sender: TObject);
var
  lSelAreaPlugin : TAreaSelectionPlugin absolute Sender;
begin
  if not (Sender is TAreaSelectionPlugin) then Exit;
  lSelAreaPlugin.Pen.Color := clGreen;
  Memo.Lines.Add('[%s]  Selected: %6.1f° %6.1f° %6.1f° %6.1f°, %s',[
    FormatDateTime('hh:nn:ss.zzz', Now()),
    lSelAreaPlugin.SelectedArea.North,
    lSelAreaPlugin.SelectedArea.West,
    lSelAreaPlugin.SelectedArea.South,
    lSelAreaPlugin.SelectedArea.East,
    lSelAreaPlugin.Name
  ]);
end;

procedure TMainForm.OnSelectedAreaChanging(Sender: TObject; ANewArea: TRealArea;
  var Allow: Boolean);
var
  lSelAreaPlugin : TAreaSelectionPlugin absolute Sender;
begin
  if not (Sender is TAreaSelectionPlugin) then Exit;
  with ANewArea do
    Memo.Lines.Add('[%s] Selecting: %6.1f° %6.1f° %6.1f° %6.1f° %s',[
      FormatDateTime('hh:nn:ss.zzz', Now()),
      TopLeft.Lon,
      TopLeft.Lat,
      BottomRight.Lon,
      BottomRight.Lat,
      lSelAreaPlugin.Name
    ]);
end;

end.

