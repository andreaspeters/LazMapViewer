{
Color picking from mouse position outside the form was inspired by
  https://lazplanet.blogspot.com/2016/06/how-to-detect-mouse-events-outside-your.html
}

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, ComCtrls,
  LCLIntf, Graphics, Dialogs, StdCtrls, Windows,
  mvMapViewer, mvPluginCommon, mvPlugins,
  mvDE_BGRA, mvDE_RGBGraphics,
  mvtilemodifyplugin, Controls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnResetBrightnessContrast: TButton;
    btnInvalidate: TButton;
    cbUseInternalCache: TCheckBox;
    cbShowRailwayLayer: TCheckBox;
    ColorDialog1: TColorDialog;
    cbDrawingEngine: TComboBox;
    gbColorExchange: TGroupBox;
    gbBrightnessContrast: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblMsPerTile: TLabel;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    Panel1: TPanel;
    rgMode: TRadioGroup;
    shpOrgColor: TShape;
    shpExchangeColor: TShape;
    tbContrast: TTrackBar;
    timerColorPick: TTimer;
    tbColorThresh: TTrackBar;
    tbBrightness: TTrackBar;
    procedure btnInvalidateClick(Sender: TObject);
    procedure btnResetBrightnessContrastClick(Sender: TObject);
    procedure cbDrawingEngineChange(Sender: TObject);
    procedure cbShowRailwayLayerChange(Sender: TObject);
    procedure cbUseInternalCacheClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MapView1AfterPaint(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
    procedure shpOrgColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shpExchangeColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbBrightnessChange(Sender: TObject);
    procedure tbContrastChange(Sender: TObject);
    procedure timerColorPickTimer(Sender: TObject);
    procedure tbColorThreshChange(Sender: TObject);
  private
    FMvBGRADrawingEngine : TMvBGRADrawingEngine;
    FMvRGBGraphicsDrawingEngine : TMvRGBGraphicsDrawingEngine;
    FTileModifyPlugin : TTileModifyPlugin;
    // Buffer for the screen wide color picking with the mouse
    FMousePoint : TPoint;
    FMousePointTick : Cardinal;
    // The saver method, to keep track of the mouse running around on the screen
    procedure SetMousePosiFromHook(const AMousePoint : TPoint);
  public

  end;

const
  DefaultColorPickerDelay = 50; // Millisecond between ColorPicker events


var
  Form1: TForm1;

implementation

{$R *.lfm}


//------------------------------------------------------------------------------
// ScreenColor
// The function returns the collor of the pixel at the given
// Screen position. Multi-Screen Systems are not supported
//------------------------------------------------------------------------------
function ScreenColor(const AScreenX, AScreenY: Integer):TColor;
var
  ScreenDC: HDC;
  SaveBitmap: Graphics.TBitmap;
begin
  SaveBitmap := Graphics.TBitmap.Create;
  try
    SaveBitmap.SetSize(Screen.Width, Screen.Height);
    ScreenDC := GetDC(0);
    try
      SaveBitmap.LoadFromDevice(ScreenDC);
    finally
      ReleaseDC(0, ScreenDC);
    end;
    Result := SaveBitmap.Canvas.Pixels[AScreenX, AScreenY];
  finally
    SaveBitmap.Free;
  end;
end;

//------------------------------------------------------------------------------
// Needed structure and global variable for the Scree-Wide-Color-Picker
//------------------------------------------------------------------------------
type
  PMouseLLHookStruct = ^TMouseLLHookStruct;
  TMouseLLHookStruct = record
    pt          : TPoint;
    mouseData   : Cardinal;
    flags       : Cardinal;
    time        : Cardinal;
    dwExtraInfo : Cardinal;
  end;
var
  mHook : Cardinal = 0;
//------------------------------------------------------------------------------
// The Hook-Callback to catch the mouse position
// possible wParam values: WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_MOUSEWHEEL, WM_RBUTTONDOWN, WM_RBUTTONUP
// wm_mousewheel
//------------------------------------------------------------------------------
function LowLevelMouseHookProc(nCode : LongInt; wParam : WPARAM; lParam : LPARAM) : LRESULT; stdcall;
var
  info : PMouseLLHookStruct absolute lParam;
begin
  Result := CallNextHookEx(mHook, nCode, wParam, lParam);
  with info^ do
  begin
    Form1.SetMousePosiFromHook(pt);
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  lmvOptions : TMapViewOptions;
begin
  MapView1.Active := true;

  FMvBGRADrawingEngine := TMvBGRADrawingEngine.Create(Self);
  FMvRGBGraphicsDrawingEngine := TMvRGBGraphicsDrawingEngine.Create(Self);
  FTileModifyPlugin := TTileModifyPlugin.Create(Self);
  FTileModifyPlugin.MapView := MapView1;
  FTileModifyPlugin.ModifyMode:= tmmNone;
  FTileModifyPlugin.OrgColor := RGB($ad,$d1,$9e);
  FTileModifyPlugin.ExchangeColor := clGreen;
  FTileModifyPlugin.SameColorRange := 0.1;
  tbColorThresh.Position := Trunc(FTileModifyPlugin.SameColorRange * tbColorThresh.Max);
  shpOrgColor.Brush.Color := FTileModifyPlugin.OrgColor;
  shpExchangeColor.Brush.Color := FTileModifyPlugin.ExchangeColor;
  FTileModifyPlugin.Brightness := 0.5;
  tbBrightness.Position := Trunc(FTileModifyPlugin.Brightness * tbBrightness.Max);
  FTileModifyPlugin.Contrast := 0.5;
  tbContrast.Position := Trunc(FTileModifyPlugin.Contrast * tbContrast.Max);
   {
  lmvOptions := MapView1.Options;
  Include(lmvOptions, mvoPluginCopyTiles);
  MapView1.Options := lmvOptions;
  }
  MapView1.Options := MapView1.Options + [mvoPluginCopyTiles];
  MvPluginManager1.PluginList.Add(FTileModifyPlugin);
end;

procedure TForm1.cbDrawingEngineChange(Sender: TObject);
begin
  FTileModifyPlugin.ResetMilliSecondsPerTile;
  case cbDrawingEngine.ItemIndex of
    1 : MapView1.DrawingEngine := FMvBGRADrawingEngine;
    2 : MapView1.DrawingEngine := FMvRGBGraphicsDrawingEngine;
  else
    MapView1.DrawingEngine := Nil;
  end;
end;

procedure TForm1.cbShowRailwayLayerChange(Sender: TObject);
begin
  MapView1.Layers[0].Visible:= cbShowRailwayLayer.Checked;
end;

procedure TForm1.cbUseInternalCacheClick(Sender: TObject);
begin
  FTileModifyPlugin.UseCache := cbUseInternalCache.Checked;
end;

procedure TForm1.btnResetBrightnessContrastClick(Sender: TObject);
begin
  FTileModifyPlugin.Brightness := 0.5;
  tbBrightness.Position := Trunc(FTileModifyPlugin.Brightness * tbBrightness.Max);
  FTileModifyPlugin.Contrast := 0.5;
  tbContrast.Position := Trunc(FTileModifyPlugin.Contrast * tbContrast.Max);
  MapView1.Invalidate;
end;

procedure TForm1.btnInvalidateClick(Sender: TObject);
begin
  MapView1.Invalidate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if (mHook <> 0) then
    UnhookWindowsHookEx(mHook);
end;

procedure TForm1.MapView1AfterPaint(Sender: TObject);
begin
  if Assigned(FTileModifyPlugin) then
    lblMsPerTile.Caption := Format('%1.4f ms %s(%d Tiles)',[FTileModifyPlugin.MilliSecondsPerTile, #13#10, FTileModifyPlugin.AvgTileCount+1]);
end;

procedure TForm1.rgModeClick(Sender: TObject);
var
  lmvOptions : TMapViewOptions;
begin
  case rgMode.ItemIndex of
    0 : FTileModifyPlugin.ModifyMode := tmmNone;
    1 : FTileModifyPlugin.ModifyMode := tmmGrayScale;
    2 : FTileModifyPlugin.ModifyMode := tmmColorExchange;
    3 : FTileModifyPlugin.ModifyMode := tmmBrightnessContrast;
  else
    FTileModifyPlugin.ModifyMode := tmmNone;
  end;
  gbColorExchange.Enabled := (FTileModifyPlugin.ModifyMode = tmmColorExchange);
  gbBrightnessContrast.Enabled := (FTileModifyPlugin.ModifyMode = tmmBrightnessContrast);

  FTileModifyPlugin.ResetMilliSecondsPerTile;

  lmvOptions := MapView1.Options;
  if FTileModifyPlugin.ModifyMode = tmmNone then
    Exclude(lmvOptions, mvoPluginCopyTiles)
  else
    Include(lmvOptions, mvoPluginCopyTiles);
  MapView1.Options := lmvOptions;
  MapView1.Invalidate;
end;
//------------------------------------------------------------------------------
// Proceed the Screen-Wide Color-Picking
// The Application.MessageBox is used to keep the situation modal here.
// The user will be able to move around with the mouse, while the Messagebox is open.
// The color below the mouse pointer is displayed in the color field in the app.
// To pick the color the user has to hit the enter-Key on the keyboard in order
// to press the OK-Button.
//------------------------------------------------------------------------------
procedure TForm1.shpOrgColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  WH_MOUSE_LL = 14;
begin
  mHook := SetWindowsHookEx(WH_MOUSE_LL, @LowLevelMouseHookProc, hInstance, 0);
  if mHook <> 0 then
  try
    timerColorPick.Enabled := True;
    if Application.MessageBox('Press RETURN if you found a color, or ESC to terminate!','Pick a color',mb_OK) = IDOK then
    begin
      FTileModifyPlugin.OrgColor := shpOrgColor.Brush.Color;
      MapView1.Invalidate;
    end
    else
      shpOrgColor.Brush.Color := FTileModifyPlugin.OrgColor;
  finally
    timerColorPick.Enabled := False;
    UnhookWindowsHookEx(mHook);
  end;
end;

procedure TForm1.shpExchangeColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ColorDialog1.Execute then
  begin
    FTileModifyPlugin.ExchangeColor := ColorDialog1.Color;
    shpExchangeColor.Brush.Color := FTileModifyPlugin.ExchangeColor;
    MapView1.Invalidate;
  end;
end;

procedure TForm1.tbBrightnessChange(Sender: TObject);
begin
  FTileModifyPlugin.Brightness:= tbBrightness.Position / tbBrightness.Max;
  MapView1.Invalidate;
end;

procedure TForm1.tbContrastChange(Sender: TObject);
begin
  FTileModifyPlugin.Contrast:= tbContrast.Position / tbContrast.Max;
  MapView1.Invalidate;
end;

procedure TForm1.tbColorThreshChange(Sender: TObject);
begin
  FTileModifyPlugin.SameColorRange:= tbColorThresh.Position / tbColorThresh.Max;
  MapView1.Invalidate;
end;

//------------------------------------------------------------------------------
// If the MouseHook is activated, it will call this method
// We store the current mouse position and also the tick, if no tick counter
// is running
//------------------------------------------------------------------------------
procedure TForm1.SetMousePosiFromHook(const AMousePoint : TPoint);
begin
  FMousePoint := AMousePoint;
  if FMousePointTick = 0 then // If the last Tick has been processed, we store a new tick
    FMousePointTick := GetTickCount;
end;
//------------------------------------------------------------------------------
// The timerColorPick event
// The timer is called 40 times a second, but only if the
// we check
//------------------------------------------------------------------------------
procedure TForm1.timerColorPickTimer(Sender: TObject);
var
  t0 : Cardinal;
  col : TColor;
begin
  if FMousePointTick > 0 then //If a tick has been stored
  begin
    t0 := GetTickCount; //get the current tick-counter
    if t0 < FMousePointTick then // Catch the 49-Day glitch
      FMousePointTick := t0;
    if (t0-FMousePointTick) >= DefaultColorPickerDelay then
    begin // here we run in, approx 50ms after the last stored mouse move
      // get the screen color. This takes a while, so we will do this only 20 times per second
      col :=  ScreenColor(FMousePoint.x, FMousePoint.y);
      // Save the Color on the mpouse pointer
      shpOrgColor.Brush.Color := col;
      // Clear the timertick
      FMousePointTick := 0;
    end;
  end;
end;


end.

