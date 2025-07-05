unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvPluginCommon, mvPlugins, mvGPSObj, mvspreadmarker_plugin;

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckBox1: TCheckBox;
    MapView: TMapView;
    Panel1: TPanel;
    PluginManager: TMvPluginManager;
    DraggableMarkerPlugin: TDraggableMarkerPlugin;
    MvPluginManager1DraggableMarkerPlugin2: TDraggableMarkerPlugin;
    MvPluginManager1LegalNoticePlugin1: TLegalNoticePlugin;
    MvPluginManager1LegalNoticePlugin2: TLegalNoticePlugin;
    SpreadMarkerPlugin: TSpreadMarkerPlugin;
    UserDefinedPlugin: TUserDefinedPlugin;
    procedure FormCreate(Sender: TObject);
    procedure UserDefinedPluginMouseDown(Sender: TObject; AMapView: TMapView;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);

  procedure AddTraditionalMarker(const ALon, ALat : Double; ACaption : String);
  var
    gpsPt: TGpsPointOfInterest;
  begin
    gpsPt := TGpsPointOfInterest.Create(ALon,ALat);
    try
      gpsPt.Name := ACaption;
      gpsPt.ImageIndex := 0;
      MapView.GPSItems.Add(gpsPt, 100);
      gpsPt := Nil;
    finally
      if Assigned(gpsPt) then
        gpsPt.Free;
    end;
  end;

var
  i : Integer;
begin
  MapView.Active := true;
  AddTraditionalMarker(0.0, 51.4825766,'Greenwich');
  AddTraditionalMarker(2.2945500,48.8582300,'Tour d´Eiffel, Paris');
  AddTraditionalMarker(-79.3884000,43.6439500,'CN Tower, Toronto');
  AddTraditionalMarker(-157.7739800,21.2716900,'Kahala Avenue, Honolulu');
  AddTraditionalMarker(114.1497900,22.2708100,'The Peak, Hong Kong');
  AddTraditionalMarker(13.377778,52.516389,'Brandenburger Tor, Berlin');
  AddTraditionalMarker(-58.3722400,-34.6084700,'Pirámide de Mayo, Buenos Aires');
  AddTraditionalMarker(151.2082800,-33.8707000,'Sydney Tower Skywalk, Sydney');
  AddTraditionalMarker(139.7021800,35.6787500,'Meiji Jingu Shrine, Tokyo');
  for i := 0 to 99 do
  begin
    AddTraditionalMarker(0.0,0.0,'Test '+IntToStr(i));
  end;
end;

{ MvPluginManager1UserDefinedPlugin1MouseDown is used to delete some markers where
  the SpreadMarker-Plugin is in the SpreadMode.
  The deletion of the markers will be messaged to the SpreadMarker-plugin, so
  that the stored information of the spreaded markers are updated and no
  access violations on invalid memory occours.
  You can debug this in the deletion or EndUpdate Method. }
procedure TMainForm.UserDefinedPluginMouseDown(Sender: TObject;
  AMapView: TMapView; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  var Handled: Boolean);
var
  delcnt : Integer;
  lstcnt : Integer;
  ndx : Integer;
  i : Integer;
begin
  if not CheckBox1.Checked then Exit;
  if Button <> mbLeft then Exit;
  if (not Handled) and SpreadMarkerPlugin.SpreadModeActive[AMapView] then
  begin
    Handled := True; // Reserve this event for us, prohibit the dragging of the map
    lstcnt := MapView.GPSItems.Count;
    delcnt := Random(5)+1;
    if delcnt > lstcnt then
      delcnt := lstcnt;
    if delcnt <= 0 then Exit;
    if delcnt = 1 then
    begin
      ndx := Random(lstcnt);
      MapView.GPSItems.Delete(MapView.GPSItems.Items[ndx]);
    end
    else
    begin
      MapView.GPSItems.BeginUpdate;
      try
        for i := 0 to delcnt-1 do
        begin
          if lstcnt <= 0 then Break;
          ndx := Random(lstcnt);
          MapView.GPSItems.Delete(MapView.GPSItems.Items[ndx]);
          Dec(lstcnt);
        end;
      finally
        MapView.GPSItems.EndUpdate;
      end;
    end;
  end;
end;


end.

