unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math,
  Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  mvDLEFpc, mvMapViewer, mvPluginCommon, mvPlugins, mvGPSObj;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    cbGlobalEnabled: TCheckBox;
    cbLeftEnabled: TCheckBox;
    cbRightEnabled: TCheckBox;
    gbDragging: TGroupBox;
    POI_Images: TImageList;
    MapView_left: TMapView;
    MapView_right: TMapView;
    PluginManager: TMvPluginManager;
    DraggableMarkerPlugin: TDraggableMarkerPlugin;
    procedure cbGlobalEnabledChange(Sender: TObject);
    procedure cbLeftEnabledChange(Sender: TObject);
    procedure cbRightEnabledChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);

  procedure AddTraditionalMarker(AMapView: TMapView; const ALon, ALat: Double;
    ACaption: String);
  const
    IMG_INDEX: Integer = 0;
  var
    gpsPt: TGpsPointOfInterest;
  begin
    gpsPt := TGpsPointOfInterest.Create(ALon,ALat);
    try
      gpsPt.Name := ACaption;
      if AMapView = MapView_left then
        gpsPt.ImageIndex := -1
      else
      begin
        gpsPt.ImageIndex := IMG_INDEX;
        IMG_INDEX := (IMG_INDEX + 1) mod POI_Images.Count;
      end;
      AMapView.GPSItems.Add(gpsPt, 100);
      gpsPt := Nil;
    finally
      if Assigned(gpsPt) then
        gpsPt.Free;
    end;
  end;

begin
  MapView_left.Active := true;
  AddTraditionalMarker(MapView_left,     0.0000000,  51.4825766, 'Greenwich');
  AddTraditionalMarker(MapView_left,     2.2945500,  48.8582300, 'Tour d´Eiffel, Paris');
  AddTraditionalMarker(MapView_left,   -79.3884000,  43.6439500, 'CN Tower, Toronto');
  AddTraditionalMarker(MapView_left,  -157.7739800,  21.2716900, 'Kahala Avenue, Honolulu');
  AddTraditionalMarker(MapView_left,   114.1497900,  22.2708100, 'The Peak, Hong Kong');
  AddTraditionalMarker(MapView_left,    13.377778,   52.5163890, 'Brandenburger Tor, Berlin');
  AddTraditionalMarker(MapView_left,   -58.3722400, -34.6084700, 'Pirámide de Mayo, Buenos Aires');
  AddTraditionalMarker(MapView_left,   151.2082800, -33.8707000, 'Sydney Tower Skywalk, Sydney');
  AddTraditionalMarker(MapView_left,   139.7021800,  35.6787500, 'Meiji Jingu Shrine, Tokyo');

  MapView_right.Active := true;
  AddTraditionalMarker(MapView_right,    0.0000000,  51.4825766, 'Greenwich');
  AddTraditionalMarker(MapView_right,    2.2945500,  48.8582300, 'Tour d´Eiffel, Paris');
  AddTraditionalMarker(MapView_right,  -79.3884000,  43.6439500, 'CN Tower, Toronto');
  AddTraditionalMarker(MapView_right, -157.7739800,  21.2716900, 'Kahala Avenue, Honolulu');
  AddTraditionalMarker(MapView_right,  114.1497900,  22.2708100, 'The Peak, Hong Kong');
  AddTraditionalMarker(MapView_right,   13.377778,   52.5163890, 'Brandenburger Tor, Berlin');
  AddTraditionalMarker(MapView_right,  -58.3722400, -34.6084700, 'Pirámide de Mayo, Buenos Aires');
  AddTraditionalMarker(MapView_right,  151.2082800, -33.8707000, 'Sydney Tower Skywalk, Sydney');
  AddTraditionalMarker(MapView_right,  139.7021800,  35.6787500, 'Meiji Jingu Shrine, Tokyo');
end;

procedure TMainForm.cbGlobalEnabledChange(Sender: TObject);
begin
  DraggableMarkerPlugin.Enabled := cbGlobalEnabled.Checked;
  cbLeftEnabled.Enabled := cbGlobalEnabled.Checked;
  cbRightEnabled.Enabled := cbGlobalEnabled.Checked;
end;

procedure TMainForm.cbLeftEnabledChange(Sender: TObject);
begin
  DraggableMarkerPlugin.MapViewEnabled[MapView_left] := cbLeftEnabled.Checked;
end;

procedure TMainForm.cbRightEnabledChange(Sender: TObject);
begin
  DraggableMarkerPlugin.MapViewEnabled[MapView_right] := cbRightEnabled.Checked;
end;

end.

