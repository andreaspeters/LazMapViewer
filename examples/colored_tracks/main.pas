unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvGpsObj, mvGpx;

type

  { TMainForm }

  TMainForm = class(TForm)
    cbRedTour: TCheckBox;
    cbBlueTour: TCheckBox;
    cbBlackTour: TCheckBox;
    cbAllowDragging: TCheckBox;
    cbAllowZooming: TCheckBox;
    cbProviders: TComboBox;
    lblProviders: TLabel;
    ZoomLabel: TLabel;
    MainLabel: TLabel;
    MapView: TMapView;
    ParamsPanel: TPanel;
    procedure cbRedTourChange(Sender: TObject);
    procedure cbBlueTourChange(Sender: TObject);
    procedure cbBlackTourChange(Sender: TObject);
    procedure cbAllowDraggingChange(Sender: TObject);
    procedure cbAllowZoomingChange(Sender: TObject);
    procedure cbProvidersChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MapViewZoomChange(Sender: TObject);
  private
    FTrack1: TGpsTrack;
    FTrack2: TGpsTrack;
    FTrack3: TGpsTrack;
    function LoadGPXFile(AFileName: String; AColor: TColor; AWidth: Double): TGPSTrack;
    procedure UpdateInfo;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  mvTypes, mvEngine;

const
  FILENAME1 = '../../Yosemite Tour 1 - Trans Yosemite.gpx';
  FILENAME2 = '../../Yosemite Tour 2 - Bear Valley.gpx';
  FILENAME3 = '../../Yosemite Tour 3 - Triangle Loop.gpx';

{ TMainForm }

procedure TMainForm.FormActivate(Sender: TObject);
var
  crs: TCursor;
  totalArea: TRealArea;
  trackArea: TRealArea;
begin
  crs := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    MapView.UseThreads := true;
    MapView.MapProvider := cbProviders.Text;
    MapView.Active := true;

    // Load GPX files
    FTrack1 := LoadGPXFile(Application.Location + FILENAME1, clRed, 1.0);
    FTrack1.GetArea(totalArea);

    FTrack2 := LoadGPXFile(Application.Location + FILENAME2, clBlue, 1.2);
    FTrack2.GetArea(trackArea);
    ExtendArea(totalArea, trackArea);

    FTrack3 := LoadGPXFile(Application.Location + FILENAME3, clBlack, 0.5);
    FTrack3.GetArea(trackArea);
    ExtendArea(totalArea, trackArea);

    MapView.ZoomOnArea(totalArea);
    UpdateInfo;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.MapViewZoomChange(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TMainForm.UpdateInfo;
begin
  ZoomLabel.Caption := 'Zoom ' + MapView.Zoom.ToString;
end;

procedure TMainForm.cbRedTourChange(Sender: TObject);
begin
  FTrack1.Visible := cbRedTour.Checked;
  Mapview.Invalidate;
end;

procedure TMainForm.cbBlueTourChange(Sender: TObject);
begin
  FTrack2.Visible := cbBlueTour.Checked;
  MapView.Invalidate;
end;

procedure TMainForm.cbBlackTourChange(Sender: TObject);
begin
  FTrack3.Visible := cbBlackTour.Checked;
  MapView.Invalidate;
end;

procedure TMainForm.cbAllowDraggingChange(Sender: TObject);
begin
  if cbAllowDragging.Checked then
    MapView.Options := MapView.Options + [mvoMouseDragging]
  else
    MapView.Options := MapView.Options - [mvoMouseDragging];
end;

procedure TMainForm.cbAllowZoomingChange(Sender: TObject);
begin
  if cbAllowZooming.Checked then
    MapView.Options := MapView.Options + [mvoMouseZooming]
  else
    MapView.Options := MapView.Options - [mvoMouseZooming];
end;

procedure TMainForm.cbProvidersChange(Sender: TObject);
begin
  MapView.MapProvider := cbProviders.Text;
end;

function TMainForm.LoadGPXFile(AFileName: String;
  AColor: TColor; AWidth: Double): TGPSTrack;
var
  reader: TGpxReader;
  id: Integer;
begin
  reader := TGpxReader.Create;
  try
    id := reader.LoadFromFile(AFileName, MapView.GPSItems);
    Result := MapView.GpsItems.FindTrackByID(id);
    Result.LineColor := AColor;
    Result.LineWidth := AWidth;
  finally
    reader.Free;
  end;
end;

end.

