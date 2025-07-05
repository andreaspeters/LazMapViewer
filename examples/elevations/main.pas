unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  LazFileUtils,
  LCLIntf, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Buttons,
  mvMapViewer, mvTypes, mvEngine, mvGpsObj, mvDrawingEngine, mvGeoMath;

type
  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    cbElevDatasets: TComboBox;
    DataGrid: TDrawGrid;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblMapData: TLabel;
    MapStyleLink: TLabel;
    lblMapPresentation: TLabel;
    MapDataLink: TLabel;
    lblDatasets: TLabel;
    OpenTopoDataLink: TLabel;
    MapLicenseLink: TLabel;
    MapView: TMapView;
    Log: TMemo;
    Panel1: TPanel;
    btnDelete: TSpeedButton;
    MapPanel: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure DataGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DataGridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure URLClick(Sender: TObject);
    procedure URLMouseEnter(Sender: TObject);
    procedure URLMouseLeave(Sender: TObject);
    procedure MapViewDrawGpsPoint(Sender: TObject;
      ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure MapViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnDeleteClick(Sender: TObject);
  private
    FCounter: Integer;
    function GetElevation(APoint: TRealPoint): Double;
    function SelectedDataset: String;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math, fpjson, jsonparser;

const
  _CLICKED_POINTS_ = 10;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView.Center := RealPoint(47, 11);
  MapView.Zoom := 8;
  MapView.MapProvider := 'Open Topo Map';
  MapView.Active := true;

  DataGrid.ColWidths[0] := 40;
  DataGrid.ColWidths[1] := 100;
  DataGrid.ColWidths[2] := 100;
  DataGrid.ColWidths[3] := 90;
end;

// Sends a request to opentopodata.org and extracts the elevation from the
// returned json stream which is structures as follows:
(*
{
    "results": [{
        "elevation": 815.0,
        "location": {
            "lat": 56.0,
            "lng": 123.0
        },
        "dataset": "test-dataset"
    }],
    "status": "OK"
}
*)
function TMainForm.GetElevation(APoint: TRealPoint): Double;
var
  url: String;
  fs: TFormatSettings;
  stream: TStream;
  json: TJsonData;
  jsonData: TJsonData;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  url := Format('https://api.opentopodata.org/v1/%s?locations=%.9f,%.9f', [
    SelectedDataset, APoint.Lat, APoint.Lon
  ], fs);
  stream := TMemoryStream.Create;
  try
    MapView.Engine.DownloadEngine.DownloadFile(url, stream);
    json := GetJSON(stream);
    try
      Log.Text := json.FormatJSON;
      jsonData := json.FindPath('results[0]').FindPath('elevation');
      if jsonData.IsNull then
        Result := NaN
      else
        Result := jsonData.AsFloat;
    finally
      json.Free;
    end;
  finally
    stream.free;
  end;
end;

// Calculates the selected dataset parameter for the opentopodata URL.
function TMainForm.SelectedDataset: String;
var
  s: String;
  p: Integer;
begin
  Result := '';
  if cbElevDatasets.ItemIndex <> -1 then
  begin
    s := cbElevDatasets.Items[cbElevDatasets.ItemIndex];
    p := pos(' ', s);
    if p > 0 then
      Result := Copy(s, 1, p-1);
  end;
end;

// Draws the stored GPS points
procedure TMainForm.MapViewDrawGpsPoint(Sender: TObject;
  ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
const
  MARGIN = 8;
var
  P: TPoint;
  d: Integer;
  extent: TSize;
  R: TRect;
begin
  // Screen coordinates of the GPS point
  P := TMapView(Sender).LatLonToScreen(APoint.RealPoint);

  // Draw the point symbol
  ADrawer.PenColor := clRed;
  ADrawer.PenWidth := 3;
  d := 5;
  ADrawer.Line(P.x - d, P.Y - d, P.X + d, P.Y + d);
  ADrawer.Line(P.x - d, P.Y + d, P.X + d, P.Y - d);

  // Prepare text output: background color...
  inc(P.Y, d + 4);
  extent := ADrawer.TextExtent(APoint.Name);
  R := Rect(0, 0, extent.CX, extent.CY);
  OffsetRect(R, P.X - extent.CX div 2, P.Y);
  InflateRect(R, MARGIN, 0);
  ADrawer.BrushStyle := bsSolid;
  ADrawer.BrushColor := clRed;
  ADrawer.FillRect(R.Left, R.Top, R.Right, R.Bottom);

  // ... and font
  ADrawer.FontColor := clWhite; //MapView.Font.Color;
  ADrawer.FontName := MapView.Font.Name;
  ADrawer.FontSize := MapView.Font.Size;
  ADrawer.FontStyle := MapView.Font.Style;

  // Write the POI text
  ADrawer.TextOut(P.X - extent.CX div 2, P.Y, APoint.Name);
end;

{ Gets the GPS coordinates of the clicked point in the map view and query the
  elevation. Store as a data point in the GPSItems of the MapView. }
procedure TMainForm.MapViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TRealPoint;
  gpsPt: TGpsPoint;
  elev: Double;
begin
  if (ssCtrl in Shift) then
  begin
    p := MapView.ScreenToLatLon(Point(X, Y));
    elev := GetElevation(p);

    gpsPt := TGpsPoint.CreateFrom(p, elev);
    inc(FCounter);
    gpsPt.Name := IntToStr(FCounter);
    MapView.GpsItems.Add(gpsPt, _CLICKED_POINTS_);

    DataGrid.RowCount := MapView.GPSItems.Count + DataGrid.FixedRows;
  end;
end;

{ Draws a cell in the DataGrid which gets is data from the GPS points stored in
  the MapView component. }
procedure TMainForm.DataGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  cellText: String;
  idx: Integer;
  pt: TGPSPoint;
begin
  cellText := '';
  if ARow = 0 then
    case ACol of
      0: cellText := 'No.';
      1: cellText := 'Longitude';
      2: cellText := 'Latitude';
      3: cellText := 'Elevation (m)';
    end
  else
  begin
    idx := ARow - DataGrid.FixedRows;
    pt := MapView.GPSItems[idx] as TGPSPoint;
    case ACol of
      0: cellText := pt.Name;
      1: cellText := LonToStr(pt.Lon, true);
      2: cellText := LatToStr(pt.Lat, true);
      3: if IsNaN(pt.Elevation) then
           cellText := '(unknown)'
         else
           cellText := FormatFloat('0', pt.Elevation);
    end;
  end;
  InflateRect(ARect, -varCellPadding, -varCellPadding);
  DataGrid.Canvas.TextRect(ARect, ARect.Left, ARect.Top, cellText);
end;

procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  btnDelete.Enabled := (DataGrid.RowCount > 1) and (DataGrid.Row > 0);
end;

// Some formatting for the DataGrid: Headers will be bold and centered,
// data cells will be right-aligned.
procedure TMainForm.DataGridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  ts := DataGrid.Canvas.TextStyle;
  if aRow = 0 then
  begin
    DataGrid.Canvas.Font.Style := [fsBold];
    ts.Alignment := taCenter;
  end else
  begin
    ts.Alignment := taRightJustify;
  end;
  DataGrid.Canvas.TextStyle := ts;
end;

//  Deletes the selected datapoint in the DataGrid.
procedure TMainForm.btnDeleteClick(Sender: TObject);
var
  idx: Integer;
  item: TGPSObj;
begin
  idx := DataGrid.Row - DataGrid.FixedRows;
  if idx < 0 then
    exit;

  // Delete from map's GPS items
  item := MapView.GPSItems[idx];
  MapView.GPSItems.Delete(item);
  MapView.Invalidate;

  // Update grid
  DataGrid.RowCount := DataGrid.RowCount-1;
  DataGrid.Invalidate;
end;

// Open the browser with the label hint's web-site.
procedure TMainForm.URLClick(Sender: TObject);
begin
  if (Sender is TLabel) and (TLabel(Sender).Hint <> '') then
    OpenURL(TLabel(Sender).Hint);
end;

// Underline the URL when the mouse enters the label
procedure TMainForm.URLMouseEnter(Sender: TObject);
begin
  if Sender is TLabel then
    TLabel(Sender).Font.Style := [fsUnderline];
end;

// Remove the underline when the mouse leaves the label
procedure TMainForm.URLMouseLeave(Sender: TObject);
begin
  if Sender is TLabel then
    TLabel(Sender).Font.Style := [];
end;

end.

