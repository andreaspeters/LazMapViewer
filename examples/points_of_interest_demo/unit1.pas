unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs,
  mvMapViewer, mvTypes, mvDrawingEngine;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    MapView1: TMapView;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapView1MouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X,Y: Integer);
  private
    FPoiLayer: TMapLayer;
    FClickRequested: Boolean;
    procedure AddPointOfInterest(ALayer: TMapLayer; APoint: TRealPoint;
      ACaption: String; AImageIndex: Integer);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPoiLayer := (MapView1.Layers.Add as TMapLayer);
  //FPoiLayer.Opacity := 0.05;              // not working...
  //FPoiLayer.DrawMode := idmUseOpacity;

  MapView1.POITextBGColor := clBlack;
  MapView1.Font.Color := clWhite;
end;

procedure TForm1.AddPointOfInterest(ALayer: TMapLayer; APoint: TRealPoint;
  ACaption: String; AImageIndex: Integer);
var
  poi: TMapPointOfInterest;
begin
  poi := ALayer.PointsOfInterest.Add as TMapPointOfInterest;
  poi.RealPoint := APoint;
  poi.Caption := ACaption;
  poi.ImageIndex := AImageIndex;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  P: TRealPoint;
begin
  P := MapView1.Center;
  AddPointOfInterest(FPoiLayer, P, Format('Center Point: Latitude=%.6f, Longitude=%.6f', [P.Lat, P.Lon]), Random(ImageList1.Count));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FClickRequested := true;
end;

procedure TForm1.MapView1MouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X,Y: Integer);
var
  P: TRealPoint;
begin
  if FClickRequested then
  begin
    P := MapView1.ScreenToLatLon(MapView1.ScreenToControl(Mouse.CursorPos));
    AddPointOfInterest(FPoiLayer, P, Format('Clicked Point: Latitude=%.6f, Longitude=%.6f', [P.Lat, P.Lon]), Random(ImageList1.Count));
    FClickRequested := false;
  end;
end;

end.

