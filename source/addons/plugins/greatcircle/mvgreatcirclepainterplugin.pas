{  Copyright (C) 2025 Ekkehard Domning (www.domis.de)

 License: modified LGPL with linking exception (like RTL, FCL and LCL)

 See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
 for details about the license.

 This unit implements an MapViewer-Plugin to display a great circle on the map.
 For details about great circles please see:

   https://en.wikipedia.org/wiki/Great_circle

 The great circle is defined by a Start and a Destination coordinate.

 If Start and Destination are equal, no greatcircle is displayed.

 If Start and Destination are 0 or separated by 180 degrees, the circle around
 the two poles are displayed.

 The part containing the shortest distance between Start and Destination is
 called Orthodrome.

 The pens for drawing the Great Circle and the Orthodrome can be chosen
 independently.

 A great circle on the map is approximated by drawing short straight lines
 segments. The length of those segments can be set.
 In general, the segments should be smaller on larger scales (smaller zoom)
 and could be longer on smaller scales (bigger zoom), since the deviations from
 linearity are bigger on large scales, especially in polar regions.

 The Z-Order can be selected in three steps:
 On the native map behind or before the markers (this allows to save the
 great circle together with the map in a file) or on the Canvas.

 The calculated points are accessible. They contain the location, the distance
 (in meters) from Start and the kind of the point (wether it is start or destination,
 is part of the Orthodrome or none of the above). The distance starts at 0 at
 Start, increases in direction of the Destination and continues increasing until
 Start is reached again.
 The points are only available for the longitudes visible in the map!
}

unit mvGreatCirclePainterPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Math, Contnrs, ClipBrd,
  mvPluginCommon, mvPlugins, mvMapViewer, mvTypes, mvGeoMath, mvDrawingEngine;

const
  DefaultSegmentLength = 10;  // 10 pixels for one line segment on screen
  MaxSegmentLength     = 200; // maximum 200 pixels

type
  TGreatCirclePointKind = (gcpkStandard, gcpkOrthodrome, gcpkStart, gcpkCenter, gcpkDestination);

  { TGreatCirclePoint }

  TGreatCirclePoint = class(TObject)
  private
    FRealPoint : TRealPoint;
    FDistance : Double;
    FPointKind : TGreatCirclePointKind;
  protected
  public
    constructor Create(const ARealPoint : TRealPoint; const ADistance : Double;
                       const APointKind : TGreatCirclePointKind = gcpkStandard);
    constructor Create(const ARealPointLat, ARealPointLon : Double; const ADistance : Double;
                       const APointKind : TGreatCirclePointKind = gcpkStandard);
    property RealPoint : TRealPoint read FRealPoint;
    property PointKind : TGreatCirclePointKind read FPointKind;
    property Distance : Double read FDistance;
  end;

  TGreatCirclePainterZOrder = (gcpzCanvas,gcpzInFrontOfMarkers,gcpzBehindMarkers);
  TGreatCirclePainterOption = (gcpoMarkStart, gcpoMarkCenter, gcpoMarkDestination);
  TGreatCirclePainterOptions = set of TGreatCirclePainterOption;

  { TGreatCirclePainterPlugin }

  TGreatCirclePainterPlugin = class;
  TGreatCirclePainterGetCoordsEvent = procedure (Sender : TGreatCirclePainterPlugin;
                                                 var FStart, FDestination : TRealPoint) of Object;
  TGreatCirclePainterPlugin = class(TMvDrawPlugin)
  private
    FZOrder : TGreatCirclePainterZOrder;
    FOrthodromePen: TPen;
    FOptions : TGreatCirclePainterOptions;
    FStart : TRealPoint;
    FDestination : TRealPoint;
    FCenterPoint : TRealPoint;
    FSegmentLength : Integer;
    FOrthodromeDistance : Double;
    FInitialBearing : Double;
    FGetStartAndDestinationCoordsEvent : TGreatCirclePainterGetCoordsEvent;
    FOnChange : TNotifyEvent;
    FGreatCircleLinePoints : TObjectList;
    function LimitLat(const ALat : Double) : Double;
    function LimitLon(const ALon : Double) : Double;
    procedure DoGetCoordEvent;
    procedure SetOrthodromePen(AValue: TPen);
    procedure SetStartLat(Value : Double);
    procedure SetStartLon(Value : Double);
    procedure SetDestinationLat(Value : Double);
    procedure SetDestinationLon(Value : Double);
    function GetGreatCirclePointsCount : Integer;
    function GetGreatCirclePoints(AIndex : Integer) : TGreatCirclePoint;
    procedure SetOptions(Value : TGreatCirclePainterOptions);
    procedure SetSegmentLength(Value : Integer);
    procedure PaintGreatCircleWithCanvas;
    procedure PaintGreatCircleWithDrawingEngine;
    procedure OrthodromePenChanged(Sender : TObject);
    procedure SetGreatCirclePen(Value : TPen);
    function GetGreatCirclePen : TPen;
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); override;
    procedure Resize(AMapView: TMapView; var Handled: Boolean); override;
    procedure Update; override;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure CalculateGreatCircle;
    procedure SetStartAndDestination(const AStartLat, AStartLon, ADestinationLat, ADestinationLon : Double);
    procedure SetStartAndDestination(const AStart, ADestination : TRealPoint);

    property OrthodromeDistance : Double read FOrthodromeDistance;
    property InitialBearing : Double read FInitialBearing;
    property CenterPoint : TRealPoint read FCenterPoint;

    property GreatCirclePointsCount : Integer read GetGreatCirclePointsCount;
    property GreatCirclePoints[AIndex : Integer] : TGreatCirclePoint read GetGreatCirclePoints;
  published
    property DestinationLat : Double read FDestination.Lat write SetDestinationLat;
    property DestinationLon : Double read FDestination.Lon write SetDestinationLon;
    property GreatCirclePen : TPen read GetGreatCirclePen write SetGreatCirclePen;
    property Options : TGreatCirclePainterOptions read FOptions write SetOptions;
    property OrthodromePen: TPen read FOrthodromePen write SetOrthodromePen;
    property SegmentLength : Integer read FSegmentLength write SetSegmentLength default DefaultSegmentLength;
    property StartLat : Double read FStart.Lat write SetStartLat;
    property StartLon : Double read FStart.Lon write SetStartLon;
    property ZOrder : TGreatCirclePainterZOrder read FZOrder write FZOrder default gcpzCanvas;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnGetStartAndDestinationCoords : TGreatCirclePainterGetCoordsEvent read FGetStartAndDestinationCoordsEvent write FGetStartAndDestinationCoordsEvent;
  end;


implementation


{ TGreatCirclePoint }

constructor TGreatCirclePoint.Create(const ARealPoint: TRealPoint; const ADistance : Double;
  const APointKind: TGreatCirclePointKind);
begin
  inherited Create;
  FRealPoint := ARealPoint;
  FDistance := ADistance;
  if FDistance < 0.0 then
    FDistance := FDistance+EARTH_CIRCUMFERENCE;
  FPointKind := APointKind;
end;

constructor TGreatCirclePoint.Create(const ARealPointLat, ARealPointLon: Double;
  const ADistance : Double;
  const APointKind: TGreatCirclePointKind);
begin
  inherited Create;
  FRealPoint.InitLatLon(ARealPointLat, ARealPointLon);
  FDistance := ADistance;
  if FDistance < 0.0 then
    FDistance := FDistance+EARTH_CIRCUMFERENCE;
  FPointKind := APointKind;
end;


{ TGreatCirclePainterPlugin }

// Default SortCompare sortes the points along the longitude and on same
// longitudes shorter distance from start first.
function GreatCircleLinePointsListSortCompare(Item1, Item2: Pointer): Integer;
var
  pt1 : TGreatCirclePoint absolute Item1;
  pt2 : TGreatCirclePoint absolute Item2;
begin
  Result := 0;
  if pt1.FRealPoint.Lon > pt2.FRealPoint.Lon then
    Result := 1
  else if pt1.FRealPoint.Lon < pt2.FRealPoint.Lon then
    Result := -1
  else if pt1.FDistance > pt2.FDistance then
    Result := 1
  else if pt1.FDistance < pt2.FDistance then
    Result := -1;
end;

// Special SortCompare for Polroutes sortes the points only along the distance from start
function GreatCircleLinePolarPointsListSortCompare(Item1, Item2: Pointer): Integer;
var
  pt1 : TGreatCirclePoint absolute Item1;
  pt2 : TGreatCirclePoint absolute Item2;
begin
  Result := 0;
  if pt1.FDistance > pt2.FDistance then
    Result := 1
  else if pt1.FDistance < pt2.FDistance then
    Result := -1
end;

// This is the big calculation method
procedure TGreatCirclePainterPlugin.CalculateGreatCircle;

  // Calculates the distance between the given start and the given point
  // including the direction, not the shorts way!
  function DistanceFromStartEx(const ALat, ALon : Double; const AStartLat, AStartLon : Double; AInitialBearing : Double) : Double;
  var
    d, d0 : Double;
    refPt : TRealPoint;
  begin
    // First the absolute distance
    d := CalcGeoDistance(AStartLat, AStartLon, ALat, ALon, duMeters);
    // Now calculate the point in the given distance
    CalcLatLon(AStartLat, AStartLon, d, AInitialBearing, refPt.Lat, refPt.Lon);
    // now calculate the delta between the points
    d0 := CalcGeoDistance(refPt.Lat, refPt.Lon, ALat, ALon, duMeters);
    if Abs(d0) > 1.0 then // if not same
      d := EARTH_CIRCUMFERENCE-d; // the distance is on the other side
    Result := d;
  end;

  // Calculates the distance between start and the given point
  // including the direction, not the shorts way!
  function DistanceFromStart(const ALat, ALon : Double) : Double;
  begin
    Result := DistanceFromStartEx(ALat, ALon, FStart.Lat, FStart.Lon, FInitialBearing);
  end;

  // PointKindFromDist
  // return gcpkOrthodrome if between start and destination else gcpkStandard
  function PointKindFromDist(const ADistFromStart : Double) : TGreatCirclePointKind;
  var
    d : Double;
  begin
    d := ADistFromStart;
    if d > EARTH_CIRCUMFERENCE then
      d := d - EARTH_CIRCUMFERENCE
    else if d < 0.0 then
      d := d + EARTH_CIRCUMFERENCE;
    if InRange(d,0.0,FOrthodromeDistance) then
      Result := gcpkOrthodrome
    else
      Result := gcpkStandard;
  end;

var
  lScreenArea : TRealArea;
  lMapCenter : TRealPoint;
  rPt, rPtSub : TRealPoint;
  d : Double;
  lStepLon, lStepLonSub : Double;
  lCurLon : Double;
  lStopLon : Double;
  curDist : Double;
  lMapViewWidth : Integer;
  lMapViewHeight : Integer;
  i : Integer;
  lStart, lDest : TRealPoint;
  lWorldSize : Int64;
  lGCP0, lGCP1 : TGreatCirclePoint;
  ptKind : TGreatCirclePointKind;
  lon0, lon1 : Double;
  lLastRPt : TrealPoint;
  cnt : Integer;
  lFullSpan : Boolean;
  pt : TPoint;
  lDateBorderOnMap : Boolean;
  lBear : Double;
begin
  FGreatCircleLinePoints.Clear;
  FOrthodromeDistance := 0.0;
  FInitialBearing := 0.0;
  if not Assigned(MapView) then Exit;
  if not MapView.Visible then Exit;
  lMapViewWidth := MapView.Width;
  lMapViewHeight := MapView.Height;
  if (lMapViewWidth <= 0) or (lMapViewHeight <= 0) then Exit;
  try
    try
      // Start and Destination on the same point, no display
      if (FStart.Lat = FDestination.Lat) and
         (FStart.Lon = FDestination.Lon) then Exit;

      lWorldSize := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
      lFullSpan := ((lMapViewWidth + 2*FSegmentLength) >= lWorldSize);
      // Set the ScreenArea (in Degree) from the visible parts, include an area of SegmentLen around.
      // Take care to limit the area to the maximum width
      if lFullSpan then
      begin
        lScreenArea.TopLeft := MapView.ScreenToLatLon(Point(0,0-FSegmentLength));
        lScreenArea.TopLeft.Lon := -180.0;
        lScreenArea.BottomRight := MapView.ScreenToLatLon(Point(lMapViewWidth,lMapViewHeight+FSegmentLength));
        lScreenArea.BottomRight.Lon := 180.0;
        lMapViewWidth := lWorldSize;
      end
      else
      begin
        lScreenArea.TopLeft := MapView.ScreenToLatLon(Point(0-FSegmentLength,0-FSegmentLength));
        lScreenArea.BottomRight := MapView.ScreenToLatLon(Point(lMapViewWidth+FSegmentLength,lMapViewHeight+FSegmentLength));
      end;
      lMapCenter.InitLatLon((lScreenArea.TopLeft.Lat+lScreenArea.BottomRight.Lat) / 2.0,
                            (lScreenArea.TopLeft.Lon+lScreenArea.BottomRight.Lon) / 2.0);
      // Calculate the step distance (in fraction of longitude degree), by using the pixel distance
      lon0 := lScreenArea.TopLeft.Lon;
      lon1 := lScreenArea.BottomRight.Lon;
      if lon1 >= lon0 then // Normal case
        d := lon1-lon0
      else  // crossing date border
        d := lon1-lon0+360.0;
      lStepLon := d / lMapViewWidth * FSegmentLength;
      lStopLon := lon0+d;
      // Now sort out two special cases vertical and horizontal
      FInitialBearing := CalcBearing(FStart.Lat, FStart.Lon, FDestination.Lat, FDestination.Lon);
      FOrthodromeDistance := CalcGeoDistance(FStart.Lat, FStart.Lon, FDestination.Lat, FDestination.Lon,duMeters);
      // Calculate CenterPoint
      CalcLatLon(FStart.Lat, FStart.Lon, FOrthodromeDistance / 2.0, FInitialBearing,
                 FCenterPoint.Lat,FCenterPoint.Lon);

      // Special processing of all Polar-Routes.
      // Those are
      // 1.) any Start and Destination sharing a common Longitude
      // 2.) same Start and Destination having 180 Degree between Start and Destination
      // 3.) Start or Destination one at a pole, the other somwhere eles
      // 4.) Start and Destination separated, each on one pole
      if (Abs(FStart.Lat) = 90.0) or (Abs(FDestination.Lat) = 90) or
         (Abs(FStart.Lon-FDestination.Lon) = 0.0) or
         (Abs(FStart.Lon-FDestination.Lon) = 180.0) then
      begin
        if (Abs(FStart.Lat) = 90.0) and (Abs(FDestination.Lat) = 90) then
        begin  // case 4.)
          FCenterPoint.Lon := FStart.Lon;
          // create 5 points (Start Longitude leads) and exit
          FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FStart.Lat,FStart.Lon,0.0,gcpkStart));
          FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FCenterPoint.Lat,FCenterPoint.Lon,
                                                 EARTH_CIRCUMFERENCE / 4.0,gcpkCenter));
          FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FDestination.Lat,FStart.Lon,FOrthodromeDistance,gcpkDestination));

          FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FDestination.Lat,NormalizeLon(FStart.Lon+180.0),FOrthodromeDistance,gcpkStandard));
          FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FStart.Lat,NormalizeLon(FStart.Lon+180.0),EARTH_CIRCUMFERENCE,gcpkStandard));
          Exit;
        end;
        // Caution: We may need to modify FStart or FDestination for the proper calculation.
        //  Doing so, cause an endless loop of repainting, if external coordiates are provided
        //  via the GetCoords Event!!
        // So we use a local copy
        lStart.InitLatLon(FStart.Lat,FStart.Lon);
        lDest.InitLatLon(FDestination.Lat,FDestination.Lon);
        if InRange(Abs(FCenterPoint.Lon-lDest.Lon),0.0,1E-10) then
          FCenterPoint.Lon := lDest.Lon
        else
          FCenterPoint.Lon := lStart.Lon;

        // Cases 3.)
        // To reuse most of the standard code we slightly remove the points from
        // the pole, to get a proper distance calculation for sorting the points
        if (Abs(FStart.Lat) = 90.0) and (Abs(FDestination.Lat) <> 90) then
        begin
          if FStart.Lat = 90.0 then
            lStart.Lat := 89.99
          else
            lStart.Lat := -89.99;
          lStart.Lon := FDestination.Lon;
          FCenterPoint.Lon := FDestination.Lon;
        end
        else if (Abs(FStart.Lat) <> 90.0) and (Abs(FDestination.Lat) = 90) then
        begin
          if FDestination.Lat = 90.0 then
            lDest.Lat := 89.99
          else
            lDest.Lat := -89.99;
          lDest.Lon := FStart.Lon;
          FCenterPoint.Lon := FStart.Lon;
        end
        else if Abs(FStart.Lon-FDestination.Lon) = 180.0 then
        begin
          if FStart.Lat > FDestination.Lat then
            FCenterPoint.Lon := FDestination.Lon
          else
            FCenterPoint.Lon := FStart.Lon;
          FCenterPoint.Lat := 0.0;
        end;

        // The bearing has changed too, using an internal copy
        if Abs(FStart.Lon-FDestination.Lon) = 180.0 then
         lBear := 0.0
        else
         lBear := CalcBearing(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon);
        // We have to insert 7 points:
        //  North pole, South pole, South pole + 180, North pole + 180
        //  plus Start, Center and Destination.
        // Start and Destination may share the same Longitude or differ by 180
        // To ease the calculation, we will insert not twice the Poles, instead
        // we will insert two points in a very close distance to the Poles.
        // This will give two different distances and a proper sort order
        // First, insert Start, CenterPoint and Destination (the local copies)
        FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(lStart.Lat,lStart.Lon,0.0,gcpkStart));
        FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FCenterPoint.Lat,FCenterPoint.Lon,
                                               FOrthodromeDistance / 2.0,gcpkCenter));
        FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(lDest.Lat,lDest.Lon,FOrthodromeDistance,gcpkDestination));
        // Now insert the four pole points
        for i := 0 to 3 do
        begin
          case i of
            0,3 :
              begin // North pole 0, 1
                rPt.Lat := 90-1E-10;
                if i = 0 then
                  rPt.Lon := lStart.Lon
                else
                  rPt.Lon := NormalizeLon(lStart.Lon + 180.0);
              end;
            1,2 :
              begin // South pole 0, 1
                rPt.Lat := -90+1E-10;
                if i = 1 then
                  rPt.Lon := lStart.Lon
                else
                  rPt.Lon := NormalizeLon(lStart.Lon + 180.0);
              end;
          end;
          // calculate the distance
          curDist := DistanceFromStartEx(rPt.Lat, rPt.Lon, lStart.Lat, lStart.Lon, lBear);
          // calculate the kind of the points (Orthrodrome or normal)
          ptKind := PointKindFromDist(curDist);
          FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(rPt.Lat,rPt.Lon,curDist,ptKind));
        end;
        // Sort all points along their distances from Start
        FGreatCircleLinePoints.Sort(@GreatCircleLinePolarPointsListSortCompare);
        // move the last point from the end to the beginning to draw a line from this pole prior to start
        lGCP0 := TGreatCirclePoint(FGreatCircleLinePoints.Extract(FGreatCircleLinePoints.Items[FGreatCircleLinePoints.Count-1]));
        FGreatCircleLinePoints.Insert(0,lGCP0);
        // The North-South straight line has been processed
        Exit;
      end;

      // Now all other points on earth, including the equator.

      // Add Start and Destination points
      FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FStart,0.0,gcpkStart));
      FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FCenterPoint,FOrthodromeDistance / 2.0,gcpkCenter));
      FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FDestination,FOrthodromeDistance,gcpkDestination));

      // Equator will be processed as normal
      lStart := FStart;
      lDest := FDestination;

      // Now we travel along the part that may(!) visible on the screen.
      // We start far in the west (left)
      lCurLon := lScreenArea.TopLeft.Lon;
      lLastRPt.Lon := lCurLon;
      LatFromLonAtGreatCircle(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon,
                              lLastRPt.Lon, lLastRPt.Lat);
      // Loop from left to right
      repeat
        // Increment the curDist (= distance on the circle to travel from normalized start)
        rPt.Lon := lCurLon;
        if rPt.Lon > 180.0 then
          rPt.Lon := rPt.Lon - 360.0;
        LatFromLonAtGreatCircle(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon,
                                rPt.Lon, rPt.Lat);

        // Determine kind of point
        d := DistanceFromStart(rPt.Lat,rPt.Lon);
        ptKind := PointKindFromDist(d);
        FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(rPt,d,ptKind));

        // Sub task.
        // If the great circle has a great slope, e.g. runing from and to high latitudes,
        // than the latitude may increase very fast, so we have to insert
        // some intermediate points.
        d := rPt.Lat-lLastRPt.Lat;
        if Abs(d) > lStepLon then
        begin
          cnt := Trunc(Abs(d) / lStepLon);
          lStepLonSub := lStepLon / cnt;
          for i := 1 to cnt-1 do
          begin
            rPtSub.Lon := lLastRPt.Lon + (i*lStepLonSub);
            if rPtSub.Lon > 180.0 then
              rPtSub.Lon := rPtSub.Lon - 360.0;
            LatFromLonAtGreatCircle(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon,
                                    rPtSub.Lon, rPtSub.Lat);
            // But we will only insert those points if they are visible on the screen.
            if lScreenArea.ContainsPoint(rPtSub) then
            begin
              // Determine kind of point
              d := DistanceFromStart(rPtSub.Lat,rPtSub.Lon);
              ptKind := PointKindFromDist(d);
              FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(rPtSub,d,ptKind));
            end;
          end;
        end;
        lLastRPt.InitLatLon(rPt.Lat,rPt.Lon); // save the current point as last point
        // terminate the loop if the right side is reached, or the full circle has been processed
        lCurLon := lCurLon + lStepLon;
      until lCurLon > lStopLon;

      // Add the points -180 and 180 to avoid a gap in the great circle in cyclic map mode
      lDateBorderOnMap := lScreenArea.BottomRight.Lon < lScreenArea.TopLeft.Lon;
      for i := 0 to 1 do
      begin
        if i = 0 then
        begin
          rPt.Lon := -180.0;
          if not lDateBorderOnMap then
          begin
            pt := MapView.LatLonToScreen(lScreenArea.TopLeft.Lat,-180.0);
            if pt.X < MapView.Engine.MapLeft then Continue;
          end;
        end
        else
        begin
          rPt.Lon := 180.0;
          if not lDateBorderOnMap then
          begin
            pt := MapView.LatLonToScreen(lScreenArea.TopLeft.Lat,180.0);
            if pt.X > MapView.Engine.MapLeft+lWorldSize then Continue;
          end;
        end;
        LatFromLonAtGreatCircle(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon,
                                rPt.Lon, rPt.Lat);
        d := DistanceFromStart(rPt.Lat,rPt.Lon);
        ptKind := PointKindFromDist(d);
        FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(rPt,d,ptKind));
      end;
      // Finally the found points must be sorted, if not processed in a different way.
      FGreatCircleLinePoints.Sort(@GreatCircleLinePointsListSortCompare);
      // Check for duplicate items, if found remove them
      cnt := FGreatCircleLinePoints.Count;
      if cnt > 1 then
      begin
        for i := cnt-1 downto 1 do
        begin
          lGCP0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
          lGCP1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i-1]);
          if (lGCP1.Distance = lGCP0.Distance) and
             (lGCP1.FRealPoint.Lon = lGCP0.FRealPoint.Lon) and
             (lGCP1.FRealPoint.Lat = lGCP0.FRealPoint.Lat) then
          begin
            FGreatCircleLinePoints.Extract(lGCP0);
            FreeAndNil(lGCP0);
          end;
        end;
      end;
    except
      // Keep silence on any computation errors
    end;
  finally
    // Give the user a message, to update the information about the current great circle
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TGreatCirclePainterPlugin.LimitLat(const ALat: Double): Double;
begin
  if ALat > 90.0 then
    Result := 90.0
  else if ALat < -90.0 then
    Result := -90.0
  else
    Result := ALat;
end;

function TGreatCirclePainterPlugin.LimitLon(const ALon: Double): Double;
begin
  if ALon > 180.0 then
    Result := 180.0
  else if ALon < -180.0 then
    Result := -180.0
  else
    Result := ALon;
end;

procedure TGreatCirclePainterPlugin.DoGetCoordEvent;
var
  lSPt, lDPt : TRealPoint;
begin
  if Assigned(FGetStartAndDestinationCoordsEvent) then
  begin
    lSPt := FStart;
    lDPt := FDestination;
    FGetStartAndDestinationCoordsEvent(Self,lSPt, lDPt);
    SetStartAndDestination(lSPt, lDPt);
  end;
end;

procedure TGreatCirclePainterPlugin.SetOrthodromePen(AValue: TPen);
begin
  if not Assigned(AValue ) then
    Exit;
  if (AValue.Color = FOrthodromePen.Color) and (AValue.Width = FOrthodromePen.Width) and
     (AValue.Style = FOrthodromePen.Style) and (AValue.Mode = FOrthodromePen.Mode) and
     (AValue.JoinStyle = FOrthodromePen.JoinStyle) and (AValue.EndCap = FOrthodromePen.EndCap)
  then
    Exit;
  FOrthodromePen.Assign(AValue);
  OrthodromePenChanged(Self);
end;

procedure TGreatCirclePainterPlugin.SetStartLat(Value: Double);
var
  d : Double;
begin
  d := LimitLat(Value);
  if FStart.Lat = d then Exit;
  FStart.Lat := d;
  Update;
end;

procedure TGreatCirclePainterPlugin.SetStartLon(Value: Double);
var
  d : Double;
begin
  d := LimitLon(Value);
  if FStart.Lon = d then Exit;
  FStart.Lon := d;
  Update;
end;

procedure TGreatCirclePainterPlugin.SetDestinationLat(Value: Double);
var
  d : Double;
begin
  d := LimitLon(Value);
  if FDestination.Lat = d then Exit;
  FDestination.Lat := d;
  Update;
end;

procedure TGreatCirclePainterPlugin.SetDestinationLon(Value: Double);
var
  d : Double;
begin
  d := LimitLon(Value);
  if FDestination.Lon = d then Exit;
  FDestination.Lon := d;
  Update;
end;

function TGreatCirclePainterPlugin.GetGreatCirclePointsCount: Integer;
begin
  Result := FGreatCircleLinePoints.Count;
end;

function TGreatCirclePainterPlugin.GetGreatCirclePoints(AIndex: Integer
  ): TGreatCirclePoint;
begin
  Result := TGreatCirclePoint(FGreatCircleLinePoints.Items[AIndex]);
end;

procedure TGreatCirclePainterPlugin.SetOptions(Value: TGreatCirclePainterOptions
  );
begin
  if FOptions = Value then Exit;
  FOptions := Value;
  inherited Update; // Points could be reused, no position changed
end;

procedure TGreatCirclePainterPlugin.SetSegmentLength(Value: Integer);
var
  v : Integer;
begin
  if Value <= 0 then
    v := 1
  else if Value > MaxSegmentLength then
    v := MaxSegmentLength
  else
    v := Value;
  if FSegmentLength = v then Exit;
  FSegmentLength := v;
  Update;
end;

// Painting the great circle is fairly straight forward.
// We simply run to the points and draw the segments.
// Care must be taken not to draw invisble parts of the graph and, even in the
// not cyclic maps, the crossing of the right and left borders must be catched.
procedure TGreatCirclePainterPlugin.PaintGreatCircleWithCanvas;
var
  i : Integer;
  pt : TPoint;
  pt0, pt1 : TPoint;
  ptCyc: TPointArray;
  dpx, dpy : Integer;
  ptGC0 : TGreatCirclePoint;
  ptGC1 : TGreatCirclePoint;
  cnt : Integer;
  lWorldSize : Int64;
  topY, bottomY : Integer;
  ptOutArr : array of Boolean = Nil;
  isCyclic : Boolean;
begin
  cnt := FGreatCircleLinePoints.Count;
  if cnt > 1 then
  begin
    isCyclic := MapView.Cyclic;
    lWorldSize := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
    pt0 := MapView.LatLonToScreen(90.0,0);
    topY := pt0.Y;
    pt0 := MapView.LatLonToScreen(-90.0,0);
    bottomY := pt0.Y;
    // Here we will setup an array, containing a flag for each point
    // wether he is in or out the visible screen.
    // Since the values running from -180 to +180 in longitude (but limited
    // to the visible longitudes), only the Latitude / Y-Values must be checked
    SetLength(ptOutArr,cnt);
    for i := 0 to cnt-1 do
    begin
      ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
      ptOutArr[i] := (pt0.Y <= topY) or (pt0.Y >= bottomY);
    end;
    // In a second step we find the segments wher one end is out, and one is in.
    // In this case the "out" point is marked as "in".
    for i := 1 to cnt-1 do
    begin
      if not ptOutArr[i] then Continue;
      ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i-1]);
      pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
      ptGC1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      pt1 := MapView.LatLonToScreen(ptGC1.RealPoint);

      if ((pt0.Y <= topY) and (pt1.Y >= bottomY)) or
         ((pt0.Y >= bottomY) and (pt1.Y <= topY)) then
        ptOutArr[i] := False;
    end;

    // No Brush, we draw only lines
    MapView.Canvas.Brush.Style:= bsClear;

    // Now loop through al points, but since we draw segmens, we start with the
    // second point (= index 1).
    ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[0]);
    for i := 1 to cnt-1 do
    begin
      ptGC1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      if (not ptOutArr[i-1]) or (not ptOutArr[i]) then
      begin // if at least one side is in, we have to paint
        pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
        pt1 := MapView.LatLonToScreen(ptGC1.RealPoint);
        // In the cyclic map we limit all points to the actual world size
        if isCyclic then
        begin
          pt0.X := Trunc(Frac(pt0.X / lWorldSize)*lWorldSize);
          if pt0.X < 0 then
            pt0.X := pt0.X + lWorldSize
          else if pt0.X > lWorldSize then
            pt0.X := pt0.X - lWorldSize;
          pt1.X := Trunc(Frac(pt1.X / lWorldSize)*lWorldSize);
          if pt1.X < 0 then
            pt1.X := pt1.X + lWorldSize
          else if pt1.X > lWorldSize then
            pt1.X := pt1.X - lWorldSize;
        end;
        // we take the differences in the X and Y axis for this point pair
        // to ease the cyclic processing
        dpy := pt1.Y-pt0.Y;
        dpx := pt1.X-pt0.X;
        // Cyclic or not, a big dpx has to be limited, to avoid big zig-zags
        if Abs(dpx) > (lWorldSize div 2) then
        begin
          if dpx < 0 then
            dpx := dpx + lWorldSize
          else
            dpx := dpx - lWorldSize;
        end;
        // Select the pen. The orthodrome ends when a standard point is used
        if (ptGC0.FPointKind <> gcpkStandard) and
           (ptGC1.FPointKind <> gcpkStandard) then
          MapView.Canvas.Pen := FOrthodromePen
        else
          MapView.Canvas.Pen := Pen;
        // Geta all cyclic points
        ptCyc := MapView.CyclicPointsOf(pt1);
        // Draw al lines for the current segment
        for pt in ptCyc do
          MapView.Canvas.Line(pt.X,pt.Y,pt.X-dpx,pt.Y-dpy);
        // If the options ticked to paint the start, center or end, paint same
        if ((ptGC1.FPointKind = gcpkStart) and (gcpoMarkStart in FOptions)) or
           ((ptGC1.FPointKind = gcpkCenter) and (gcpoMarkCenter in FOptions)) or
           ((ptGC1.FPointKind = gcpkDestination) and (gcpoMarkDestination in FOptions)) then
        begin
          MapView.Canvas.Pen := FOrthodromePen;
          for pt in ptCyc do
            MapView.Canvas.Ellipse(pt.X-5,pt.Y-5,pt.X+5,pt.Y+5);
        end;
      end;
      // take the current point as the last one
      ptGC0 := ptGC1;
    end;
  end;
end;

// Nearly the same for drawing with the drawing engine on the map.
procedure TGreatCirclePainterPlugin.PaintGreatCircleWithDrawingEngine;
var
  i : Integer;
  pt : TPoint;
  pt0, pt1 : TPoint;
  ptCyc: TPointArray;
  dpx, dpy : Integer;
  ptGC0 : TGreatCirclePoint;
  ptGC1 : TGreatCirclePoint;
  cnt : Integer;
  lWorldSize : Int64;
  topY, bottomY : Integer;
  ptOutArr : array of Boolean = Nil;
  lDrawingEngine : TMvCustomDrawingEngine;
  isCyclic : Boolean;
begin
  cnt := FGreatCircleLinePoints.Count;
  if cnt > 1 then
  begin
    isCyclic := MapView.Cyclic;
    lWorldSize := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
    lDrawingEngine := MapView.DrawingEngine;
    pt0 := MapView.LatLonToScreen(90.0,0);
    topY := pt0.Y;
    pt0 := MapView.LatLonToScreen(-90.0,0);
    bottomY := pt0.Y;
    SetLength(ptOutArr,cnt);
    for i := 0 to cnt-1 do
    begin
      ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
      ptOutArr[i] := (pt0.Y <= topY) or (pt0.Y >= bottomY);
    end;
    for i := 1 to cnt-1 do
    begin
      if not ptOutArr[i] then Continue;
      ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i-1]);
      pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
      ptGC1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      pt1 := MapView.LatLonToScreen(ptGC1.RealPoint);

      if ((pt0.Y <= topY) and (pt1.Y >= bottomY)) or
         ((pt0.Y >= bottomY) and (pt1.Y <= topY)) then
        ptOutArr[i] := False;
    end;

    lDrawingEngine.BrushStyle:= bsClear;
    lDrawingEngine.PenStyle:= Pen.Style;
    lDrawingEngine.PenWidth:= Pen.Width;
    lDrawingEngine.PenColor:= Pen.Color;

    ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[0]);
    for i := 1 to cnt-1 do
    begin
      ptGC1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      if (not ptOutArr[i-1]) or (not ptOutArr[i]) then
      begin
        pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
        pt1 := MapView.LatLonToScreen(ptGC1.RealPoint);
        if isCyclic then
        begin
          pt0.X := Trunc(Frac(pt0.X / lWorldSize)*lWorldSize);
          if pt0.X < 0 then
            pt0.X := pt0.X + lWorldSize
          else if pt0.X > lWorldSize then
            pt0.X := pt0.X - lWorldSize;
          pt1.X := Trunc(Frac(pt1.X / lWorldSize)*lWorldSize);
          if pt1.X < 0 then
            pt1.X := pt1.X + lWorldSize
          else if pt1.X > lWorldSize then
            pt1.X := pt1.X - lWorldSize;
        end;

        dpy := pt1.Y-pt0.Y;
        dpx := pt1.X-pt0.X;
        if Abs(dpx) > (lWorldSize div 2) then
        begin
          if dpx < 0 then
            dpx := dpx + lWorldSize
          else
            dpx := dpx - lWorldSize;
        end;

        if (ptGC0.FPointKind <> gcpkStandard) and
           (ptGC1.FPointKind <> gcpkStandard) then
        begin
          lDrawingEngine.PenStyle:= FOrthodromePen.Style;
          lDrawingEngine.PenWidth:= FOrthodromePen.Width;
          lDrawingEngine.PenColor:= FOrthodromePen.Color;
        end
        else
        begin
          lDrawingEngine.PenStyle:= Pen.Style;
          lDrawingEngine.PenWidth:= Pen.Width;
          lDrawingEngine.PenColor:= Pen.Color;
        end;

        ptCyc := MapView.CyclicPointsOf(pt1);
        for pt in ptCyc do
          lDrawingEngine.Line(pt.X,pt.Y,pt.X-dpx,pt.Y-dpy);

        if ((ptGC1.FPointKind = gcpkStart) and (gcpoMarkStart in FOptions)) or
           ((ptGC1.FPointKind = gcpkCenter) and (gcpoMarkCenter in FOptions)) or
           ((ptGC1.FPointKind = gcpkDestination) and (gcpoMarkDestination in FOptions)) then
        begin
          lDrawingEngine.PenStyle:= FOrthodromePen.Style;
          lDrawingEngine.PenWidth:= FOrthodromePen.Width;
          lDrawingEngine.PenColor:= FOrthodromePen.Color;
          for pt in ptCyc do
          begin
// A small bug in the TMvIntfGraphicsDrawingEngine, the ellipse is not drawn in
// the correct pen width, while a rectangle is.
//            lDrawingEngine.Rectangle(pt.X-5,pt.Y-5,pt.X+5,pt.Y+5);
            lDrawingEngine.Ellipse(pt.X-5,pt.Y-5,pt.X+5,pt.Y+5);
          end;
        end;
      end;
      ptGC0 := ptGC1;
    end;
  end;
end;

procedure TGreatCirclePainterPlugin.OrthodromePenChanged(Sender: TObject);
begin
  inherited Update; // No recalculation of the map needed, only a redraw of the map
end;

procedure TGreatCirclePainterPlugin.SetGreatCirclePen(Value: TPen);
begin
  Pen := Value;
end;

function TGreatCirclePainterPlugin.GetGreatCirclePen: TPen;
begin
  Result := Pen;
end;

procedure TGreatCirclePainterPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  if FZOrder <> gcpzInFrontOfMarkers then Exit;
  DoGetCoordEvent;
  PaintGreatCircleWithDrawingEngine;
end;

procedure TGreatCirclePainterPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  if FZOrder <> gcpzCanvas then Exit;
  DoGetCoordEvent;
  PaintGreatCircleWithCanvas;
end;

procedure TGreatCirclePainterPlugin.BeforeDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  if FZOrder <> gcpzBehindMarkers then Exit;
  DoGetCoordEvent;
  PaintGreatCircleWithDrawingEngine;
end;

procedure TGreatCirclePainterPlugin.CenterMove(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  Update;
end;

procedure TGreatCirclePainterPlugin.Resize(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  Update;
end;

procedure TGreatCirclePainterPlugin.ZoomChange(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  Update;
end;

procedure TGreatCirclePainterPlugin.Update;
begin
  CalculateGreatCircle;
  inherited;
end;

procedure TGreatCirclePainterPlugin.SetStartAndDestination(const AStartLat,
  AStartLon, ADestinationLat, ADestinationLon: Double);
var
  lSPt, sDPt : TRealPoint;
begin
  lSPt.InitLatLon(AStartLat,AStartLon);
  sDPt.InitLatLon(ADestinationLat, ADestinationLon);
  SetStartAndDestination(lSPt, sDPt);
end;

procedure TGreatCirclePainterPlugin.SetStartAndDestination(const AStart,
  ADestination: TRealPoint);
var
  lNewStart, lNewDest : TRealPoint;
begin
  lNewStart.Lat := LimitLat(AStart.Lat);
  lNewStart.Lon := LimitLon(AStart.Lon);
  lNewDest.Lat := LimitLat(ADestination.Lat);
  lNewDest.Lon := LimitLon(ADestination.Lon);
  if (FStart.Lat <>lNewStart.Lat) or
     (FStart.Lon <>lNewStart.Lon) or
     (FDestination.Lat <> lNewDest.Lat) or
     (FDestination.Lon <> lNewDest.Lon) then
  begin
    FStart.Lat := lNewStart.Lat;
    FStart.Lon := lNewStart.Lon;
    FDestination.Lat := lNewDest.Lat;
    FDestination.Lon := lNewDest.Lon;
    Update;
  end;
end;

constructor TGreatCirclePainterPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FGreatCircleLinePoints := TObjectList.Create(True);
  FSegmentLength := DefaultSegmentLength;
  FOrthodromePen := TPen.Create;
  FOrthodromePen.OnChange := @OrthodromePenChanged;
end;

destructor TGreatCirclePainterPlugin.Destroy;
begin
  if Assigned(FGreatCircleLinePoints) then
    FGreatCircleLinePoints.Free;
  FOrthodromePen.Free;
  inherited;
end;

initialization
  RegisterPluginClass(TGreatCirclePainterPlugin, 'Great Circle Painter');

end.

