unit mvtests_geomath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TMvTests_GeoMath = class(TTestCase)
  private
    procedure InitGreatCircleData;
  published
    procedure Test_Distance;
    procedure Test_LatToStr_DMS;
    procedure Test_LatToStr_Deg;
    procedure Test_LonToStr_DMS;
    procedure Test_LonToStr_Deg;
    procedure Test_StrToGPS_Lat;
    procedure Test_StrToGPS_Lon;
    procedure Test_StrToGPS_Lat_UTF8;
    procedure Test_StrToGPS_Lon_UTF8;
    procedure Test_StrDMSToDeg_Lat;
    procedure Test_StrDMSToDeg_Lon;
    procedure Test_StrDMSToDeg_Lat_UTF8;
    procedure Test_StrDMSToDeg_Lon_UTF8;
    procedure Test_SplitGPS;
    procedure Test_ZoomFactor;

    // Great circle tests
    procedure Test_GreatCircleBearing;
    procedure Test_GreatCircleMidPoint;
    procedure Test_GreatCircleVertex;
    procedure Test_GreatCircleWayPoints;
  end;

implementation

uses
  LazUTF8, Math, mvGeoMath;

type
  TDistanceRec = record
    Name1: String;
    Lat1, Lon1: Double;
    Name2: String;
    Lat2, Lon2: Double;
    Distance_km: Double;
  end;

const
  Distance_TestData: array[0..2] of TDistanceRec = (
    // Calculated on https://keisan.casio.com/exec/system/1224587128 for R=6378km
    (Name1: 'Sydney'; Lat1:-33.865143; Lon1:151.209900;
      Name2: 'San Francisco'; Lat2:37.828724; Lon2:-122.355537;
      Distance_km: 11968),
    (Name1: 'London'; Lat1: 51.503368; Lon1: -0.127721;
      Name2: 'Istanbul'; Lat2: 41.276901; Lon2: 28.729324;
      Distance_km: 2468.6),
    (Name1: 'Tokyo'; Lat1:35.652832; Lon1:139.839478;
      Name2: 'Singapore'; Lat2:1.290270; Lon2:103.851959;
      Distance_km: 5331.97)
  );

type
  TLatLonRec = record
    Name: String;
    Lat: Double;
    Lat_Deg: String;
    Lat_DMS: String;
    Lat_D, Lat_M: Integer;
    Lat_S: Double;
    Lon: Double;
    Lon_Deg: String;
    Lon_DMS: String;
    Lon_D, Lon_M: Integer;
    Lon_S: Double;
  end;

const
  LatLon_TestData: array[0..7] of TLatLonRec = (
    (Name:'Sydney';   // https://www.latlong.net/place/sydney-nsw-australia-700.html
      Lat:-33.865143; Lat_Deg:'33.865143° S'; Lat_DMS:'33° 51'' 54.5148" S';  Lat_D:-33; Lat_M:51; Lat_S:54.5148;
      Lon:151.209900; Lon_Deg:'151.209900° E'; Lon_DMS:'151° 12'' 35.6400" E'; Lon_D:151; Lon_M:12; Lon_S:35.64),
    (Name:'San Francisco';  // https://www.latlong.net/place/san-francisco-bay-area-ca-usa-32614.html
      Lat:37.828724; Lat_Deg:'37.828724° N'; Lat_DMS:'37° 49'' 43.4064" N'; Lat_D:37; Lat_M:49; Lat_S:43.4064;
      Lon:-122.355537; Lon_Deg:'122.355537° W'; Lon_DMS:'122° 21'' 19.9332" W'; Lon_D:-122; Lon_M:21; Lon_S:19.9332),
    (Name:'London';  // https://www.latlong.net/place/10-downing-street-london-uk-32612.html
      Lat:51.503368; Lat_Deg:'51.503368° N'; Lat_DMS:'51° 30'' 12.1248" N'; Lat_D:51; lat_M:30; Lat_S:12.1248;
      Lon:-0.127721; Lon_Deg:'0.127721° W'; Lon_DMS:'0° 7'' 39.7956" W'; Lon_D:0; Lon_M:7; Lon_S:39.7956),
    (Name:'Istanbul';  // https://www.latlong.net/place/istanbul-airport-turkey-32591.html
      Lat:41.276901; Lat_Deg:'41.276901° N'; Lat_DMS:'41° 16'' 36.8436" N'; Lat_D:41; Lat_M:16; Lat_S:36.8436;
      Lon:28.729324; Lon_Deg:'28.729324° E'; Lon_DMS:'28° 43'' 45.5664" E'; Lon_D:28; Lon_M:43; Lon_S:45.5664),
    (Name:'Tokyo';    // https://www.latlong.net/place/tokyo-japan-8040.html
      Lat:35.652832; Lat_Deg:'35.652832° N'; Lat_DMS:'35° 39'' 10.1952" N'; Lat_D:35; Lat_M:39; Lat_S:10.1952;
      Lon:139.839478; Lon_Deg:'139.839478° E'; Lon_DMS:'139° 50'' 22.1208" E'; Lon_D:139; Lon_M:50; Lon_S:22.1208),
    (Name:'Singapore';  // https://www.latlong.net/place/singapore-788.html
      Lat:1.290270; Lat_Deg:'1.290270° N'; Lat_DMS:'1° 17'' 24.9720" N'; Lat_D:1; Lat_M:17; Lat_S:24.9720;
      Lon:103.851959; Lon_Deg:'103.851959° E'; Lon_DMS:'103° 51'' 7.0524" E'; Lon_D:103; Lon_M:51; Lon_S:7.0524),
    (Name:'Lima';   // https://www.latlong.net/place/lima-city-lima-province-peru-6919.html
      Lat:-12.046374; Lat_Deg:'12.046374° S'; Lat_DMS:'12° 2'' 46.9464" S'; Lat_D:-12; Lat_M:2; Lat_S:46.9464;
      Lon:-77.042793; Lon_Deg:'77.042793° W'; Lon_DMS:'77° 2'' 34.0548" W'; Lon_D:-77; Lon_M:2; Lon_S:34.0548),
    (Name: 'Johannesburg';  // https://www.latlong.net/place/johannesburg-south-africa-1083.html
      Lat:-26.195246; Lat_Deg:'26.195246° S'; Lat_DMS:'26° 11'' 42.8856" S'; Lat_D:-26; Lat_M:11; Lat_S:42.8856;
      Lon:28.034088; Lon_Deg:'28.034088° E'; Lon_DMS:'28° 2'' 2.7168" E'; Lon_D:28; Lon_M:2; Lon_S:2.7168)
  );

{ Data for great circle calculation }

type
  TGreatCirclePointRec = record
    Lat, Lon: Double;
    Bearing: Double;
  end;

  TGreatCircleRec = record
    Lat1: Double;
    Lon1: Double;
    Lat2: Double;
    Lon2: Double;
    BearingInitial: Double;
    BearingFinal: Double;
    MidPointLat: Double;
    MidPointLon: Double;
    WayPts: array of TGreatCirclePointRec;
  end;

const
  // data from https://www.rainerstumpe.de/HTML/kurse3.html
  GreatCircle_TestData1: TGreatCircleRec = (
    Lat1: 41.1578; Lon1: -8.6333;   // Porto (Portugal)
    Lat2: 10.6722; Lon2: -61.5333;  // Port of Spain (Trinidad)
    BearingInitial: 252.0;          // Bearing leaving Porto (degrees)
    BearingFinal: 227.0;            // Bearing arriving in Port of Spain (degrees)
    MidPointLat: NaN; MidPointLon: NaN;  // Mid-point  --> InitGreatCircle
    WayPts: nil;                    // To be initialized by InitGreatCircle
  );

  // Data from https://en.wikipedia.org/wiki/Great-circle_navigation#Example
  GreatCircle_TestData2: TGreatCircleRec = (
    Lat1: -33.0; Lon1: -71.6;      // Valparaiso
    Lat2:  31.4; Lon2: 121.8;      // Shangai
    BearingInitial: NaN;
    BearingFinal: NaN;
    MidPointLat: -6.81; MidPointLon: -159.18;
    WayPts: nil;
  );

var
  PointFormatsettings: TFormatSettings;

procedure TMvTests_GeoMath.Test_Distance;
const
  TOLERANCE = 2;
  RADIUS = 6378; // Earth radius in km, as used by the references
var
  i: Integer;
begin
  for i := 0 to High(Distance_TestData) do
    with Distance_TestData[i] do
      AssertEquals(
        'Distance mismatch between ' + Name1 + ' and ' + Name2,
        Distance_km,
        HaverSineDist(Lat1, Lon1, Lat2, Lon2, RADIUS), TOLERANCE
      );
end;

procedure TMvTests_GeoMath.Test_GreatCircleBearing;
const
  EPS = 0.5;
var
  bearing: Double;
begin
  with GreatCircle_TestData1 do
  begin
    bearing := CalcBearing(Lat1, Lon1, Lat2, Lon2);
    AssertEquals('Initial bearing mismatch', BearingInitial, bearing, EPS);

    bearing := CalcBearing(Lat2, Lon2, Lat1, Lat1) + 180;
    AssertEquals('Final bearing mismatch', BearingFinal, bearing, EPS);
  end;
end;

procedure TMvTests_GeoMath.Test_GreatCircleMidPoint;
const
  EPS = 1E-2;
var
  lat, lon: Double;
begin
  with GreatCircle_TestData2 do
  begin
//    CalcMidPoint(Lat1, Lon1, Lat2, Lon2, lat, lon);
    CalcIntermedPoint(Lat1, Lon1, Lat2, Lon2, 0.5, lat, lon);
    AssertEquals('Midpoint Latitude mismatch', MidPointLat, lat, EPS);
    AssertEquals('Midpoint Longitude mismatch', MidPointLon, lon, EPS);
  end;
end;

procedure TMvTests_GeoMath.Test_GreatCircleVertex;
const
  EPS = 0.5;
var
  lat, lon: Double;
  Soll_Lat, Soll_Lon: Double;
begin
  with GreatCircle_TestData1 do
  begin
    Soll_Lat := DMSToDeg(44, 10, 34.3);
    Soll_Lon := DMSToDeg(17, 18, 0);
    VertexOfGreatCircle(Lat1, Lon1, Lat2, Lon2, lat, lon);
  end;
  AssertEquals('Great circle vertex latitude mismatch', Soll_Lat, lat, EPS);
  AssertEquals('Great circle vertex longitude mismatch', Soll_Lon, lon, EPS);
end;

procedure TMvTests_GeoMath.InitGreatCircleData;

  procedure InitPt(AIndex: Integer; ALat, ALon, ABearing: Double);
  begin
    with GreatCircle_TestData1.WayPts[AIndex] do
    begin
      Lat := ALat;
      Lon := ALon;
      Bearing := ABearing;
    end;
  end;

begin
  // data from https://www.rainerstumpe.de/HTML/kurse3.html
  with GreatCircle_TestData1 do
  begin
    SetLength(WayPts, 26);
    InitPt( 0, DMSToDeg(41,  9, 28.0), DMSToDeg(- 8, 38,  0.0), 252);
    InitPt( 1, DMSToDeg(40, 37, 28.9), DMSToDeg(-10, 44, 57.6), 251);
    InitPt( 2, DMSToDeg(40,  2, 37.2), DMSToDeg(-12, 51, 55.2), 250);
    InitPt( 3, DMSToDeg(39, 24, 48.3), DMSToDeg(-14, 58, 52.8), 248);
    InitPt( 4, DMSToDeg(38, 43, 57.0), DMSToDeg(-17,  5, 50.4), 247);
    InitPt( 5, DMSToDeg(37, 59, 58.1), DMSToDeg(-19, 12, 48.0), 246);
    InitPt( 6, DMSToDeg(37, 12, 46.0), DMSToDeg(-21, 19, 45.6), 244);
    InitPt( 7, DMSToDeg(36, 22, 15.2), DMSToDeg(-23, 26, 43.2), 243);
    InitPt( 8, DMSToDeg(35, 28, 19.9), DMSToDeg(-25, 33, 40.8), 242);
    InitPt( 9, DMSToDeg(34, 30, 54.7), DMSToDeg(-27, 40, 38.4), 240);
    InitPt(10, DMSToDeg(33, 29, 54.1), DMSToDeg(-29, 47, 36.0), 239);
    InitPt(11, DMSToDeg(32, 25, 12.8), DMSToDeg(-31, 54, 33.6), 238);
    InitPt(12, DMSToDeg(31, 16, 46.0), DMSToDeg(-34,  1, 31.2), 237);
    InitPt(13, DMSToDeg(30,  4, 29.5), DMSToDeg(-36,  8, 28.8), 236);
    InitPt(14, DMSToDeg(28, 48, 19.7), DMSToDeg(-38, 15, 26.4), 235);
    InitPt(15, DMSToDeg(27, 28, 14.1), DMSToDeg(-40, 22, 24.0), 234);
    InitPt(16, DMSToDeg(26,  4, 11.3), DMSToDeg(-42, 29, 21.6), 233);
    InitPt(17, DMSToDeg(24, 36, 11.1), DMSToDeg(-44, 36, 19.2), 232);
    InitPt(18, DMSToDeg(23,  4, 15.2), DMSToDeg(-46, 43, 16.8), 231);
    InitPt(19, DMSToDeg(21, 28, 27.0), DMSToDeg(-48, 50, 14.4), 230);
    InitPt(20, DMSToDeg(19, 48, 52.1), DMSToDeg(-50, 57, 12.0), 230);
    InitPt(21, DMSToDeg(18,  5, 38.3), DMSToDeg(-53,  4,  9.6), 229);
    InitPt(22, DMSToDeg(16, 18, 55.8), DMSToDeg(-55, 11,  7.2), 228);
    InitPt(23, DMSToDeg(14, 28, 57.8), DMSToDeg(-57, 18,  4.8), 228);
    InitPt(24, DMSToDeg(12, 35, 59.7), DMSToDeg(-59, 25,  2.4), 227);
    InitPt(25, DMSToDeg(10, 40, 19.9), DMSToDeg(-61, 32,  0.0), 227);
  end;
end;

procedure TMvTests_GeoMath.Test_GreatCircleWayPoints;
var
  lat, lat_1: Double;
  bearing: Double;
  i: Integer;
begin
  InitGreatCircleData;

  with GreatCircle_TestData1 do
  begin
    lat_1 := WayPts[0].Lat;
    for i := Low(WayPts) + 1 to High(WayPts) - 1 do
    begin
      LatFromLonAtGreatCircle(Lat1, Lon1, Lat2, Lon2, WayPts[i].Lon, lat);
      bearing := CalcBearing(WayPts[i].Lat, WayPts[i].Lon, lat_1, Lon1) + 180;
      AssertEquals('Great circle way point #' + IntToStr(i) + ' latitude mismatch',
        WayPts[i].Lat,     // exptected
        lat,               // actual
        1E-4               // tolerance
      );
      AssertEquals('Great circle way point #' + IntToStr(i) + ' bearing mismatch',
        WayPts[i].Bearing, // expected
        bearing,           // actual
        0.5                // tolerance
      );
    end;
  end;
end;

procedure TMvTests_GeoMath.Test_LatToStr_Deg;
const
  NO_DMS = false;
var
  i: Integer;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
      AssertEquals(
        'Latitude string mismatch for ' + Name,
        Lat_Deg,                                     // expected
        LatToStr(Lat, NO_DMS, PointFormatSettings)   // actual
      );
end;

procedure TMvTests_GeoMath.Test_LatToStr_DMS;
const
  NEED_DMS = true;
var
  i: Integer;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
      AssertEquals(
        'Latitude string mismatch for ' + Name,
        Lat_DMS,                                      // expected
        LatToStr(Lat, NEED_DMS, PointFormatSettings)  // actual
      );
end;

procedure TMvTests_GeoMath.Test_LonToStr_Deg;
const
  NO_DMS = false;
var
  i: Integer;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
      AssertEquals(
        'Latitude string mismatch for ' + Name,
        Lon_Deg,                                    // expected
        LonToStr(Lon, NO_DMS, PointFormatSettings)  // actual
      );
end;

procedure TMvTests_GeoMath.Test_LonToStr_DMS;
const
  NEED_DMS = true;
var
  i: Integer;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
      AssertEquals(
        'Latitude string mismatch for ' + Name,
        Lon_DMS,                                       // expected
        LonToStr(Lon, NEED_DMS, PointFormatSettings)   // actual
      );
end;

procedure TmvTests_GeoMath.Test_StrToGPS_Lat;
var
  i: Integer;
  deg: Double;
  sec: Double;
  ok: Boolean;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      ok := TryStrToGPS(Lat_DMS, deg);
      AssertEquals(
        'No valid DMS string for latitude of ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        true, ok);
      AssertEquals(
        'Latitude value mismatch for ' + Name + ' (Test index: '+ IntToStr(i) + ')',
        Lat, deg);
    end;
end;

procedure TmvTests_GeoMath.Test_StrToGPS_Lon;
var
  i: Integer;
  deg: Double;
  sec: Double;
  ok: Boolean;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      ok := TryStrToGPS(Lon_DMS, deg);
      AssertEquals(
        'No valid DMS string for longitude of ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        true, ok);
      AssertEquals(
        'Longitude value mismatch for ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        Lon, deg);
    end;
end;

// Like Test_StrDMSToDeg, but checks also whether UTF8 in NWES is handled correctly.
procedure TmvTests_GeoMath.Test_StrToGPS_Lat_UTF8;
var
  i: Integer;
  deg: Double;
  ok: Boolean;
  testStr: String;
  N, E, S, W: String;
begin
  N := 'ν';
  E := 'ε';
  S := 'σ';
  W := 'ω';

  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      testStr := Lat_DMS;
      testStr := Utf8StringReplace(testStr, 'N', N,[]);
      testStr := UTF8StringReplace(testStr, 'E', E, []);
      testStr := UTF8StringReplace(testStr, 'S', S, []);
      testStr := Utf8StringReplace(testStr, 'W', W, []);

      ok := TryStrToGPS(Lat_DMS, deg, N,E,S,W);
      AssertEquals(
        'No valid DMS string for latitude of ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        true, ok);
      AssertEquals(
        'Latitude value mismatch for ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        Lat, deg);
    end;
end;

procedure TmvTests_GeoMath.Test_StrToGPS_Lon_UTF8;
var
  i: Integer;
  deg: Double;
  ok: Boolean;
  testStr: String;
  N, E, S, W: String;
begin
  N := 'ν';
  E := 'ε';
  S := 'σ';
  W := 'ω';

  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      testStr := Lon_DMS;
      testStr := Utf8StringReplace(testStr, 'N', N,[]);
      testStr := UTF8StringReplace(testStr, 'E', E, []);
      testStr := UTF8StringReplace(testStr, 'S', S, []);
      testStr := Utf8StringReplace(testStr, 'W', W, []);

      ok := TryStrToGPS(testStr, deg, N,E,S,W);
      AssertEquals(
        'No valid DMS string for longitude of ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        true, ok);
      AssertEquals(
        'Longitude value mismatch for ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        Lon, deg);
    end;
end;

procedure TmvTests_GeoMath.Test_StrDMSToDeg_Lat;
var
  i: Integer;
  deg: Double;
  sec: Double;
  ok: Boolean;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      ok := TryStrDMSToDeg(Lat_DMS, deg);
      AssertEquals(
        'No valid DMS string for latitude of ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        true, ok);
      AssertEquals(
        'Latitude value mismatch for ' + Name + ' (Test index: '+ IntToStr(i) + ')',
        Lat, deg);
    end;
end;

procedure TmvTests_GeoMath.Test_StrDMSToDeg_Lon;
var
  i: Integer;
  deg: Double;
  sec: Double;
  ok: Boolean;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      ok := TryStrDMSToDeg(Lon_DMS, deg);
      AssertEquals(
        'No valid DMS string for longitude of ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        true, ok);
      AssertEquals(
        'Longitude value mismatch for ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        Lon, deg);
    end;
end;

// Like Test_StrDMSToDeg, but checks also whether UTF8 in NWES is handled correctly.
procedure TmvTests_GeoMath.Test_StrDMSToDeg_Lat_UTF8;
var
  i: Integer;
  deg: Double;
  ok: Boolean;
  testStr: String;
  N, E, S, W: String;
begin
  N := 'ν';
  E := 'ε';
  S := 'σ';
  W := 'ω';

  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      testStr := Lat_DMS;
      testStr := Utf8StringReplace(testStr, 'N', N,[]);
      testStr := UTF8StringReplace(testStr, 'E', E, []);
      testStr := UTF8StringReplace(testStr, 'S', S, []);
      testStr := Utf8StringReplace(testStr, 'W', W, []);

      ok := TryStrDMSToDeg(Lat_DMS, deg, N,E,S,W);
      AssertEquals(
        'No valid DMS string for latitude of ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        true, ok);
      AssertEquals(
        'Latitude value mismatch for ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        Lat, deg);
    end;
end;

procedure TmvTests_GeoMath.Test_StrDMSToDeg_Lon_UTF8;
var
  i: Integer;
  deg: Double;
  ok: Boolean;
  testStr: String;
  N, E, S, W: String;
begin
  N := 'ν';
  E := 'ε';
  S := 'σ';
  W := 'ω';

  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      testStr := Lon_DMS;
      testStr := Utf8StringReplace(testStr, 'N', N,[]);
      testStr := UTF8StringReplace(testStr, 'E', E, []);
      testStr := UTF8StringReplace(testStr, 'S', S, []);
      testStr := Utf8StringReplace(testStr, 'W', W, []);

      ok := TryStrDMSToDeg(testStr, deg, N,E,S,W);
      AssertEquals(
        'No valid DMS string for longitude of ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        true, ok);
      AssertEquals(
        'Longitude value mismatch for ' + Name + ' (Test index: ' + IntToStr(i) + ')',
        Lon, deg);
    end;
end;

procedure TMvTests_GeoMath.Test_SplitGPS;
const
  TOLERANCE = 1e-5;
var
  i: Integer;
  D, M, S: double;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      SplitGPS(Lat, D, M, S);
      AssertEquals(
        'Latitude degrees mismatch for ' + Name,
        Lat_D,       // expected
        round(D)     // actual
      );
      AssertEquals(
        'Latitude minutes mismatch for ' + Name,
        Lat_M,       // expected
        round(M)     // actual
      );
      AssertEquals(
        'Latitude seconds mismatch for ' + Name,
        Lat_S,
        S,
        TOLERANCE
      );

      SplitGPS(Lon, D, M, S);
      AssertEquals(
        'Longitude degrees mismatch for ' + Name,
        Lon_D,       // expected
        round(D)     // actual
      );
      AssertEquals(
        'Longitude minutes mismatch for ' + Name,
        Lon_M,       // expected
        round(M)     // actual
      );
      AssertEquals(
        'Longitude seconds mismatch for ' + Name,
        Lon_S,
        S,
        TOLERANCE
      );
    end;
end;

procedure TMvTests_GeoMath.Test_ZoomFactor;
var
  z: Integer;
  f: Extended;
begin
  for z := 0 to 32 do
  begin
    f := ZoomFactor(z);
    AssertEquals('Zoomlevel lookup failure at ' + IntToStr(z), f, IntPower(2, z))
  end;
end;


initialization
  PointFormatSettings := DefaultFormatSettings;
  PointFormatSettings.DecimalSeparator := '.';
  DMS_Decimals := 4;

  RegisterTest(TMvTests_GeoMath);
end.

