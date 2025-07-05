{ Reads/writes GPX files
  (C) 2019 Werner Pamler (user wp at Lazarus forum https://forum.lazarus.freepascal.org)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvGPX;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, DateUtils,
  mvTypes, mvGpsObj;

type

  { TGpxReader }

  TGpxReader = class
  private
    ID: Integer;
    FMinLat, FMinLon, FMaxLat, FMaxLon: Double;
  protected
    procedure ReadExtensions(ANode: TDOMNode; ATrack: TGpsTrack); overload;
    procedure ReadExtensions(ANode: TDOMNode; APt: TGPSPoint); overload;
    function ReadPoint(ANode: TDOMNode): TGpsPoint;
    procedure ReadRoute(ANode: TDOMNode; AList: TGpsObjectlist);
    procedure ReadTrack(ANode: TDOMNode; AList: TGpsObjectList);
    procedure ReadTracks(ANode: TDOMNode; AList: TGpsObjectList);
    procedure ReadTrackSegment(ANode: TDOMNode; ATrack: TGpsTrack);
    procedure ReadWayPoints(ANode: TDOMNode; AList: TGpsObjectList);
  public
    function LoadFromFile(AFileName: String; AList: TGpsObjectList): Integer;
    function LoadFromFile(AFileName: String; AList: TGpsObjectList; out ABounds: TRealArea): Integer;
    function LoadFromStream(AStream: TStream; AList: TGpsObjectList): Integer;
    function LoadFromStream(AStream: TStream; AList: TGpsObjectList; out ABounds: TRealArea): Integer;
  end;

  { TGpxWriter }

  TGpxWriter = class
  public
    class procedure SaveToStream(AStream: TStream; AList: TGpsObjectList);
    class procedure SaveToFile(AFileName: String; AList: TGPSObjectList);
  end;


implementation

uses
  Math,
  mvExtraData, Graphics;

var
  PointSettings: TFormatSettings;

function GetTZD: TTime;
begin
  Result := -(DateUtils.OneMinute * SysUtils.GetLocalTimeOffset);
end;

function Str2DTTZD_l(S: AnsiString; out Tzd: TTime; out IsLocal: Boolean): TDateTime;
const
  FMT = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';
var
  I, L, Sg: Integer;
  Millis: Int64;
  digit: Integer;

  procedure RaiseErr;
  begin
    raise EConvertError.Create('Bad ISO8601 format.');
  end;

begin
  L := Length(S);
  if (L > 19) and (S[20] in ['.', ',']) then // 'YYYY-MM-DDTHH:NN:SS[.,]?.*'
  begin
    I := 21; // beyond the millis dot
    S[20] := '.';
    // Unfortunately, millis are not scanned correctly with zzz format chars.
    // .8 is scanned same as .08 or .008 which is *totally* wrong!
    Result := ScanDateTime(FMT {+ '"."zzz'}, S);
    Millis := 0;
    Sg := 0;
    while (I < L) and (S[I] in ['0'..'9']) do
    begin
      if Sg < 3 then // Only first three
      begin
        digit := ord(S[I]) - ord('0');
        Millis := Millis * 10 + digit;
        Inc(Sg);
      end;
      Inc(I); // Skip millis field
    end;
    case Sg of
      0: RaiseErr;
      1: Millis := Millis * 100;
      2: Millis := Millis * 10;
    end;
    // Adjust with milliseconds
    Result := IncMilliSecond(Result, Millis);
  end
  else
  begin
    I := 20; // beyond the seconds
    Result := ScanDateTime(FMT, S);
  end;

  // ScanDateTime does not treat trailing characters as error
  if I <= L then   // Length('YYYY-MM-DDTHH:NN:SS')
  begin

    // Check Zulu
    if (I = L) and (S[I] = 'Z') then
      Tzd := 0.0 // TZD is zero

    // Check TZD
    else if ((L - I) in [2,4,5]) and (S[I] in ['+', '-']) then
    begin
      if S[I] = '-' then Sg := -1 else Sg := 1;
      case (L - I) of
        2: Tzd := ScanDateTime('hh', Copy(S, I + 1, 2)) * Sg;
        4: Tzd := ScanDateTime('hhnn', Copy(S, I + 1, 4)) * Sg;
        5: Tzd := ScanDateTime('hh:nn', Copy(S, I + 1, 5)) * Sg;
      end;
    end

    // Garbage at the end
    else
      RaiseErr;

    IsLocal := False; // Qualified
  end
  else
  begin
    Tzd := GetTZD;   // Using local TZD!!!
    IsLocal := True; // Unqualified (local time)
  end;
end;

function Str2DTTZD(S: AnsiString; out Tzd: TTime): TDateTime;
var
  Local: Boolean;
begin
  Result := Str2DTTZD_l(S, Tzd, Local);
end;

function ExtractISODateTime(AText: String): TDateTime;
var
  Tzd: TTime;
begin
  try
    Result := Str2DTTZD(AText, Tzd);
  except
    Result := NO_DATE;
  end;
end;

function GetAttrValue(ANode: TDOMNode; AAttrName: string) : string;
var
  i: LongWord;
  Found: Boolean;
begin
  Result := '';
  if (ANode = nil) or (ANode.Attributes = nil) then
    exit;

  Found := false;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then begin
      Found := true;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    inc(i);
  end;
end;

function GetNodeValue(ANode: TDOMNode): String;
var
  child: TDOMNode;
begin
  Result := '';
  child := ANode.FirstChild;
  if Assigned(child) and ((child.NodeName = '#text') or
    (child.NodeName = '#cdata-section'))
  then
    Result := child.NodeValue;
end;

function TryStrToGpxColor(AGpxText: String; out AColor: TColor): Boolean;
begin
  if AGpxText = 'default' then
  begin
    AColor := clDefault;
    Exit(True);
  end;
  if AGpxText = 'none' then
  begin
    AColor := clNone;
    Exit(True);
  end;
  if Length(AGpxText) <> 6 then
    Exit;
  Result := TryStrToInt('$' + AGpxText, AColor);
  if Result then
    AColor := ((AColor and $FF) shl 16) + (AColor and $FF00) + ((AColor and $FF0000) shr 16);
end;


{ TGpxReader }

{ Loads the specified gpx file and stores the tracks, points etc. in the provided
  list. All items share the same mapviewer ID which is selected randomly and
  return as function result. ABounds is the geo rectangle enclosing the items. }
function TGpxReader.LoadFromFile(AFileName: String; AList: TGpsObjectList;
  out ABounds: TRealArea): Integer;
var
  stream: TStream;
begin
  stream := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    Result := LoadFromStream(stream, AList, ABounds);
  finally
    stream.Free;
  end;
end;

function TGpxReader.LoadFromFile(AFileName: String; AList: TGpsObjectList): Integer;
var
  area: TRealArea;
begin
  Result := LoadFromFile(AFileName, AList, area);
end;

{ See LoadFromFile. }
function TGpxReader.LoadFromStream(AStream: TStream; AList: TGpsObjectList;
  out ABounds: TRealArea): Integer;
var
  doc: TXMLDocument = nil;
begin
  try
    ID := random(MaxInt - 1000) + 1000;
    FMinLon := 9999; FMinLat := 9999;
    FMaxLon := -9999; FMaxLat := -9999;
    ReadXMLFile(doc, AStream);
    ReadWayPoints(doc.DocumentElement.FindNode('wpt'), AList);
    ReadTracks(doc.DocumentElement.FindNode('trk'), AList);
    ReadRoute(doc.DocumentElement.FindNode('rte'), AList);
    ABounds.TopLeft.Lon := FMinLon;
    ABounds.TopLeft.Lat := FMaxLat;
    ABounds.BottomRight.Lon := FMaxLon;
    ABounds.BottomRight.Lat := FMinLat;
    Result := ID;
  finally
    doc.Free;
  end;
end;

class procedure TGpxWriter.SaveToStream(AStream: TStream; AList: TGpsObjectList
  );
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  Ex, LineEx: TDOMElement;
  I: Integer;
  O: TGPSObj;

  function CreateTextElement(ANode: TDOMNode; ATag, AValue: DOMString): TDOMNode;
  begin
    Result := ANode;
    Result.AppendChild(Doc.CreateElement(ATag)).AppendChild(
      Doc.CreateTextNode(AValue));
  end;

  function ColorToGPXColor(C: TColor): LongInt;
  begin
    Result := ColorToRGB(C);
    Result := ((Result and $FF) shl 16) + (Result and $FF00) + ((Result and $FF0000) shr 16);
  end;

  function ColorToGPXColorStr(C: TColor): String;
  begin
    if (C = clDefault)
      then Result := 'default'
      else if (C = clNone)
        then Result := 'none'
        else Result := ColorToGPXColor(C).ToHexString(6);
  end;

  function AddEx(AElm: TDOMElement): TDOMElement;
  begin
    if not Assigned(Ex) then
      Ex := Doc.CreateElement('extensions');
    Ex.AppendChild(AElm);
    Result := AElm;
  end;

  function CreatePoint(AName: String; APt: TGPSPoint): TDOMElement;
  var
    Img: Integer = -1;
  begin
    Ex := Nil; LineEx := Nil;
    Result := Doc.CreateElement(AName);
    Result.SetAttribute('lat', APt.Lat.ToString);
    Result.SetAttribute('lon', APt.Lon.ToString);
    if APt.Name <> '' then
      CreateTextElement(Result, 'name', APt.Name);
    if APt.Elevation <> NO_ELE then
      CreateTextElement(Result, 'ele', APt.Elevation.ToString);
    if APt.DateTime <> NO_DATE then
      CreateTextElement(Result, 'time', DateToISO8601(APt.DateTime));
    if (APt is TGPSPointOfInterest) then
      Img := TGPSPointOfInterest(APt).ImageIndex;
    if not APt.Visible or (Img <> -1) then
    begin
      LineEx := AddEx(Doc.CreateElement('misc'));
      if not APt.Visible then
        CreateTextElement(LineEx, 'visible', '0');
      if Img <> -1 then
        CreateTextElement(LineEx, 'image', Img.ToString);
    end;
    if Assigned(Ex) then
      Result.AppendChild(Ex);
  end;

  function CreateTrack(AName: String; ATrack: TGPSTrack): TDOMElement;
  var
    Seg: TDOMElement;

    procedure CreateSegments;
    var
      P: TGPSObj;
    begin
      for P in ATrack.AllObjs do
      begin
        Seg.AppendChild(CreatePoint('trkpt', TGPSPoint(P)));
        if TSegmentExtraData.MarkOf(P) = smEnd then
        begin
          Seg := Doc.CreateElement('trkseg');
          Result.AppendChild(Seg);
        end;
      end;
    end;

  begin
    Ex := Nil;
    Result := Doc.CreateElement(AName);
    if ATrack.Name <> '' then
      CreateTextElement(Result, 'name', ATrack.Name);
    if not ATrack.Visible then
    begin
      LineEx := AddEx(Doc.CreateElement('misc'));
      CreateTextElement(LineEx, 'visible', '0');
    end;
    if (ATrack.LineColor <> clDefault) or (ATrack.LineWidth > 0) then
    begin
      LineEx := AddEx(Doc.CreateElement('line'));
      CreateTextElement(LineEx, 'color', ColorToGPXColorStr(ATrack.LineColor));
      CreateTextElement(LineEx, 'width', ATrack.LineWidth.ToString);
    end;
    if (ATrack.ConnectColor <> clNone) or (ATrack.ConnectWidth > 0) then
    begin
      LineEx := AddEx(Doc.CreateElement('segconn'));
      CreateTextElement(LineEx, 'color', ColorToGPXColorStr(ATrack.ConnectColor));
      CreateTextElement(LineEx, 'width', ATrack.ConnectWidth.ToString);
    end;
    if Assigned(Ex) then
      Result.AppendChild(Ex);
    if ATrack.Points.Count > 0 then
    begin
      Seg := Doc.CreateElement('trkseg');
      Result.AppendChild(Seg);
      CreateSegments;
    end;
  end;

begin
  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('gpx');
    Doc.AppendChild(RootNode);
    RootNode := Doc.DocumentElement;
    TDOMElement(RootNode).SetAttribute('version', '1.1');
    TDOMElement(RootNode).SetAttribute('creator',
      'LazMapViewer https://wiki.lazarus.freepascal.org/LazMapViewer');

    Ex := Nil;
    for I := 0 to Pred(AList.Count) do
    begin
      O := AList[I];
      if O is TGPSPointOfInterest then
        RootNode.AppendChild(CreatePoint('wpt', TGPSPoint(O)))
      else if O is TGPSPoint then
        RootNode.AppendChild(CreatePoint('wpt', TGPSPoint(O)))
      else if O is TGPSTrack then
        RootNode.AppendChild(CreateTrack('trk', TGPSTrack(O)))
      else
        {TODO};
    end;
    WriteXMLFile(doc, AStream);

  finally
    Doc.Free;
  end;
end;

class procedure TGpxWriter.SaveToFile(AFileName: String; AList: TGPSObjectList);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream, AList);
  finally
    Stream.Free;
  end;
end;

function TGpxReader.LoadFromStream(AStream: TStream; AList: TGpsObjectList): Integer;
var
  area: TRealArea;
begin
  Result := LoadFromStream(AStream, AList, area);
end;

function FindChild(ANode: TDOMNode; APath: TStringArray): TDOMNode;
var
  N: TDOMNode;
begin
  Result := Nil;
  if not Assigned(ANode) or (Length(APath) < 1) then
    Exit;
  N := ANode.FirstChild;
  while Assigned(N) do
  begin
    if APath[0] = N.NodeName then
      if Length(APath) > 1
        then Exit(FindChild(N, Copy(APath, 1, Pred(Length(APath)))))
        else Exit(N);
    N := N.NextSibling;
  end;
end;

procedure TGpxReader.ReadExtensions(ANode: TDOMNode; ATrack: TGpsTrack);
var
  N: TDOMNode;
  color: TColor;
  inv: LongInt;
  w: Double = -1;
begin
  if ANode = Nil then
    Exit;
  N := FindChild(ANode, ['line', 'color']);
  if Assigned(N) and TryStrToGpxColor(GetNodeValue(N), color) then
    ATrack.LineColor := color;
  N := FindChild(ANode, ['line', 'width']);
  if Assigned(N) and TryStrToFloat(GetNodeValue(N), w, PointSettings) and (w > 0) then
    ATrack.LineWidth := w;
  N := FindChild(ANode, ['segconn', 'color']);
  if Assigned(N) and TryStrToGpxColor(GetNodeValue(N), color) then
    ATrack.ConnectColor := color;
  N := FindChild(ANode, ['segconn', 'width']);
  if Assigned(N) and TryStrToFloat(GetNodeValue(N), w, PointSettings) and (w > 0) then
    ATrack.ConnectWidth := w;
  N := FindChild(ANode, ['visible']);
  if Assigned(N) and TryStrToInt(GetNodeValue(N), inv) then
    ATrack.Visible := inv <> 0;
end;

procedure TGpxReader.ReadExtensions(ANode: TDOMNode; APt: TGPSPoint);
var
  N: TDOMNode;
  inv: LongInt;
begin
  if ANode = Nil then
    Exit;
  N := FindChild(ANode, ['visible']);
  if Assigned(N) and TryStrToInt(GetNodeValue(N), inv) then
    APt.Visible := inv <> 0;
end;

function TGpxReader.ReadPoint(ANode: TDOMNode): TGpsPoint;
var
  s, slon, slat, sName: String;
  lon, lat, ele: Double;
  dt: TDateTime;
  node: TDOMNode;
  nodeName: String;
begin
  Result := nil;
  if ANode = nil then
    exit;

  slon := GetAttrValue(ANode, 'lon');
  slat := GetAttrValue(ANode, 'lat');
  if (slon = '') or (slat = '') then
    exit;

  if not TryStrToFloat(slon, lon, PointSettings) then
    exit;
  if not TryStrToFloat(slat, lat, PointSettings) then
    exit;

  sName := '';
  dt := NO_DATE;
  ele := NO_ELE;
  Result := TGpsPoint.Create(lon, lat, ele, dt);
  node := ANode.FirstChild;
  while node <> nil do begin
    nodeName := node.NodeName;
    case nodeName of
      'ele' :
        begin
          s := GetNodeValue(node);
          if s <> '' then
            TryStrToFloat(s, ele, PointSettings);
        end;
      'name':
        sName := GetNodeValue(node);
      'time':
        begin
          s := GetNodeValue(node);
          if s <> '' then
            dt := ExtractISODateTime(s);
        end;
      'extensions':
        ReadExtensions(node, Result);
    end;
    node := node.NextSibling;
  end;
  Result.Name := sname;
  Result.Elevation := ele;
  Result.DateTime := dt;
  FMinLon := Min(FMinLon, lon);
  FMaxLon := Max(FMaxLon, lon);
  FMinLat := Min(FMinLat, lat);
  FMaxLat := Max(FMaxLat, lat);
end;

procedure TGpxReader.ReadRoute(ANode: TDOMNode; AList: TGpsObjectlist);
var
  trk: TGpsTrack;
  nodeName: string;
  pt: TGpsPoint;
  trkName: String;
begin
  if ANode = nil then
    exit;
  ANode := ANode.FirstChild;
  if ANode = nil then
    exit;
  trk := TGpsTrack.Create;
  while ANode <> nil do begin
    nodeName := ANode.NodeName;
    case nodeName of
      'name':
        trkName := GetNodeValue(ANode);
      'rtept':
        begin
          pt := ReadPoint(ANode);
          if pt <> nil then trk.Points.Add(pt);
        end;
    end;
    ANode := ANode.NextSibling;
  end;
  trk.Name := trkName;
  AList.Add(trk, ID);
end;

procedure TGpxReader.ReadTrack(ANode: TDOMNode; AList: TGpsObjectList);
var
  trk: TGpsTrack;
  nodeName: string;
  pt: TGpsPoint;
  trkName: String = '';
begin
  if ANode = nil then
    exit;
  ANode := ANode.FirstChild;
  if ANode = nil then
    exit;

  trk := TGpsTrack.Create;
  while ANode <> nil do begin
    nodeName := ANode.NodeName;
    case nodeName of
      'name':
        trkName := GetNodeValue(ANode);
      'trkseg':
        ReadTrackSegment(ANode.FirstChild, trk);
      'trkpt':
        begin
          pt := ReadPoint(ANode);
          if pt <> nil then trk.Points.Add(pt);
        end;
      'extensions':
        ReadExtensions(ANode, trk);
    end;
    ANode := ANode.NextSibling;
  end;
  trk.Name := trkName;
  AList.Add(trk, ID);
end;

procedure TGpxReader.ReadTracks(ANode: TDOMNode; AList: TGpsObjectList);
var
  nodeName: String;
begin
  while ANode <> nil do begin
    nodeName := ANode.NodeName;
    if nodeName = 'trk' then
      ReadTrack(ANode, AList);
    ANode := ANode.NextSibling;
  end;
end;

procedure TGpxReader.ReadTrackSegment(ANode: TDOMNode; ATrack: TGpsTrack);
var
  gpsPt: TGpsPoint = Nil;
  nodeName: String;
begin
  while ANode <> nil do begin
    nodeName := ANode.NodeName;
    if nodeName = 'trkpt' then begin
      gpsPt := ReadPoint(ANode);
      if gpsPt <> Nil then
        ATrack.Points.Add(gpsPt);
    end;
    ANode := ANode.NextSibling;
  end;
  if Assigned(gpsPt) then // Mark the last point of the segment
    gpsPt.ExtraData := TSegmentExtraData.Create(smEnd);
end;

procedure TGpxReader.ReadWayPoints(ANode: TDOMNode; AList: TGpsObjectList);
var
  nodeName: String;
  gpsPt: TGpsPoint;
begin
  while ANode <> nil do begin
    nodeName := ANode.NodeName;
    if nodeName = 'wpt' then begin
      gpsPt := ReadPoint(ANode);
      if gpsPt <> nil then
        AList.Add(gpsPt, ID);
    end;
    ANode := ANode.NextSibling;
  end;
end;

initialization
  PointSettings := DefaultFormatSettings;
  PointSettings.DecimalSeparator := '.';

end.

