{
  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvExtraData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  { TDrawingExtraData }

  TDrawingExtraData = class
  private
    FColor: TColor;
    FId: integer;
    procedure SetColor(AValue: TColor);
  public
    constructor Create(aId: integer); virtual;
    property Color: TColor read FColor write SetColor;
    property Id: integer read FId;
  end;

  TTrackExtraData = class(TDrawingExtraData)
  private
    FWidth: Double;
    procedure SetWidth(AValue: Double);
  public
    property Width: Double read FWidth write SetWidth;   // Line width in mm
  end;

  { TSegmentExtraData }

  TSegmentExtraData = class(TObject)
  public
    type TSegmentMark = (smNone, smStart, smMid,  smEnd);
  private
    FMark: TSegmentMark;
  public
    constructor Create(AMark: TSegmentMark);
    class function MarkOf(const APoint: TObject): TSegmentMark;
    property Mark: TSegmentMark read FMark write FMark;
  end;


implementation

uses
  mvGpsObj;

{ TSegmentExtraData }

constructor TSegmentExtraData.Create(AMark: TSegmentMark);
begin
  inherited Create;
  FMark := AMark;
end;

// Can be invoked with APoint.Extradata or with APoint itself
class function TSegmentExtraData.MarkOf(const APoint: TObject): TSegmentMark;
begin
  // Quick check for Nil (APoint.Extradata, APoint)
  if not Assigned(APoint) then
    Result := smNone
  // Check for non-nil TSegmentExtraData (APoint.Extradata)
  else if (APoint is Self) then
    Result := TSegmentExtraData(APoint).Mark
  // Check for non-nil object with a non-nil TSegmentExtraData (APoint)
  else if (APoint is TGPSObj) and Assigned(TGPSObj(APoint).ExtraData) and
    (TGPSObj(APoint).ExtraData is Self)
  then
    Result := TSegmentExtraData(TGPSObj(APoint).ExtraData).Mark
  else // None of the above
    Result := smNone;
end;

{ TDrawingExtraData }

constructor TDrawingExtraData.Create(aId: integer);
begin
  FId := aId;
  FColor := clRed;
end;

procedure TDrawingExtraData.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
end;


{ TTrackExtraData }

procedure TTrackExtraData.SetWidth(AValue: Double);
begin
  if AValue = FWidth then Exit;
  FWidth := abs(AValue);
end;

end.

