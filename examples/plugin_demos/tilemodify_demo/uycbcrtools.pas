unit uYCbCrTools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TYCbCrRGBParms = array[0..8] of Double;
var
// Values taken from
// Keith Jack, Video Demystified, Fiths Edition, Elsevier Inc. 2007,
// Chapter 3: Color Spaces, RGB-YCbCr Equitations: HDTV, P. 21
// Processing of contrast and brightness taken form Chapter 7: Digital Rnhancement
// P. 198

  RGBToYCbCrParms : TYCbCrRGBParms = (
      0.183,  0.614,  0.062,  // Y  = aR + bG + cB + 16
     -0.101, -0.338,  0.439,  // Cb = aR + bG + cB + 128
      0.439, -0.399, -0.040   // Cr = aR + bG + cB + 128
    );
  YCbCrToRGBParms : TYCbCrRGBParms = (
      1.164,  1.793,  0.000,  // R = a(Y-16) + b(Cr-128)              Hint: Cb is ignored
      1.164, -0.534,  0.213,  // G = a(Y-16) + b(Cr-128) + c(Cb-128)
      1.164,  0.000,  2.115   // B = a(Y-16)             + c(Cb-128)  Hint: Cr is ignored
    );

function LimitToByteRange(const AValue : Double) : Double;
function LimitToByteRangeInline(const AValue : Double) : Double;inline;

procedure RGBToYCbCr(const R, G, B : Byte; out Y, Cr, Cb : Double);
procedure RGBToYCbCrInline(const R, G, B : Byte; out Y, Cr, Cb : Double);inline;
procedure YCbCrToRGB(const Y, Cr, Cb : Double; out R, G, B : Byte);
procedure YCbCrToRGBInline(const Y, Cr, Cb : Double; out R, G, B : Byte);inline;

{ The parameters Contrast, Brightness must be between 0.0 and 1.0 (values included)
  The Y, Cr, Cb are assumed to be normalised between 0 and 255 }
procedure YCbCrContrastBrightness(var Y, Cr, Cb : Double;
                                  const Contrast, Brightness : Double);

implementation

function LimitToByteRangeInline(const AValue: Double): Double;
begin
  if AValue > 255 then
    Result := 255.0
  else if AValue < 0.0 then
    Result := 0.0
  else
    Result := AValue;
end;
function LimitToByteRange(const AValue: Double): Double;
begin
  Result := LimitToByteRangeInline(AValue);
end;

procedure RGBToYCbCrInline(const R, G, B: Byte; out Y, Cr, Cb: Double);
begin
  Y  := Trunc(LimitToByteRangeInline(R*RGBToYCbCrParms[0] + G*RGBToYCbCrParms[1] + B*RGBToYCbCrParms[2] + 16.0));
  Cb := Trunc(LimitToByteRangeInline(R*RGBToYCbCrParms[3] + G*RGBToYCbCrParms[4] + B*RGBToYCbCrParms[5] + 128.0));
  Cr := Trunc(LimitToByteRangeInline(R*RGBToYCbCrParms[6] + G*RGBToYCbCrParms[7] + B*RGBToYCbCrParms[8] + 128.0));
end;
procedure RGBToYCbCr(const R, G, B: Byte; out Y, Cr, Cb: Double);
begin
  RGBToYCbCrInline(R,G,B,Y,Cr,Cb);
end;

procedure YCbCrToRGBInline(const Y, Cr, Cb: Double; out R, G, B: Byte);
var
  y16 : Double;
  cr128 : Double;
  cb128 : Double;
begin
  y16 := Y-16.0;
  cr128 := Cr-128.0;
  cb128 := Cb-128.0;
  R := Trunc(LimitToByteRangeInline(y16*YCbCrToRGBParms[0] + cr128*YCbCrToRGBParms[1]));
  G := Trunc(LimitToByteRangeInline(y16*YCbCrToRGBParms[3] + cr128*YCbCrToRGBParms[4] + cb128*YCbCrToRGBParms[5]));
  B := Trunc(LimitToByteRangeInline(y16*YCbCrToRGBParms[6]                            + cb128*YCbCrToRGBParms[8]));
end;
procedure YCbCrToRGB(const Y, Cr, Cb: Double; out R, G, B: Byte);
begin
  YCbCrToRGBInline(Y,Cr,Cb,R,G,B);
end;

procedure YCbCrContrastBrightness(var Y, Cr, Cb: Double; const Contrast,
  Brightness: Double);
var
  lContrast, lBrightness : Double;
begin
  lContrast := ((Contrast-0.5)*2.0) + 1.0;
  lBrightness := (Brightness-0.5)* 2.0 * 128.0;
  Y := LimitToByteRangeInline((Y - 16.0) * lContrast + lBrightness + 16.0);
  Cr := LimitToByteRangeInline((Cr - 128.0) * lContrast + 128.0);
  Cb := LimitToByteRangeInline((Cb - 128.0) * lContrast + 128.0);
end;

end.

