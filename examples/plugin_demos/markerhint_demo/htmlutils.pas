{ Simple HTML rendering, extracted from JVCL

  Supports only the following HTML tags

    <align="center"|"right> -- alignment
    <a href="...">...</a>  -- hyperlink
    <b>...</b> -- bold text
    <i>...</i> -- italic text
    <u>...</u> -- underlined text
    <s>...>/s> -- striked-out text
    <sub>...</sub> -- subscript
    <sup>...</sup> -- superscript
    <font size="..." color="..." bgcolor="..." -- font size, color, background color
    <ind="...">  -- indentation
    <hr> -- horizontal line
    <br>, <br/> -- line break
}

unit htmlutils;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf,
  Graphics, Types, Classes, SysUtils;

type
  TJvHTMLCalcType = (htmlShow, htmlCalcWidth, htmlCalcHeight, htmlHyperLink);

procedure HTMLDrawTextEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; out Width: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; out MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer = 100); overload;
procedure HTMLDrawTextEx2(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; out Width, Height: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; out MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer = 100); overload;
procedure HTMLDrawText(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double;
  Scale: Integer = 100);
procedure HTMLDrawTextHL(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; MouseX, MouseY: Integer;
  SuperSubScriptRatio: Double; Scale: Integer = 100);
function HTMLPlainText(const Text: string): string;
function HTMLTextExtent(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): TSize;
function HTMLTextWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
function HTMLTextHeight(Canvas: TCanvas; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
function HTMLPrepareText(const Text: string): string;
function HTMLStringToColor(AText: String): TColor;

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;

implementation

uses
  Math, Forms;

const
  cBR = '<BR>';
  cBRnew = '<BR/>';
  cHR = '<HR>';
  cTagBegin = '<';
  cTagEnd = '>';
  cLT = '<';
  cGT = '>';
  cQuote = '"';
  cCENTER = 'CENTER';
  cRIGHT = 'RIGHT';
  cHREF = 'HREF';
  cIND = 'IND';
  cCOLOR = 'COLOR';
  cBGCOLOR = 'BGCOLOR';

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;
var
  tt: TTextMetric;
begin
  // (ahuser) Qt returns different values for TextHeight('Ay') and TextHeigth(#1..#255)
  GetTextMetrics(Canvas.Handle, tt{%H-});
  Result := tt.tmHeight;
end;

// moved from JvHTControls and renamed
function HTMLPrepareText(const Text: string): string;
type
  THtmlCode = record
    Html: string;
    Text: UTF8String;
  end;
const
  Conversions: array [0..6] of THtmlCode = (
    (Html: '&amp;'; Text: '&'),
    (Html: '&quot;'; Text: '"'),
    (Html: '&reg;'; Text: #$C2#$AE),
    (Html: '&copy;'; Text: #$C2#$A9),
    (Html: '&trade;'; Text: #$E2#$84#$A2),
    (Html: '&euro;'; Text: #$E2#$82#$AC),
    (Html: '&nbsp;'; Text: ' ')
  );
var
  I: Integer;
begin
  Result := Text;
  for I := Low(Conversions) to High(Conversions) do
    Result := StringReplace(Result, Conversions[I].Html, Utf8ToAnsi(Conversions[I].Text), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, sLineBreak, '', [rfReplaceAll, rfIgnoreCase]); // only <BR> can be new line
  Result := StringReplace(Result, cBR, sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, cBRnew, sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, cHR, cHR + sLineBreak, [rfReplaceAll, rfIgnoreCase]); // fixed <HR><BR>
end;

function HTMLStringToColor(AText: String): TColor;
type
  TRGBA = packed record
    R, G, B, A: byte;
  end;
var
  c: Int32;
begin
  if AText[1] = '#' then AText[1] := '$';
  if TryStrToInt(AText, c) then begin
    TRgba(Result).R := TRgba(c).B;
    TRgba(Result).G := TRgba(c).G;
    TRgba(Result).B := TRgba(c).R;
    TRgba(Result).A := 0;
  end else begin
    Result := StringToColor('cl'+AText);
  end;
end;

function HTMLBeforeTag(var Str: string; DeleteToTag: Boolean = False): string;
begin
  if Pos(cTagBegin, Str) > 0 then
  begin
    Result := Copy(Str, 1, Pos(cTagBegin, Str) - 1);
    if DeleteToTag then
      Delete(Str, 1, Pos(cTagBegin, Str) - 1);
  end
  else
  begin
    Result := Str;
    if DeleteToTag then
      Str := '';
  end;
end;

function GetChar(const Str: string; Pos: Word; Up: Boolean = False): Char;
begin
  if Length(Str) >= Pos then
    Result := Str[Pos]
  else
    Result := ' ';
  if Up then
    Result := UpCase(Result);
end;

function HTMLDeleteTag(const Str: string): string;
begin
  Result := Str;
  if (GetChar(Result, 1) = cTagBegin) and (Pos(cTagEnd, Result) > 1) then
    Delete(Result, 1, Pos(cTagEnd, Result));
end;

// wp: Made Width and MouseOnLink out parameters (were "var" in the original)
// to silence the compiler
procedure HTMLDrawTextEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; out Width: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; out MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer);
var
  H: Integer;
begin
  HTMLDrawTextEx2(Canvas, Rect, State, Text, Width, H, CalcType, MouseX, MouseY, MouseOnLink,
    LinkName, SuperSubScriptRatio, Scale);
  if CalcType = htmlCalcHeight then
    Width := H;
end;

type
  TScriptPosition = (spNormal, spSuperscript, spSubscript);

// wp: Make Width, Height and MouseOnLink "out" parameters
// (they were "var" in the original) to silence the compiler
procedure HTMLDrawTextEx2(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; out Width, Height: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; out MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer);
const
  DefaultLeft = 0; // (ahuser) was 2
var
  vText, vM, TagPrp, Prp, TempLink: string;
  vCount: Integer;
  vStr: TStringList;
  Selected: Boolean;
  Alignment: TAlignment;
  Trans, IsLink: Boolean;
  CurLeft: Integer;
  // for begin and end
  OldFontStyles: TFontStyles;
  OldFontColor: TColor;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldAlignment: TAlignment;
  OldFont: TFont;
  OldWidth: Integer;
  // for font style
  RemFontColor,
  RemBrushColor: TColor;
  RemFontSize: Integer;
  ScriptPosition: TScriptPosition;

  function ExtractPropertyValue(const Tag: string; PropName: string): string;
  var
    I: Integer;
  begin
    Result := '';
    PropName := UpperCase(PropName);
    if Pos(PropName, UpperCase(Tag)) > 0 then
    begin
      Result := Copy(Tag, Pos(PropName, UpperCase(Tag)) + Length(PropName), Length(Tag));
     if Pos('"', Result) <> 0 then
     begin
       Result := Copy(Result, Pos('"', Result) + 1, Length(Result));
       Result := Copy(Result, 1, Pos('"', Result) - 1);
     end
     else
     if Pos('''', Result) <> 0 then
     begin
       Result := Copy(Result, Pos('''', Result) + 1, Length(Result));
       Result := Copy(Result, 1, Pos('''', Result) - 1);
     end
     else
     begin
       Result := Trim(Result);
       Delete(Result, 1, 1);
       Result := Trim(Result);
       I := 1;
       while (I < Length(Result)) and (Result[I+1] <> ' ') do
         Inc(I);
       Result := Copy(Result, 1, I);
     end;
    end;
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if Assigned(Canvas) then
      if Include then
        Canvas.Font.Style := Canvas.Font.Style + [Style]
      else
        Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

  function CalcPos(const Str: string): Integer;
  begin
    case Alignment of
      taRightJustify:
        Result := (Rect.Right - Rect.Left) - HTMLTextWidth(Canvas, Rect, State, Str, Scale);
      taCenter:
        Result := DefaultLeft + ((Rect.Right - Rect.Left) - HTMLTextWidth(Canvas, Rect, State, Str, SuperSubScriptRatio)) div 2;
    else
      Result := DefaultLeft;
    end;
    if Result <= 0 then
      Result := DefaultLeft;
  end;

  procedure Draw(const M: string);
  var
    Width, Height: Integer;
    R: TRect;
    OriginalFontSize: Integer;
    lineHeight: Integer;
  begin
    R := Rect;
    Inc(R.Left, CurLeft);
    if Assigned(Canvas) then
    begin
      lineHeight := Canvas.TextHeight('Tg');
      OriginalFontSize := Canvas.Font.Size;
      try
        if ScriptPosition <> spNormal then
          Canvas.Font.Size := Round(Canvas.Font.Size * SuperSubScriptRatio);

        Width  := Canvas.TextWidth(M);
        Height := CanvasMaxTextHeight(Canvas);

        if ScriptPosition = spSubscript then
          R.Top := R.Top + lineHeight - Height - 1;

        if IsLink and not MouseOnLink then
          if (MouseY >= R.Top) and (MouseY <= R.Top + Height) and
             (MouseX >= R.Left) and (MouseX <= R.Left + Width) and
             ((MouseY > 0) or (MouseX > 0)) then
          begin
            MouseOnLink := True;
            Canvas.Font.Color := clRed; // hover link
            LinkName := TempLink;
          end;
        if CalcType = htmlShow then
        begin
          if Trans then
            Canvas.Brush.Style := bsClear; // for transparent
          Canvas.TextOut(R.Left, R.Top, M);
        end;
        CurLeft := CurLeft + Width;
      finally
        Canvas.Font.Size := OriginalFontSize;
      end;
    end;
  end;

  procedure NewLine(Always: Boolean = False);
  begin
    if Assigned(Canvas) then
      if Always or (vCount < vStr.Count - 1) then
      begin
        Width := Max(Width, CurLeft);
        CurLeft := DefaultLeft;
        Rect.Top := Rect.Top + CanvasMaxTextHeight(Canvas);
      end;
  end;

begin
  // (p3) remove warnings
  OldFontColor := 0;
  OldBrushColor := 0;
  OldBrushStyle := bsClear;
  RemFontSize := 0;
  RemFontColor := 0;
  RemBrushColor := 0;
  OldAlignment := taLeftJustify;
  OldFont := TFont.Create;

  if Canvas <> nil then
  begin
    if Lowercase(Canvas.Font.Name) = 'default' then
      Canvas.Font.Name := Screen.SystemFont.Name;
    if Canvas.Font.Size = 0 then
      Canvas.Font.Size := Screen.SystemFont.Size;
    OldFontStyles := Canvas.Font.Style;
    OldFontColor  := Canvas.Font.Color;
    OldBrushColor := Canvas.Brush.Color;
    OldBrushStyle := Canvas.Brush.Style;
  //  OldAlignment  := Alignment;
    RemFontColor  := Canvas.Font.Color;
    RemBrushColor := Canvas.Brush.Color;
    RemFontSize   := Canvas.Font.size;
  end;
  vStr  := TStringList.Create;
  try
    Alignment := taLeftJustify;
    IsLink := False;
    MouseOnLink := False;
    vStr.Text := HTMLPrepareText(Text);
    vStr.SkipLastLineBreak := true;
    LinkName := '';
    TempLink := '';
    ScriptPosition := spNormal;

    Selected := (odSelected in State) or (odDisabled in State);
    Trans := (Canvas.Brush.Style = bsClear) and not selected;

    Width := DefaultLeft;
    CurLeft := DefaultLeft;

    vM := '';
    for vCount := 0 to vStr.Count - 1 do
    begin
      vText := vStr[vCount];
//      vText := HTMLPrepareText(vStr[vCount]);
      CurLeft := CalcPos(vText);
      while vText <> '' do
      begin
        vM := HTMLBeforeTag(vText, True);
        vM := StringReplace(vM, '&lt;', cLT, [rfReplaceAll, rfIgnoreCase]); // <--+ this must be here
        vM := StringReplace(vM, '&gt;', cGT, [rfReplaceAll, rfIgnoreCase]); // <--/
        if GetChar(vText, 1) = cTagBegin then
        begin
          if vM <> '' then
            Draw(vM);
          if Pos(cTagEnd, vText) = 0 then
            Insert(cTagEnd, vText, 2);
          if GetChar(vText, 2) = '/' then
          begin
            case GetChar(vText, 3, True) of
              'A':
                begin
                  IsLink := False;
                  Canvas.Font.Assign(OldFont);
                end;
              'B':
                Style(fsBold, False);
              'I':
                Style(fsItalic, False);
              'U':
                Style(fsUnderline, False);
              'S':
                begin
                  ScriptPosition := spNormal;
                  Style(fsStrikeOut, False);
                end;
              'F':
                begin
                  if not Selected then // restore old colors
                  begin
                    Canvas.Font.Color := RemFontColor;
                    Canvas.Brush.Color := RemBrushColor;
                    Canvas.Font.Size := RemFontSize;
                    Trans := True;
                  end;
                end;
            end
          end
          else
          begin
            case GetChar(vText, 2, True) of
              'A':
                begin
                  if GetChar(vText, 3, True) = 'L' then // ALIGN
                  begin
                    TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText) - 2));
                    if Pos(cCENTER, TagPrp) > 0 then
                      Alignment := taCenter
                    else
                    if Pos(cRIGHT, TagPrp) > 0 then
                      Alignment := taRightJustify
                    else
                      Alignment := taLeftJustify;
                    CurLeft := DefaultLeft;
                    if CalcType in [htmlShow, htmlHyperLink] then
                      CurLeft := CalcPos(vText);
                  end
                  else
                  begin   // A HREF
                    TagPrp := Copy(vText, 2, Pos(cTagEnd, vText) - 2);
                    if Pos(cHREF, UpperCase(TagPrp)) > 0 then
                    begin
                      IsLink := True;
                      OldFont.Assign(Canvas.Font);
                      if not Selected then
                        Canvas.Font.Color := clBlue;
                      TempLink := ExtractPropertyValue(TagPrp, cHREF);
                    end;
                  end;
                end;
              'B':
                if GetChar(vText, 3) = cTagEnd then
                  Style(fsBold, True);   // BOLD
              'I':
                if GetChar(vText, 3, True) = 'N' then //IND="%d"
                begin
                  TagPrp := Copy(vText, 2, Pos(cTagEnd, vText) - 2);
                  CurLeft := StrToInt(ExtractPropertyValue(TagPrp, cIND)); // ex IND="10"
                  if odReserved1 in State then
                    CurLeft := Round((CurLeft * Scale) div 100);
                end
                else
                if GetChar(vText, 3) = cTagEnd then
                  Style(fsItalic, True); // ITALIC
              'U':
                if GetChar(vText, 3) = cTagEnd then
                  Style(fsUnderline, True);   // UNDERLINE
              'S':
                begin
                  if GetChar(vText, 4, True) = 'P' then      // SUP
                  begin
                    ScriptPosition := spSuperscript;
                  end
                  else if GetChar(vText, 4, True) = 'B' then   // SUB
                  begin
                    ScriptPosition := spSubscript;
                  end
                  else if GetChar(vText, 3) = cTagEnd then
                  begin
                    ScriptPosition := spNormal;
                    Style(fsStrikeOut, True);
                  end;
                end;
              'H':
                if (GetChar(vText, 3, True) = 'R') and Assigned(Canvas) then // HR
                begin
                  if odDisabled in State then // only when disabled
                    Canvas.Pen.Color := Canvas.Font.Color;
                  OldWidth := Canvas.Pen.Width;
                  TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText)-2));
                  Canvas.Pen.Width := StrToIntDef(ExtractPropertyValue(TagPrp, 'SIZE'), 1); // ex HR="10"
                  if odReserved1 in State then
                    Canvas.Pen.Width := Round((Canvas.Pen.Width * Scale) div 100);
                  if CalcType = htmlShow then
                  begin
                    Canvas.MoveTo(Rect.Left, Rect.Top + CanvasMaxTextHeight(Canvas));
                    Canvas.LineTo(Rect.Right, Rect.Top + CanvasMaxTextHeight(Canvas));
                  end;
                  Rect.Top := Rect.Top + 1 + Canvas.Pen.Width;
                  Canvas.Pen.Width := OldWidth;
                  NewLine(HTMLDeleteTag(vText) <> '');
                end;
              'F':
                if (Pos(cTagEnd, vText) > 0) and (not Selected) and Assigned(Canvas) {and (CalcType in [htmlShow, htmlHyperLink])} then // F from FONT
                begin
                  TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText) - 2));
                  RemFontColor := Canvas.Font.Color;
                  RemBrushColor := Canvas.Brush.Color;

                  if Pos(cCOLOR, TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, cCOLOR);
                    Canvas.Font.Color := HTMLStringToColor(Prp);
                  end;
                  if Pos(cBGCOLOR, TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, cBGCOLOR);
                    if UpperCase(Prp) = 'CLNONE' then
                      Trans := True
                    else
                    begin
                      Canvas.Brush.Color := HTMLStringToColor(Prp);
                      Trans := False;
                    end;
                  end;
                  if Pos('SIZE', TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, 'SIZE');
                    Canvas.Font.Size := StrToIntDef(Prp,2){ * Canvas.Font.Size div 2};
                  end;
                end;
            end;
          end;
          vText := HTMLDeleteTag(vText);
          vM := '';
        end;
      end;
      if vM <> '' then
        Draw(vM);
      NewLine;
      vM := '';
    end;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := OldFontStyles;
      Canvas.Font.Color := OldFontColor;
      Canvas.Brush.Color := OldBrushColor;
      Canvas.Brush.Style := OldBrushStyle;
      Alignment := OldAlignment;
  {    Canvas.Font.Color := RemFontColor;
      Canvas.Brush.Color:= RemBrushColor;}
    end;
    FreeAndNil(vStr);
    FreeAndNil(OldFont);
  end;
  Width := Max(Width, CurLeft - DefaultLeft);
  Height := Rect.Top + CanvasMaxTextHeight(Canvas);
end;

// wp: I made this a procedure - it was a function in the original with the
// result being unassigned.
procedure HTMLDrawText(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer);
var
  W: Integer;
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, W, htmlShow, 0, 0, S, St, SuperSubScriptRatio, Scale);
end;

// wp: I made this a procedure - it was a function in the original with the
// result being unassigned.
procedure HTMLDrawTextHL(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; MouseX, MouseY: Integer;
  SuperSubScriptRatio: Double; Scale: Integer);
var
  W: Integer;
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, W, htmlShow, MouseX, MouseY, S, St, SuperSubScriptRatio, Scale);
end;

function HTMLPlainText(const Text: string): string;
var
  S: string;
begin
  Result := '';
  S := HTMLPrepareText(Text);
  while Pos(cTagBegin, S) > 0 do
  begin
    Result := Result + Copy(S, 1, Pos(cTagBegin, S)-1);
    if Pos(cTagEnd, S) > 0 then
      Delete(S, 1, Pos(cTagEnd, S))
    else
      Delete(S, 1, Pos(cTagBegin, S));
  end;
  Result := Result + S;
end;

function HTMLTextExtent(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): TSize;
var
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx2(Canvas, Rect, State, Text, Result.cx, Result.cy, htmlCalcWidth, 0, 0, S, St, SuperSubScriptRatio, Scale);
  if Result.cy = 0 then
    Result.cy := CanvasMaxTextHeight(Canvas);
  Inc(Result.cy);
end;

function HTMLTextWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
var
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, Result, htmlCalcWidth, 0, 0, S, St, SuperSubScriptRatio, Scale);
end;

function HTMLTextHeight(Canvas: TCanvas; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
var
  S: Boolean;
  St: string;
  R: TRect;
begin
  R := Rect(0, 0, 0, 0);
  HTMLDrawTextEx(Canvas, R, [], Text, Result, htmlCalcHeight, 0, 0, S, St, SuperSubScriptRatio, Scale);
  if Result = 0 then
    Result := CanvasMaxTextHeight(Canvas);
  Inc(Result);
end;

end.
end.

