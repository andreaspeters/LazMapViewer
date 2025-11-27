{
  (C) 2014 ti_dic@hotmail.com

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDragObj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TDragObj = Class;

  TDragEvent = Procedure (Sender : TDragObj) of Object;

  { TDragObj }

  TDragObj = class
  private
    FMouseDown : boolean;
    FLnkObj: TObject;
    FDragsrc,FStartSrc : TObject;
    FOfsX: integer;
    FOfsY: integer;
    FInDrag : Boolean;
    FStartX,FStartY : integer;
    FMouseX,FMouseY : integer;
    FEndX,FEndY : integer;
    FOnDrag: TDragEvent;
    FOnEndDrag: TDragEvent;
    function GetEndPt: TPoint;
    function GetOfsPt: TPoint;
    function GetStartPt: TPoint;

    procedure SetDest(X,Y : Integer);
    procedure SetLnkObj(AValue: TObject);
    procedure SetOnDrag(AValue: TDragEvent);
    procedure SetOnEndDrag(AValue: TDragEvent);

    Procedure DoStartDrag(X,Y: Integer);
    Procedure DoDrag(X,Y: integer);
    Procedure DoEndDrag(X,Y: integer);
    Function HasMoved(X,Y: integer) : Boolean;

    public
      Procedure MouseDown(aDragSrc: TObject; X,Y: integer);
      Procedure MouseUp(X,Y: integer);
      Procedure MouseMove(X,Y: integer);
      Procedure AbortDrag;

      property OnDrag: TDragEvent read FOnDrag write SetOnDrag;
      property OnEndDrag: TDragEvent read FOnEndDrag write SetOnEndDrag;

      property OfsPt: TPoint read GetOfsPt;
      property OfsX: integer read FOfsX;
      property OfsY: integer read FOfsY;
      property StartPt: TPoint read GetStartPt;
      property StartX: integer read FStartX;
      property StartY: integer read FStartY;
      property MouseX: Integer read FMouseX;
      property MouseY: integer read FMouseY;
      property EndPt: TPoint read GetEndPt;
      property EndX: integer read FEndX;
      property EndY: integer read FEndY;
      Property LnkObj: TObject Read FLnkObj write SetLnkObj;
      property DragSrc: TObject Read FStartSrc;
      property InDrag: Boolean read FInDrag;
  end;


implementation

{ TDragObj }

procedure TDragObj.SetDest(X, Y: Integer);
begin
  FEndX := X;
  FEndY := Y;
  FOfsX := FEndX-FstartX;
  FOfsY := FEndY-FstartY;
end;

procedure TDragObj.SetLnkObj(AValue: TObject);
begin
  if FLnkObj=AValue then Exit;
  FreeAndNil(FLnkObj);
  FLnkObj := AValue;
end;

procedure TDragObj.SetOnDrag(AValue: TDragEvent);
begin
  if FOnDrag=AValue then Exit;
  FOnDrag := AValue;
end;

procedure TDragObj.SetOnEndDrag(AValue: TDragEvent);
begin
  if FOnEndDrag=AValue then Exit;
  FOnEndDrag := AValue;
end;

procedure TDragObj.DostartDrag(X, Y: Integer);
begin
  FInDrag := True;
  FStartSrc := FDragSrc;
  DoDrag(X,Y);
end;

procedure TDragObj.DoDrag(X, Y: integer);
begin
  if (X<>FEndX) or (Y<>FEndY) then
  begin
    SetDest(X,Y);
    if Assigned(FOnDrag) then
      FOnDrag(Self);
  end;
end;

procedure TDragObj.DoEndDrag(X, Y: integer);
begin
  DoDrag(X,Y);
  if Assigned(FOnEndDrag) then
    FOnEndDrag(self);
  FreeAndNil(FLnkObj);
  FStartSrc := nil;
  FInDrag := False;
end;

function TDragObj.GetEndPt: TPoint;
begin
  Result := Point(FEndX, FEndY);
end;

function TDragObj.GetOfsPt: TPoint;
begin
  Result := Point(FOfsX, FOfsY);
end;

function TDragObj.GetStartPt: TPoint;
begin
  Result := Point(FStartX, FStartY);
end;

function TDragObj.HasMoved(X, Y: integer): Boolean;
begin
  Result := (X <> FStartX) or (Y <> FStartY);
end;

procedure TDragObj.AbortDrag;
begin
  if FInDrag then
  Begin
    DoDrag(FStartX, FStartY);
    FInDrag := False;
    FMouseDown := False;
    FDragSrc := nil;
    FStartSrc := nil;
    FreeAndNil(FLnkObj);
  end;
end;

procedure TDragObj.MouseDown(aDragSrc : TObject;X, Y: integer);
begin
  if not(FMouseDown) then
  begin
    FDragSrc := aDragSrc;
    FMouseDown := True;
    FStartX := X;
    FStartY := Y;
    FEndX := X;
    FEndY := Y;
  end
  else
    AbortDrag;
end;


procedure TDragObj.MouseMove(X, Y: integer);
begin
  FMouseX := X;
  FMouseY := Y;
  if FMouseDown then
  begin
    if FInDrag then
      DoDrag(X,Y)
    else
    begin
      if HasMoved(X,Y) then
        DoStartDrag(X,Y);
    end;
  end;
end;


procedure TDragObj.MouseUp(X, Y: integer);
begin
  if FMouseDown then
  begin
    FMouseDown := False;
    if FInDrag then
      DoEndDrag(X,Y);
    FDragSrc := nil;
  end;
end;

end.

