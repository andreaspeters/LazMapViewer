{ Registration of classes (plugins)
  Code was adapted from TAChart.
}

unit mvClassRegistration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PStr = ^String;

  TMvClassRegistryItem = class
    FClass: TClass;
    FCaption: String;
    FCaptionPtr: PStr;
    constructor Create(AClass: TClass; const ACaption: String);
    constructor CreateRS(AClass: TClass; ACaptionPtr: PStr);
  end;

  TMvClassRegistry = class(TFPList)
  public
    destructor Destroy; override;
    procedure Clear;
    function GetCaption(AIndex: Integer): String;
    function GetClass(AIndex: Integer): TClass;
    function IndexOfClass(AClass: TClass): Integer;
    procedure SetCaption(AIndex: Integer; ACaption: String);
  end;

implementation

{ TMvClassRegistryItem }

constructor TMvClassRegistryItem.Create(AClass: TClass; const ACaption: String);
begin
  FClass := AClass;
  FCaption := ACaption;
end;

constructor TMvClassRegistryItem.CreateRS(AClass: TClass; ACaptionPtr: PStr);
begin
  FClass := AClass;
  FCaptionPtr := ACaptionPtr;
  if FCaptionPtr <> nil then
    FCaption := ACaptionPtr^;
end;


{ TMvClassRegistry }

destructor TMvClassRegistry.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMvClassRegistry.Clear;
var
  i: Integer;
begin
  for i:= Count-1 downto 0 do
    TObject(Items[i]).Free;
  inherited;
end;

function TMvClassRegistry.GetCaption(AIndex: Integer): String;
var
  item: TMvClassRegistryItem;
begin
  item := TMvClassRegistryItem(Items[AIndex]);
  if item.FCaptionPtr <> nil then
    Result := item.FCaptionPtr^
  else
    Result := item.FCaption;
end;

function TMvClassRegistry.GetClass(AIndex: Integer): TClass;
begin
  Result := TMvClassRegistryItem(Items[AIndex]).FClass;
end;

function TMvClassRegistry.IndexOfClass(AClass: TClass): Integer;
begin
  for Result := 0 to Count-1 do
    if TMvClassRegistryItem(Items[Result]).FClass = AClass then
      exit;
  Result := -1;
end;

procedure TMvClassRegistry.SetCaption(AIndex: Integer; ACaption: String);
begin
  TMvClassRegistryItem(Items[AIndex]).FCaption := ACaption;
end;

end.

