unit ConfigFrame_with_Addons;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  mvDrawingEngine, mvDE_RGBGraphics, mvDE_BGRA, //mvDE_LCL,
  mvDLEFPC, mvDLEWin, mvDLESynapse, mvDLECache,
  ConfigFrame;

type

  { TCfgFrame_with_addons }

  TCfgFrame_with_addons = class(TCfgFrame)
    cbDownloadEngine: TComboBox;
    cbDrawingEngine: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure cbDownloadEngineChange(Sender: TObject);
    procedure cbDrawingEngineChange(Sender: TObject);
  end;

implementation

{$R *.lfm}

{ TCfgFrame_with_addons }

procedure TCfgFrame_with_addons.cbDrawingEngineChange(Sender: TObject);
begin
  if not MapView.UsesDefaultDrawingEngine then
    MapView.DrawingEngine.Free;
  case cbDrawingEngine.ItemIndex of
    0: MapView.DrawingEngine := nil;
    1: MapView.DrawingEngine := TMvRGBGraphicsDrawingEngine.Create(Self);
    2: MapView.DrawingEngine := TMvBGRADrawingEngine.Create(self);
//    3: MapView.DrawingEngine := TMvLCLDrawingEngine.Create(self);
  end;
  DoUpdateLayers;
end;

procedure TCfgFrame_with_addons.cbDownloadEngineChange(Sender: TObject);
begin
  if not MapView.UsesDefaultDownloadEngine then
    MapView.DownloadEngine.Free;
  case cbDownloadEngine.ItemIndex of
    0: MapView.DownloadEngine := nil;
    1: MapView.DownloadEngine := TMvDESynapse.Create(self);
    2: MapView.DownloadEngine := TMvDEFPC.Create(self);
    3: {$IFDEF MSWINDOWS}
       MapView.DownloadEngine := TMvDEWin.Create(self);
       {$ELSE}
       begin
         MapView.DownloadEngine := nil;
         ShowMessage('WinInet download engine can only be used in Windows.');
       end;
       {$ENDIF}
    4: MapView.DownloadEngine := TMvDECache.Create(self);
  end;
  UpdateDownloadEngineProxy;
end;

end.

